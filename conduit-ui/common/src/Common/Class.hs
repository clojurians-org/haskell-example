{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Class where

import Common.Types
-- import Common.ExampleData
import Prelude
import Debug.Trace
import Data.String (IsString)

import GHC.Generics (Generic)
import Data.Function ((&))
import qualified Data.Text as T
import qualified Data.Tree as TR
import qualified Data.HashMap.Lazy as M
import Data.Default (Default, def)
import Data.List (foldl')
import Data.String.Conversions (cs)
import Data.Maybe (fromJust)

import Data.Semigroup (sconcat)
import Labels (lens)
import Control.Lens (view, (^.), (^..), (.~), at, _Just, each, to)
import qualified Data.List.NonEmpty as NE

data HaskellCodeBuilder = HaskellCodeBuilder {
    hcbCombinator :: TR.Tree T.Text
  , hcbFns :: M.HashMap T.Text T.Text
  , hcbSelfFn :: (T.Text, T.Text)
  } deriving (Generic, Show)

instance Default HaskellCodeBuilder

class ToHaskellCodeBuilder a where
  toHaskellCodeBuilder :: FaaSCenter -> a -> HaskellCodeBuilder

toHaskellCode :: HaskellCodeBuilder -> T.Text
toHaskellCode (HaskellCodeBuilder combinators fns selfFn) =
  "do\n  let\n"
       <> (T.unlines . map ("    " <> ) . T.lines .  T.unlines) (map mkFn (M.toList fns))
       <> "  " <> combinatorsCode
  where
    mkFn (name, body) = name <> "=do\n" <> body    
    combinatorsCode = "void $ " <>
      TR.foldTree (\x xs -> case length xs of
                             0 -> x
                             1 | elem x [">>", "<|>", "<>"] -> head xs
                             1 -> x <> "(" <> head xs <> ")"
                             _ -> "(" <> T.intercalate x xs <> ")")
        combinators

{--
exampleHaskellCodeBuilder :: HaskellCodeBuilder
exampleHaskellCodeBuilder = def
  { hcbCombinator = TR.Node ">>" [ TR.Node "f" []
                                  , TR.Node "<|>"
                                      [ TR.Node "g" []
                                      , TR.Node "h" []]]
  , hcbFns = M.fromList
             [ ("f", "  putStrLn \"hello world\"\n  putStrLn \"???\"")
             , ("g", "  putStrLn \"what's wrong\"")
             , ("h", "  putStrLn \"hi hi hi\"") ] }
--}

instance ToHaskellCodeBuilder EventPulse where
  toHaskellCodeBuilder faas ep =  do
    let dcivs@(x:xs) = (epDataCircuitValues ep)
        tupleFnS 1 = "id"
        tupleFnS n = T.replicate (n - 1)  ","
        hcbCombinator' = do
          foldl' (\b a -> TR.Node "<*>" [ b, (hcbCombinator . toHaskellCodeBuilder faas) a ])
            (TR.Node "<$>" [ TR.Node (tupleFnS (length dcivs)) []
                           , TR.Node "async" [(hcbCombinator . toHaskellCodeBuilder faas) x]])
            xs
        hcbFns' = (M.unions . fmap (hcbFns . toHaskellCodeBuilder faas)) dcivs
    def {hcbCombinator = hcbCombinator', hcbFns = hcbFns'}

instance ToHaskellCodeBuilder DataCircuitValue where
  toHaskellCodeBuilder faas dciv = do
    let dci = faas ^. lens #dataNetwork
                    . lens #dataCircuits
                    . at (fst . dcivLinkedDataCircuit $ dciv)
                    & fromJust
        dcivHCB = toHaskellCodeBuilder faas dci
        ldsaHCB = toHaskellCodeBuilder faas (dcivLinkedDataSandbox dciv)
    def { hcbCombinator = hcbCombinator dcivHCB
        , hcbFns = hcbFns dcivHCB <> hcbFns ldsaHCB}

instance ToHaskellCodeBuilder DataCircuit where
  toHaskellCodeBuilder faasCenter dci = toHaskellCodeBuilder faasCenter (dciPartCombinator dci)

instance ToHaskellCodeBuilder (TR.Tree DataCircuitPart) where
  toHaskellCodeBuilder faas (TR.Node DCIP_RootBindNode xs) = def
    { hcbCombinator = TR.Node ">>" (map (hcbCombinator . toHaskellCodeBuilder faas) xs)
    , hcbFns = M.unions $ map (hcbFns . toHaskellCodeBuilder faas) xs
      }
  toHaskellCodeBuilder faas (TR.Node DCIP_RootAlternativeNode xs) = def
    { hcbCombinator = TR.Node "<|>" (map (hcbCombinator . toHaskellCodeBuilder faas) xs)
    , hcbFns = M.unions $ map (hcbFns . toHaskellCodeBuilder faas) xs
      }
  toHaskellCodeBuilder faas (TR.Node (DCIP_BindNode _) xs) = def
    { hcbCombinator = TR.Node ">>" (map (hcbCombinator . toHaskellCodeBuilder faas) xs)
    , hcbFns = M.unions $ map (hcbFns . toHaskellCodeBuilder faas) xs
      }
  toHaskellCodeBuilder faas (TR.Node (DCIP_AlternateNode _) xs) = def
    { hcbCombinator = TR.Node "<|>" (map (hcbCombinator . toHaskellCodeBuilder faas) xs)
    , hcbFns = M.unions $ map (hcbFns . toHaskellCodeBuilder faas) xs
      }
  toHaskellCodeBuilder faas (TR.Node (DCIP_LinkedDataCircuit ldci) []) = do
    let dci = faas ^. lens #dataNetwork
                    . lens #dataCircuits
                    . at (fst ldci)
                    & fromJust
    toHaskellCodeBuilder faas dci
  toHaskellCodeBuilder faas (TR.Node (DCIP_LinkedDataConduit ldco) []) = do
    let dci = faas ^. lens #dataNetwork
                    . lens #dataConduits
                    . at (fst ldco)
                    & fromJust
    toHaskellCodeBuilder faas dci
  toHaskellCodeBuilder faas (TR.Node (DCIP_EmbededDataCircuit dci) []) = do
    toHaskellCodeBuilder faas dci
  toHaskellCodeBuilder faas (TR.Node (DCIP_EmbededDataConduit dco) []) = do
    toHaskellCodeBuilder faas dco
  toHaskellCodeBuilder faas _ = def

instance ToHaskellCodeBuilder DataConduit where
  toHaskellCodeBuilder faasCenter dco = toHaskellCodeBuilder faasCenter (dcoPartCombinator dco)

instance ToHaskellCodeBuilder (TR.Tree DataConduitPart) where
  toHaskellCodeBuilder faas (TR.Node DCOP_RootBindNode xs) = def
    { hcbCombinator = TR.Node ">>" (map (hcbCombinator . toHaskellCodeBuilder faas) xs)
    , hcbFns = M.unions $ map (hcbFns . toHaskellCodeBuilder faas) xs
      }
  toHaskellCodeBuilder faas (TR.Node DCOP_RootAlternativeNode xs) = def
    { hcbCombinator = TR.Node "<|>" (map (hcbCombinator . toHaskellCodeBuilder faas) xs)
    , hcbFns = M.unions $ map (hcbFns . toHaskellCodeBuilder faas) xs
      }
  toHaskellCodeBuilder faas (TR.Node (DCOP_BindNode _) xs) = def
    { hcbCombinator = TR.Node ">>" (map (hcbCombinator . toHaskellCodeBuilder faas) xs)
    , hcbFns = M.unions $ map (hcbFns . toHaskellCodeBuilder faas) xs
      }
  toHaskellCodeBuilder faas (TR.Node (DCOP_AlternateNode _) xs) = def
    { hcbCombinator = TR.Node "<|>" (map (hcbCombinator . toHaskellCodeBuilder faas) xs)
    , hcbFns = M.unions $ map (hcbFns . toHaskellCodeBuilder faas) xs
      }
  toHaskellCodeBuilder faas (TR.Node (DCOP_LinkedLogicFragment llf) []) = do
    let lf = faas ^. lens #dataNetwork
                   . lens #logicFragments
                   . at (fst llf)
                   & fromJust
    toHaskellCodeBuilder faas lf
  toHaskellCodeBuilder faas (TR.Node (DCOP_EmbededLogicFragment lf) []) = do
    toHaskellCodeBuilder faas lf
  toHaskellCodeBuilder faas (TR.Node (DCOP_LinkedPrimLogicFragment lplf) []) = do
    let lpf = faas ^. lens #dataNetwork
                   . lens #primLogicFragments
                   . at (fst lplf)
                   & fromJust
    toHaskellCodeBuilder faas lpf
  toHaskellCodeBuilder faas (TR.Node (DCOP_EmbededPrimLogicFragment plf) []) = do
    toHaskellCodeBuilder faas plf
  toHaskellCodeBuilder faas _ = def    

instance ToHaskellCodeBuilder LogicFragment where
  toHaskellCodeBuilder faasCenter lf = toHaskellCodeBuilder faasCenter (lfPartCombinator lf)  

instance ToHaskellCodeBuilder (TR.Tree LogicFragmentPart) where
  toHaskellCodeBuilder faas (TR.Node LFP_RootBindNode xs) = def
    { hcbCombinator = TR.Node ">>" (map (hcbCombinator . toHaskellCodeBuilder faas) xs)
    , hcbFns = M.unions $ map (hcbFns . toHaskellCodeBuilder faas) xs
      }
  toHaskellCodeBuilder faas (TR.Node LFP_RootAlternativeNode xs) = def
    { hcbCombinator = TR.Node "<|>" (map (hcbCombinator . toHaskellCodeBuilder faas) xs)
    , hcbFns = M.unions $ map (hcbFns . toHaskellCodeBuilder faas) xs
      }
  toHaskellCodeBuilder faas (TR.Node (LFP_BindNode _) xs) = def
    { hcbCombinator = TR.Node ">>" (map (hcbCombinator . toHaskellCodeBuilder faas) xs)
    , hcbFns = M.unions $ map (hcbFns . toHaskellCodeBuilder faas) xs
      }
  toHaskellCodeBuilder faas (TR.Node (LFP_AlternateNode _) xs) = def
    { hcbCombinator = TR.Node "<|>" (map (hcbCombinator . toHaskellCodeBuilder faas) xs)
    , hcbFns = M.unions $ map (hcbFns . toHaskellCodeBuilder faas) xs
      }
  toHaskellCodeBuilder faas (TR.Node (LFP_LinkedLogicFragment llf) []) = do
    let lf = faas ^. lens #dataNetwork
                   . lens #logicFragments
                   . at (fst llf)
                   & fromJust
    toHaskellCodeBuilder faas lf
  toHaskellCodeBuilder faas (TR.Node (LFP_EmbededLogicFragment lf) []) = do
    toHaskellCodeBuilder faas lf
  toHaskellCodeBuilder faas _ = def

instance ToHaskellCodeBuilder PrimLogicFragment where
  toHaskellCodeBuilder faasCenter plf = def
    { hcbCombinator = TR.Node (plfName plf) []
    , hcbFns = M.singleton (plfName plf) (snd . plfEffectEngineCode $ plf)
      }

instance ToHaskellCodeBuilder LinkedDataSandbox where
  toHaskellCodeBuilder faas (LinkedDataSandbox lscs ldsos ldses)  = do
    let scs = faas ^.. lens #dataSandbox
                     . lens #stateContainers
                     . (mconcat . fmap (\(x, _) -> at x . _Just) ) lscs
        dsos = faas ^.. lens #dataSandbox
                      . lens #dataSources
                      . (mconcat . fmap (\(x, _) -> at x . _Just) ) ldsos
        dses = faas ^.. lens #dataSandbox
                      . lens #dataServices
                      .  (mconcat . fmap (\(x, _) -> at x . _Just) ) ldses
        scsFns = map (hcbFns . toHaskellCodeBuilder faas) scs
        dsoFns = map (hcbFns . toHaskellCodeBuilder faas) dsos
        dseFns = map (hcbFns . toHaskellCodeBuilder faas) dses
        dasFns = M.singleton
                   "dataSandbox"
                   (T.unlines [ "  (  #stateContainer := undefined"
                              , "   , #dataSource := "  <> (getDataSourceName . head $ dsos)
                              , "   , #dataService := " <> (getDataServiceName . head $ dses)
                              , "     )"])
        
    def { hcbFns = M.unions $ scsFns <> dsoFns <> dseFns <> [dasFns]
         } 

instance ToHaskellCodeBuilder StateContainer where
  toHaskellCodeBuilder _ _ = def

instance ToHaskellCodeBuilder DataSource where
  toHaskellCodeBuilder faas (DSO_SQLCursor sqlCursor) = toHaskellCodeBuilder faas sqlCursor
  toHaskellCodeBuilder _ _ = def
  
instance ToHaskellCodeBuilder DataService where
  toHaskellCodeBuilder faas (DSE_FileService_MinIO minio) = toHaskellCodeBuilder faas minio    
  toHaskellCodeBuilder faas (DSE_FileService_SFTP sftp) = toHaskellCodeBuilder faas sftp
  toHaskellCodeBuilder _ _ = def


instance ToHaskellCodeBuilder DSEFSMinIO where
  toHaskellCodeBuilder _ minio = def
    { hcbFns = M.fromList [(dsefsMinIOName minio, (cs . unlines)
        [  "  let"
         , "    (ci', accessKey, secretKey, bucket, filepath)"
         , "      = ( \"http://10.132.37.200:9000\""
         , "        , \"XF90I4NV5E1ZC2ROPVVR\""
         , "        , \"6IxTLFeA2g+pPuXu2J8BMvEUgywN6kr5Uckdf1O4\""
         , "        , \"larluo\""
         , "        , \"postgresql.txt\")"
         , "    ci = ci' & M.setCreds (M.Credentials accessKey secretKey)"
         , "             & M.setRegion \"us-east-1\""
         , "  "
         , "  Right uid <- liftIO . M.runMinio ci $ do"
         , "    bExist <- M.bucketExists bucket"
         , "    when (not bExist) $ void $ M.makeBucket bucket Nothing"
         , "    let a = M.putObjectPart"
         , "    M.newMultipartUpload bucket filepath  []"
         , "  "
         , "  C.chunksOfE (2000 * 1000)"
         , "    .| mergeSource (C.yieldMany [1..])"
         , "    .| C.mapM  (\\(pn, v) -> liftIO . M.runMinio ci $ M.putObjectPart bucket filepath uid pn [] (M.PayloadBS v))"
         , "    .| (C.sinkList >>= yield . fromRight' . sequence)"
         , "    .| C.mapM (liftIO . M.runMinio ci . M.completeMultipartUpload bucket filepath uid)"
          ])]
      }
instance ToHaskellCodeBuilder DSOSQLCursor where
  toHaskellCodeBuilder _ dsoSQLCusor = def
    { hcbFns = M.fromList [(dsoSQLCursorName dsoSQLCusor, (cs . unlines)
        [ "  let"
        , "    sql = [str|select"
        , "                |  id, name, description, 'type'"
        , "                |, state, timeliness, params, result_plugin_type"
        , "                |, vendor_id, server_id, success_code"
        , "                |from tb_interface"
        , "                |] :: B.ByteString"
        , "  "
        , "    pgSettings = H.settings \"10.132.37.200\" 5432 \"monitor\" \"monitor\" \"monitor\""
        , "    (curName, cursorSize, chanSize) = (\"larluo\", 200, 1000)"
        , "  "
        , "    textColumn = HD.column HD.text"
        , "    mkRow = (,,,,,,,,,,)"
        , "                <$> fmap (#id :=) textColumn"
        , "                <*> fmap (#name :=) textColumn"
        , "                <*> fmap (#description :=) textColumn"
        , "                <*> fmap (#type :=) textColumn"
        , "                <*> fmap (#state :=) textColumn"
        , "                <*> fmap (#timeliness :=) textColumn"
        , "                <*> fmap (#params :=) textColumn"
        , "                <*> fmap (#result_plugin_type :=) textColumn"
        , "                <*> fmap (#vendor_id :=) textColumn"
        , "                <*> fmap (#server_id :=) textColumn"
        , "                <*> fmap (#success_code :=) textColumn"
        , "  "
        , "  chan <- do"
        , "    Right connection <- liftIO $ H.acquire pgSettings"
        , "    lift $ pgToChan connection sql curName cursorSize chanSize mkRow"
        , "  sourceTBMChan chan"
          ])]
      }

instance ToHaskellCodeBuilder DSEFSSFtp where
  toHaskellCodeBuilder _ dsefsSFtp = def
    { hcbFns = M.fromList [(dsefsSFtpName dsefsSFtp, (cs . unlines)
      [ "  let (hostname, port, username, password) = (\"10.132.37.201\", 22, \"op\", \"op\") "
      , "      filepath = \"larluo111.txt\" "
      , "      flags = [SSH.FXF_WRITE, SSH.FXF_CREAT, SSH.FXF_TRUNC, SSH.FXF_EXCL] "
      , "  bracketP (SSH.sessionInit hostname port) SSH.sessionClose $ \\s -> do "
      , "    liftIO $ SSH.usernamePasswordAuth s username password "
      , "    bracketP (SSH.sftpInit s) SSH.sftpShutdown $ \\sftp -> do "
      , "      bracketP (SSH.sftpOpenFile sftp filepath 0o777 flags) SSH.sftpCloseHandle $ \\sftph -> "
      , "        C.mapM (liftIO . SSH.sftpWriteFileFromBS sftph) "
        ])]
      }
