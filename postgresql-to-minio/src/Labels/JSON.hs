{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Support for reading from/to JSON via the Aeson package.

module Labels.JSON where

import Prelude
import           Data.Aeson
import           Data.Proxy
import qualified Data.Text as T
import           GHC.TypeLits
import           Labels.Internal
import           Language.Haskell.TH

{--
instance {-# OVERLAPPING #-} (KnownSymbol l1, FromJSON v1, KnownSymbol l2, FromJSON v2)
  => FromJSON (l1 := v1, l2 := v2) where
  parseJSON (Object v) = do
    let p1 = Proxy :: Proxy l1
    let p2 = Proxy :: Proxy l2

    v1 <- v .: T.pack (symbolVal p1)
    v2 <- v .: T.pack (symbolVal p2)
    return (p1 := v1, p2 := v2)

instance  {-# OVERLAPPING #-} (KnownSymbol l1, ToJSON v1, KnownSymbol l2, ToJSON v2)
  => ToJSON (l1 := v1, l2 := v2) where
  toJSON (p1 := v1, p2 := v2)  = object [ T.pack (symbolVal p1) .= v1
                                        , T.pack (symbolVal p2) .= v2]
--}    
  
$(let makeInstance :: Int -> Q Dec
      makeInstance size =
          instanceWithOverlapD
              (Just Overlapping)
              context
              (appT (conT ''FromJSON) instHead)
              [ funD
                    'parseJSON
                    [clause [conP 'Object [varP j_var]]
                            (normalB [|do $(tuplize (map (getter j_var) [1::Int .. size])) |]) []]]
        where
          l_tyvar j = mkName ("l" ++ show j)
          v_tyvar j = mkName ("v" ++ show j)
          j_var = mkName "j"
--          context = 
          context =
              return
                  (concat
                       (map
                            (\i ->
                                  [AppT (ConT ''KnownSymbol) (VarT (l_tyvar i))
                                  ,AppT (ConT ''FromJSON) (VarT (v_tyvar i))])
                            [1 .. size]))
          instHead =
              foldl
                  appT
                  (tupleT size)
                  (map
                       (\j ->
                             appT
                                 (appT (conT ''(:=)) (varT (l_tyvar j)))
                                 (varT (v_tyvar j)))
                       [1 .. size])
          tuplize [] = fail "Need at least one field."
          tuplize [j] = j
          tuplize js = foldl (\acc (i,g) -> infixApp acc (varE (if i == 1
                                                                   then '(<$>)
                                                                   else '(<*>))) g)
                             tupSectionE
                             (zip [1::Int ..] js)
          tupSectionE  =
            lamE (map (varP.var) [1..size])
                 (tupE (map (varE.var) [1..size]))
            where var i = mkName ("t" ++ show i)
          getter o (j::Int) =
              [|let proxy = Proxy :: Proxy $(varT (l_tyvar j))
                in do v <- $(varE o) .: (T.pack (symbolVal proxy))
                      return (proxy := v)|]
  in mapM makeInstance [1..24])

$(let makeInstance :: Int -> Q Dec
      makeInstance size =
          instanceWithOverlapD
              (Just Overlapping)
              context
              (appT (conT ''ToJSON) instHead)
              [ funD
                    'toJSON
                    [clause [tupP (map (\i -> infixP (varP (p_tyvar i)) '(:=) (varP (v_tyvar i)))
                                        [1::Int .. size])]
                            (normalB (appE (varE 'object)
                                           (listE (map setter [1::Int .. size])) ))
                            [] ]]
        where
          l_tyvar j = mkName ("l" ++ show j)
          v_tyvar j = mkName ("v" ++ show j)
          p_tyvar j = mkName ("p" ++ show j)
          j_var = mkName "j"
          context =
              return
                  (concat
                       (map
                            (\i ->
                                  [AppT (ConT ''KnownSymbol) (VarT (l_tyvar i))
                                  ,AppT (ConT ''ToJSON) (VarT (v_tyvar i))])
                            [1 .. size]))
          instHead =
              foldl
                  appT
                  (tupleT size)
                  (map
                       (\j ->
                             appT
                                 (appT (conT ''(:=)) (varT (l_tyvar j)))
                                 (varT (v_tyvar j)))
                       [1 .. size])
          setter (j::Int) =
              [|T.pack (symbolVal $(varE (p_tyvar j))) .= $(varE (v_tyvar j)) |]
  in mapM makeInstance [1..24])

repl :: IO ()
repl = do
  print $ (decode "{\"a\":3, \"b\":{\"c\":4}}":: Maybe ("a" := Int, "b" := ("c" := Int)))
  print $ (encode (#a := 3, #b := (#c:=4)))
