{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
module JSON where

import Control.Applicative ( (<|>) )
import Control.Monad (void, when)
import Control.Exception (evaluate, throw, try)
import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, endOfInput
                                       , isDigit_w8, signed, string)

import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString.Lazy (parse, eitherResult)
import Data.Aeson (Value(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Text (Text)
import Data.Text.Internal.Private (runText)
import Data.Text.Encoding.Error (UnicodeException)
import Data.Text.Unsafe (unsafeDupablePerformIO)
import qualified Data.Text.Array as TA
import Data.Scientific (Scientific)
import qualified Data.HashMap.Strict as H
import qualified Data.Scientific as Sci

import Data.Vector as Vector (Vector, empty, fromListN, reverse)
import Utils (unescapeText)


#define BACKSLASH 92
#define CLOSE_CURLY 125
#define CLOSE_SQUARE 93
#define COMMA 44
#define DOUBLE_QUOTE 34
#define OPEN_CURLY 123
#define OPEN_SQUARE 91
#define COLON 58
#define C_0 48
#define C_9 57
#define C_A 65
#define C_F 70
#define C_a 97
#define C_f 102
#define C_n 110
#define C_t 116

skipSpace :: Parser ()
skipSpace = A.skipWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09

object_ :: Parser Value
object_ = Object <$> objectValues jlabel json

objectValues :: Parser Text -> Parser Value -> Parser (H.HashMap Text Value)
objectValues str val = do
    skipSpace
    w <- A.peekWord8'
    if w == CLOSE_CURLY
      then A.anyWord8 >> return H.empty
      else loop []
  where
    loop acc = do
      k <- str <* skipSpace <* char ':'
      v <- val <* skipSpace
      ch <- A.satisfy $ \w -> w == COMMA ||  w == CLOSE_CURLY
      let acc' = (k, v) : acc
      if ch == COMMA
        then skipSpace >> loop acc'
        else return (H.fromList acc')
{-# INLINE objectValues #-}

jlabel :: Parser Text
jlabel = do
  w <- A.peekWord8'
  if w == DOUBLE_QUOTE
    then A.word8 DOUBLE_QUOTE *> jstring_
    else do
      s <- A.takeTill $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09 || w == COLON
      case unescapeText s of
        Right r -> return r
        Left err -> fail $ show err

jstring_ :: Parser Text
jstring_ = do
    s <- A.scan startState go <* A.anyWord8
    case unescapeText s of
      Right r -> return r
      Left err -> fail $ show err
  where
    startState = False
    go a c
      | a = Just False
      | c == DOUBLE_QUOTE = Nothing
      | otherwise = let a' = c == backslash
                    in Just a'
      where backslash = BACKSLASH


array_ :: Parser Value
array_ = Array <$> arrayValues json

arrayValues :: Parser Value -> Parser (Vector Value)
arrayValues val = do
    skipSpace
    w <- A.peekWord8'
    if w == CLOSE_SQUARE
      then A.anyWord8 >> return Vector.empty
      else loop [] 1
--    undefined
  where
    loop acc !len = do
      v <- val <* skipSpace
      ch <- A.satisfy $ \w -> w == COMMA || w == CLOSE_SQUARE
      if ch == COMMA
        then skipSpace >> loop (v:acc) (len+1)
        else return (Vector.reverse (Vector.fromListN len (v:acc)))

-- | Parse a JSON number.
scientific :: Parser Scientific
scientific = do
  let minus = 45
      plus  = 43
  sign <- A.peekWord8'
  let !positive = sign == plus || sign /= minus
  when (sign == plus || sign == minus) $
    void A.anyWord8

  n <- decimal0

  let f fracDigits = SP (B.foldl' step n fracDigits)
                        (negate $ B.length fracDigits)
      step a w = a * 10 + fromIntegral (w - 48)

  dotty <- A.peekWord8
  -- '.' -> ascii 46
  SP c e <- case dotty of
              Just 46 -> A.anyWord8 *> (f <$> A.takeWhile1 isDigit_w8)
              _       -> pure (SP n 0)

  let !signedCoeff | positive  =  c
                   | otherwise = -c

  let littleE = 101
      bigE    = 69
  (A.satisfy (\ex -> ex == littleE || ex == bigE) *>
      fmap (Sci.scientific signedCoeff . (e +)) (signed decimal)) <|>
    return (Sci.scientific signedCoeff    e)

data SP = SP !Integer {-# UNPACK #-}!Int

decimal0 :: Parser Integer
decimal0 = do
  let zero = 48
  digits <- A.takeWhile1 isDigit_w8
  if B.length digits > 1 && B.unsafeHead digits == zero
    then fail "leading zero"
    else return (bsToInteger digits)

bsToInteger :: B.ByteString -> Integer
bsToInteger bs
    | l > 40    = valInteger 10 l [ fromIntegral (w - 48) | w <- B.unpack bs ]
    | otherwise = bsToIntegerSimple bs
  where
    l = B.length bs

bsToIntegerSimple :: B.ByteString -> Integer
bsToIntegerSimple = B.foldl' step 0 where
  step a b = a * 10 + fromIntegral (b - 48) -- 48 = '0'

valInteger :: Integer -> Int -> [Integer] -> Integer
valInteger = go
  where
    go :: Integer -> Int -> [Integer] -> Integer
    go _ _ []  = 0
    go _ _ [d] = d
    go b l ds
        | l > 40 = b' `seq` go b' l' (combine b ds')
        | otherwise = valSimple b ds
      where
        -- ensure that we have an even number of digits
        -- before we call combine:
        ds' = if even l then ds else 0 : ds
        b' = b * b
        l' = (l + 1) `quot` 2

    combine b (d1 : d2 : ds) = d `seq` (d : combine b ds)
      where
        d = d1 * b + d2
    combine _ []  = []
    combine _ [_] = errorWithoutStackTrace "this should not happen"

valSimple :: Integer -> [Integer] -> Integer
valSimple base = go 0
  where
    go r [] = r
    go r (d : ds) = r' `seq` go r' ds
      where
        r' = r * base + fromIntegral d

json :: Parser Value
json = do
  skipSpace
  w <- A.peekWord8'
  case w of
    DOUBLE_QUOTE  -> A.anyWord8 *> (String <$> jstring_)
    OPEN_CURLY    -> A.anyWord8 *> object_
    OPEN_SQUARE   -> A.anyWord8 *> array_
    C_f           -> string "false" *> pure (Bool False)
    C_t           -> string "true" *> pure (Bool True)
    C_n           -> string "null" *> pure Null
    _              | w >= 48 && w <= 57 || w == 45
                  -> Number <$> scientific
      | otherwise -> fail "not a valid json value"


{--
  parse (json <* skipSpace <* endOfInput) $ "{a :\"b\"}"

  parse (json <* skipSpace <* endOfInput) $ "{\"a\": \"b\"} "

--}
