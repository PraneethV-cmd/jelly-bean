{-# LANGUAGE DeriveGeneric, TupleSections, LambdaCase #-}
module Main where 

import Control.Applicative (Alternative(..), optional)
import Control.Monad (replicateM)
import Data.Bits (shiftL)
import Data.Char (isDigit, isHexDigit, isSpace, chr, ord, digitToInt)
import Data.Functor (($>))
import Data.List (intercalate)
import GHC.Generics (Generic)
import Numeric (showHex)
import Test.QuickCheck hiding (Positive, Negative)

data JValue = JNull
	     | JBool Bool
	     | JString String
	     | JNumber {int :: Integer, frac :: [Int], exponent :: Integer}
	     | JArray [JValue]
	     | JObject [(String,JValue)]
	     deriving (Eq, Generic)

instance Show JValue where 
	show value = case value of 
		JNull -> "null"
		JBool True -> "true"
		JBool False -> "false"
		JString s -> showJSONString s 
		JNumber s [] 0 -> show s 
		JNumber s f 0 -> show s ++ "." ++ concatMap show f 
		JNumber s [] e -> show s ++ "e" ++ show e 
		JNumber s f e -> show s ++ "." ++ concatMap show f ++ "e" ++ show e 
		JArray a -> "[" ++ intercalate ", " (map show a) ++ "]"
		JObject o -> "{" ++ intercalate ", " (map showKV o) ++ "}"
		where 
			showKV (k, v) = showJSONString k ++ ": " ++ show v

showJSONString :: String -> String 
showJSONString s = "\"" ++ concatMap showJSONChar s ++ "\""

isControl :: Char -> Bool 
isControl c = c `elem` ['\0' .. '\31']

showJSONChar :: Char -> String 
showJSONChar c = case c of 
	'\'' -> "'"
	'\"' -> "\\\""
	'\\' -> "\\\\"
	'/' -> "\\/"
	'\b' -> "\\b"
	'\f' -> "\\f"
	'\n' -> "\\n"
	'\r' -> "\\r"
	'\t' -> "\\t"
	_ | isControl c -> "\\u" ++ showJSONNonASCIIChar c 
	_ -> [c] 
	where 
		showJSONNonASCIIChar c = 
			let a = "0000" ++ showHex (ord c) "" in drop (length a - 4) a 

newtype Parser i o =
	Parser { runParser :: i -> Maybe (i, o) } 

char1 :: Char -> Parser String Char 
char1 c = Parser $ \case 
	(x:xs) | x == c -> Just (xs, x)
	_		-> Nothing 


-- now we make json generator using the QuickCheck Arbitrary typeclass where we 
-- use that to make or generate for random values
-- but we cannot use generators for JSON numbers, JSON strings and other cases
-- so for these we will haev to like use functions directly 
-- we do this by implementing the typeclass for JValue and define seperate generator fucntions 

-- SCALAR GENERATORS	

jNullGen :: Gen JValue 
jNullGen = pure JNull

jBoolGen :: Gen JValue 
jBoolGen = JBool <$> arbitrary 

jNumberGen :: Gen JValue 
jNumberGen = JNumber <$> arbitrary <*> listOf (choose (0, 9)) <*> arbitrary 

jsonStringGen :: Gen String
jsonStringGen =
	concat <$> listOf (oneof [ vectorOf 1 arbitraryUnicodeChar
                           , escapedUnicodeChar ])
	where
		escapedUnicodeChar = ("\\u" ++) <$> vectorOf 4 (elements hexDigitLetters)
		hexDigitLetters    = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']

jStringGen :: Gen JValue
jStringGen = JString <$> jsonStringGen

-- COMPOSITE GENERATORS

jArrayGen :: Int -> Gen JValue 
jArrayGen = fmap JArray . scale (`div` 2) . listOf . jValueGen . (`div` 2) 

-- div 2 is used to reduce the size of the generated values whichj can become super large and long to generate 
 
jObjectGen :: Int -> Gen JValue
jObjectGen = fmap JObject . scale(`div` 2) . listOf . objKV . (`div` 2)
	where 
		objKV n = (,) <$> jsonStringGen <*> jValueGen n

jValueGen :: Int -> Gen JValue
jValueGen n = if n < 5
	then frequency [(4, oneof scalarGens), (1, oneof (compositeGens n))]
	else frequency [(1, oneof scalarGens), (4, oneof (compositeGens n))]
	where
		scalarGens      = [jNullGen , jBoolGen , jNumberGen , jStringGen]
		compositeGens n = [jArrayGen n, jObjectGen n]

-- WHITE SPACE GENERATORS
 
jsonWhitespaceGen :: Gen String
jsonWhitespaceGen =	
	scale (round . sqrt . fromIntegral)
	. listOf
	. elements
	$ [' ', '\n', '\r', '\t']

stringify :: JValue -> Gen String 
stringify = pad . go 
	where 
		surround l r j = l ++ r ++ j 
		pad gen = surround <$> jsonWhitespaceGen <*> jsonWhitespaceGen <*> gen 
		commaSeperated = pad . pure . intercalate ","

		go value = case value of 
			JArray elements -> 
				mapM (pad . stringify) elements 
				>>= fmap (surround "[" "]"). commaSeperated
			JObject kvs ->
				mapM stringifyKV kvs >>= fmap (surround "{" "}"). commaSeperated
			_ -> return $ show value 
		stringifyKV (k, v) =
			surround <$> pad (pure $ showJSONString k) <*> stringify v <*> pure ":"

instance Arbitrary JValue where 
	arbitrary = sized jValueGen
	shrink    = genericShrink

-- PARSERS 
 
 newtype Parser i o =
	Parser { runParser :: i -> Maybe (i,  o) }

main :: IO () 
main = do 
	putStrLn "parser"
