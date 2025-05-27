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

char1 :: Char -> Parser String Char 
char1 c = Parser $ \case 
	(x:xs) | x == c -> Just (xs, x)
	_ -> Nothing

satisfy :: (a -> Bool) -> Parser [a] a 
satisfy predicate = Parser $ \case 
	(x:xs) | predicate x -> Just (xs, x)
	_ -> Nothing

char :: Char -> Parser String Char 
char c = satisfy (== c)

digit1 :: Parser String Int 
digit1 =  Parser $ \i -> case runParser (satisfy isDigit) i of 
	Nothing -> Nothing 
	Just(i', o) -> Just (i', digitToInt o)

digit2 :: Parser String Int 
digit2 = Parser $ \i -> case runParser (satisfy isDigit) i of 
	Nothing -> Nothing 
	Just (i', o) -> Just . fmap digitToInt $ (i', o) 

digit3 :: Parser String Int 
digit3 = Parser $ \i -> fmap (fmap digitToInt) . runParser (satisfy isDigit) $ i

instance Functor (Parser i) where 
	fmap f parser = Parser $ fmap (fmap f) . runParser parser 

digit :: Parser String Int 
digit = digitToInt <$> satisfy isDigit

string1 :: String -> Parser String String 
string1 s = case s of 
	"" -> Parser $ \i -> Just (i, "")
	(c:cs) -> Parser $ \i -> case runParser (char c) i of 
		Nothing -> Nothing 
		Just (rest, _) -> case runParser (string1 cs) rest of 
			Nothing -> Nothing 
			Just (rest', _) -> Just (rest', c:cs)

string2 :: String -> Parser String String 
string2 s = case s of 
	"" -> Parser $ pure . (, "")
	(c:cs) -> Parser $ \i -> case runParser (char c) i of 
		Nothing -> Nothing 
		Just(rest, c) -> fmap (c:) <$> runParser (string2 cs) rest 

instance Applicative (Parser i) where
	pure x = Parser $ pure . (, x)
	pf <*> po = Parser $ \input -> case runParser pf input of 
		Nothing -> Nothing 
		Just (rest, f) -> fmap f <$> runParser po rest 

string :: String -> Parser String String 
string "" = pure  "" 
string (c:cs) = (:) <$> char c <*> string cs 

jNull :: Parser String JValue 
jNull = string "null" $> JNull

instance Alternative (Parser i) where 
	empty = Parser $ const empty 
	p1 <|> p2 = Parser $ \input -> runParser p1 input <|> runParser p2 input 

jBool :: Parser String JValue 
jBool = string "true" $> JBool True 
	<|> string "false" $> JBool False 

jsonChar :: Parser String Char 
jsonChar = string "\\\"" $> '"'
	<|> string "\\\\" $> '\\'
	<|> string "\\/" $> '/'
	<|> string "\\b" $> '\b'
	<|> string "\\f" $> '\f'
	<|> string "\\n" $> '\n'
	<|> string "\\r" $> '\r'
	<|> string "\\t" $> '\t'
	<|> unicodeChar 
	<|> satisfy (\c -> not (c == '\"' || c == '\\' || isControl c))
	where 
		unicodeChar =
			chr . fromIntegral . digitsToNumber 16 0 
			<$> (string "\\u" *> replicateM 4 hexDigit)
		hexDigit = digitToInt <$> satisfy isHexDigit

digitsToNumber :: Int -> Integer -> [Int] -> Integer
digitsToNumber base =	
	foldl (\num d -> num * fromIntegral base + fromIntegral d)

instance Monad (Parser i) where 
	p >>= f = Parser $ \input -> case runParser p input of 
		Nothing  -> Nothing 
		Just (rest, o) -> runParser (f o) rest 

jString :: Parser String JValue
jString = JString <$> (char '"' *> jString')
	where 
		jString' = do 
			optFirst <-  optional jsonChar 
			case optFirst of 
				Nothing -> "" <$ char '"'
				Just first | not (isSurrogate first) -> (first:) <$> jString'
				Just first -> do 
					second <- jsonChar 
					if isHighSurrogate first && isLowSurrogate second 
					then (combineSurrogates first second :) <$> jString'
					else empty 


highSurrogateLowerBound, highSurrogateUpperBound :: Int
highSurrogateLowerBound = 0xD800
highSurrogateUpperBound = 0xDBFF

lowSurrogateLowerBound, lowSurrogateUpperBound :: Int
lowSurrogateLowerBound  = 0xDC00
lowSurrogateUpperBound  = 0xDFFF

isHighSurrogate, isLowSurrogate, isSurrogate :: Char -> Bool
isHighSurrogate a =
	ord a >= highSurrogateLowerBound && ord a <= highSurrogateUpperBound
isLowSurrogate a  =
	ord a >= lowSurrogateLowerBound && ord a <= lowSurrogateUpperBound
isSurrogate a = isHighSurrogate a || isLowSurrogate a

combineSurrogates :: Char -> Char -> Char
combineSurrogates a b = chr $
	((ord a - highSurrogateLowerBound) `shiftL` 10) +
	(ord b - lowSurrogateLowerBound) + 0x10000

prop_genParseJString :: Property
prop_genParseJString = 
	forAllShrink jStringGen shrink $ \js -> 
		case runParser jString (show js) of 
			Nothing -> False 
			Just (_, o) -> o == js

jUInt :: Parser String Integer 
jUInt = (\d ds -> digitsToNumber 10 0 (d:ds)) <$> digit19 <*> digits 
	<|> fromIntegral <$> digit

digit19 :: Parser String Int 
digit19 = digitToInt <$> satisfy (\x -> isDigit x && x /= '0')

digits :: Parser String [Int]
digits = some digit

jInt' :: Parser String Integer 
jInt' = signInt <$> optional (char '-') <*> jUInt 

signInt :: Maybe Char -> Integer -> Integer 
signInt (Just '-') i = negate i 
signInt _ i = i

jFrac :: Parser String [Int]
jFrac = char '.' *> digits 

jExp :: Parser String Integer 
jExp = (char 'e' <|> char 'E')
	*> (signInt <$> optional (char '+' <|> char '-') <*> jUInt)
	
jInt :: Parser String JValue 
jInt = JNumber <$> jInt' <*> pure [] <*> pure 0

jIntExp :: Parser String JValue 
jIntExp = JNumber <$> jInt' <*> pure [] <*> jExp

jIntFrac :: Parser String JValue 
jIntFrac = (\i f -> JNumber i f 0) <$> jInt' <*> jFrac 

jIntFracExp :: Parser String JValue 
jIntFracExp = (\ ~(JNumber i f _) e -> JNumber i f e) <$> jIntFrac <*> jExp 

jNumber :: Parser String JValue 
jNumber = jIntFracExp <|> jIntExp <|> jIntFrac <|> jInt

prop_genParseJNumber :: Property
prop_genParseJNumber = 
	forAllShrink jNumberGen shrink $ \jn -> 
		case runParser jNumber (show jn) of
			Nothing -> False 
			Just (_ , o) -> o == jn 

surroundedBy :: Parser String a -> Parser String b -> Parser String a 
surroundedBy p1 p2 = p2 *> p1 <* p2 

seperatedBy :: Parser i v -> Parser i s -> Parser i [v]
seperatedBy v s = (:) <$> v <*> many (s *> v)
		<|> pure []

spaces :: Parser String String 
spaces = many (char ' ' <|> char '\n' <|> char '\r' <|> char '\t')

jArray :: Parser String JValue 
jArray = JArray <$> 
	(char '[' 
	*> (jValue `seperatedBy` char ',' `surroundedBy` spaces) 
	<* char ']')

prop_genParseJArray :: Property
prop_genParseJArray =
	forAllShrink (sized jArrayGen) shrink $ \ja -> 
		forAll (fmap (dropWhile isSpace) (stringify ja)) $ \jas -> 
			counterexample (show jas) $ case runParser jArray jas of 
				Nothing -> False 
				Just(_, o) -> o == ja

jObject :: Parser String JValue 
jObject = JObject <$> 
	(char '{' *> pair `seperatedBy` char ',' `surroundedBy` spaces <* char '}')
	where 
		pair = (\ ~(JString s) j -> (s, j))
			<$> (jString `surroundedBy` spaces)
			<* char ':'
			<*> jValue

prop_genParseJObject :: Property 
prop_genParseJObject = 
	forAllShrink (sized jObjectGen) shrink $ \jo -> do 
		jos <- dropWhile isSpace <$> stringify jo 
		return . counterexample (show jos) $ case runParser jObject jos of
			Nothing -> False 
			Just (_, o) -> o == jo

jValue :: Parser String JValue 
jValue = jValue' `surroundedBy` spaces 
	where	 
		jValue' =	jNull
			<|>	jBool
			<|>	jString
			<|>	jNumber
			<|>	jArray
			<|>	jObject

parseJSON :: String -> Maybe JValue 
parseJSON s = case runParser jValue s of 
	Just ("", j) -> Just j 
	_ -> Nothing

prop_genParseJSON :: Property 
prop_genParseJSON = forAllShrink (sized jValueGen) shrink $ \value -> do 
	json <- stringify value 
	return . counterexample (show json) . (== Just value) . parseJSON $ json 

main :: IO () 
main = do 
	putStrLn "== prop_genParseJString =="
	quickCheck prop_genParseJString

	putStrLn "== prop_genParseJNumber =="
	quickCheck prop_genParseJNumber

	putStrLn "== prop_genParseJArray =="
	quickCheck prop_genParseJArray

	putStrLn "== prop_genParseJObject =="
	quickCheck prop_genParseJObject

	putStrLn "== prop_genParseJSON =="
	quickCheck prop_genParseJSON

