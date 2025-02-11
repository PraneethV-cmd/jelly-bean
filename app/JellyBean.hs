{-# LANGUAGE LambdaCase #-}

module JellyBean(
    parse,
    char,
    string,
    jsTrue,
    JSON(..)
) where  

newtype Parser a = Parser {
    parse :: String -> Maybe (a, String) 
}

char :: Char -> Parser Char 
char c = Parser $ \case 
    (c' : rest) -> if c == c' then Just (c, rest) else Nothing
    [] -> Nothing

string :: String -> Parser String 
string [] = Parser $ \text -> Just ("", text)
string (c:cs) = Parser $ \text -> do 
    (_, rest) <- parse (char c) text
    (_, rest') <- parse (string cs) rest
    pure (c:cs, rest')

data JSON
    = JsTrue
    deriving (Show, Eq)

jsTrue = Parser $ \text -> do 
    (_, rest) <- parse (string "true") text 
    pure (JsTrue, rest)
