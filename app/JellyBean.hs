{-# LANGUAGE LambdaCase #-}

module JellyBean(
    parse,
    char,
    string,
    jsTrue,
    jsFalse,
    jsNull,
    jsValue,
    JSON(..)
) where  

newtype Parser a = Parser {
    parse :: String -> Maybe (a, String) 
}

instance Functor Parser where 
    fmap f (Parser p) = Parser $ \text ->
        (\(v, rest) -> (f v, rest)) <$> p text 

instance Applicative Parser where 
    pure v = Parser $ \text -> Just (v, text)
    pf <*> p = Parser $ \text -> do
        (f, rest) <- parse pf text 
        (v, rest') <- parse p text 
        pure $ (f v, rest')

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
    = JsBool Bool
    | JsNull
    deriving (Show, Eq)

jsValue :: Parser JSON 
jsValue = Parser $ \text -> 
    case parse jsTrue text of   
        Just (val, rest) -> Just (val, rest)
        Nothing -> 
            case parse jsFalse text of 
                Just (val, rest) -> Just (val, rest) 
                Nothing  -> 
                    case parse jsNull text of 
                        Just (val, rest) -> Just (val, rest) 
                        Nothing -> Nothing

jsTrue :: Parser JSON
jsTrue = const (JsBool True) <$> string "true"

jsFalse :: Parser JSON
jsFalse = const (JsBool False) <$> string "false"

jsNull :: Parser JSON
jsNull = const JsNull <$> string "null"
