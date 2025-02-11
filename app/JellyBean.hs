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
jsTrue = Parser $ \text -> do 
    (_, rest) <- parse (string "true") text 
    pure (JsBool True, rest)

jsFalse :: Parser JSON
jsFalse = Parser $ \text -> do 
    (_, rest) <- parse (string "false") text 
    pure (JsBool False, rest)

jsNull :: Parser JSON
jsNull = Parser $ \text -> do 
    (_, rest) <- parse (string "null") text 
    pure (JsNull, rest)

