module Main where 

import JellyBean (parse, char, string)

main :: IO () 
main = do 
    let result = parse (char 'a') "apple"
    print result
