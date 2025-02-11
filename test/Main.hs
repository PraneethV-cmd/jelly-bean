module Main where 

import JellyBean (parse, char, string, jsTrue, JSON(..))
import Test.HUnit

testCharA :: Test 
testCharA = TestCase $ do 
    let result = parse (char 'a') "apple"
    assertEqual "Parsing 'a' from 'apple' " (Just ('a', "pple")) result

testCharB :: Test 
testCharB = TestCase $ do 
    let result = parse (char 'a') "hello"
    assertEqual "Parsing 'a' from 'hello'" Nothing result

testStringA :: Test 
testStringA = TestCase $ do 
    let result = parse (string "apple") "applepie"
    assertEqual "Parsing 'apple' from 'applepie'" (Just("apple", "pie")) result

jsonTestA :: Test 
jsonTestA = TestCase $ do 
    let s = "sometext"
    let result = parse jsTrue ("true " ++ s) 
    assertEqual "Parsing or detecting 'true':" (Just(JsTrue," " ++ s)) result

tests :: Test 
tests = TestList [testCharA, testCharB, testStringA, jsonTestA]

main :: IO () 
main = do  
    counts <- runTestTT tests 
    if errors counts + failures counts == 0 
        then putStrLn "All tests passed"
        else putStrLn "Some  tests failed"
