module MyModule (hello, main) where

import Prelude

hello :: String
hello = "hello"

main :: IO ()
main = putStrLn hello
