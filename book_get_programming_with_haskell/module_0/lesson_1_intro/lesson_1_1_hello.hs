-- ! в проекте должен быть модуль Main, иначе компилятор не будет компилировать файл
module Main where

-- ! аналог main() из C++ и __main__ в Python
main = do
  putStrLn "Hello, Haskell!"

mainArg x = do
  putStrLn ("Hello, " ++ x ++ ". And bye!")

-- * можно скомпилировать файл в исполняемый через ghc <filename> (обязательно наличие main и модуля main)
-- * а можно использовать из интерпретатора ghci: :load <filename> (название модуля может быть любым)