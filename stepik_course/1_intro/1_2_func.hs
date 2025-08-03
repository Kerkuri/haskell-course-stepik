module Func where

sumSquares x y = x^2 + y^2

-- sign x = if x > 0 then 1 else (if x == 0 then 0 else (-1))
-- Вот это предложил линтер вместо функции выше:
sign x
  | x > 0 = 1
  | x == 0 = 0
  | otherwise = - 1

gign x = sign x + 3

-- max5 x = max 5 x
-- Более короткий способ определить функцию max5:
max5' = max 5

-- discount thresh perc price = if price >= thresh then price * (100 - perc) / 100 else price
-- Порядок аргументов выбран такой, чтобы удобно было
-- использовать частичное определение функции ниже:
discount thresh perc price
  | price >= thresh = price * (100 - perc) / 100
  | otherwise = price

-- Определение функции через частичное применение функции:
standardDiscount = discount 1000 5


makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)

makeAddressReal :: Int -> String -> String -> (Int, String, String)
makeAddressReal = (
  \number ->
    (\street ->
      (\town ->
        (number, street, town))))