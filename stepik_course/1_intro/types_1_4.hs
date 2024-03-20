module Types where

-- Как импортировать модули:
-- Это должно идти в самом начале скрипта, после строки с "module"

import Data.Char 

test = isDigit '7'

-- Чтобы описать тип функции нужен оператор ->:
-- :t not 
-- not :: Bool -> Bool

-- (&&) - логическое И
-- Тип оператора &&:
-- Bool -> (Bool -> Bool)
-- Принимает аргумент типа Bool -> Возращает функцию,
-- которая принимает аргумент типа Bool и возвращает Bool

-- Если у функции N >= 2 аргументов, то будет редукция типов:
-- Принимается один аргумент -> Возращается функция N-1 аргументов,
-- которая принимает аргумент и возвращает функцию N-2 аргументов и т.д. 
-- Другими словами, количество аргументов у функции - это
-- кол-во стрелочек "->" в выражении.

-- Пример определения функции с типом
discount :: Double -> Double -> Double -> Double
discount thresh perc price
  | price >= thresh = price * (100 - perc) / 100
  | otherwise = price

standardDiscount :: Double -> Double
standardDiscount = discount 1000 5

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int tens ones
  | isDigit tens && isDigit ones = digitToInt tens * 10 + digitToInt ones
  | otherwise = 100

-- КОРТЕЖ - упорядоченный набор элементов произвольных типов фиксированной длины
a = (2, True, 'c')
b = (42, "Yes")
-- Тип кортежа - это кортеж типов его элементов:
-- :t a = (Integer, Bool, Char)
-- :t b = (Integer, String)

-- Вспомогательные функции для двухэлементных кортежей
-- взять первый элемент кортежа (fst = first)
firstElement = fst b
-- взять второй элемент кортежа (snd = second)
secondElement = snd b

dist :: (Double, Double) -> (Double, Double) -> Double
-- dist p1 p2 = sqrt $ (fst p1 - fst p2)^2 + (snd p1 - snd p2)^2
-- Альтернативный способ задать функцию:
dist p1 p2 = sqrt $ (+) (dx ^ 2) (dy ^ 2) where
  dx = fst p1 - fst p2
  dy = snd p1 - snd p2

-- СПИСОК - упорядоченный набор элементов одинакового типа нефиксированной длины
listInt = [1, 2, 3]
listBool = [True, False]
-- Тип списка - это [<тип элементов>]
-- т.к. список гомогенный в плане типов его элементов

-- Список типа Char - это String
-- ['H', 'i'] == "Hi"

-- Добавление элемента в голову списка - оператор ":" :
str = 'H' : "ello"

-- Конкатенация списков - оператор "++"
-- Можно складывать только списки одного типа
strFull = str ++ ", world!"