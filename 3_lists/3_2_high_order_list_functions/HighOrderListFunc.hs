module HighOrderListFunc where

-- ФУНКЦИИ ВЫСШИХ ПОРЯДКОВ в ФП - это такие
-- функции, которые принимают в качестве аргументов
-- другие функции.

-- Функции высших порядков для работы со списками
-- в стандартной библиотеке:
-- 1. Принимают унарный предикат
-- 1.1) filter - фильтрация списка по условию:
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x : xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

-- 1.2) takeWhile - аналог filter, но останавливается
-- при первом False:
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x : xs)
  | p x = x : takeWhile' p xs
  | otherwise = []

-- 1.3) dropWhile - takeWhile наоборот,
-- т.е. убирает элементы до тех пор, пока True:
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p xs@(x : xs') -- @ - локальный псевдомним: xs - это (x : xs')
  | p x = dropWhile' p xs'
  | otherwise = xs -- без @ было бы (x : xs')

-- 1.4) span - разбивает список на две части в том месте списка,
-- где впервые значение предиката равно False.
-- Т.е. первая часть - takeWhile, вторая - dropWhile:
span' :: (a -> Bool)  -> [a] -> ([a], [a])
span' p xs = (takeWhile' p xs, dropWhile' p xs)

-- Через нее можно элегантнее решить задание из предыдущего модуля:
-- ЗАДАНИЕ:
-- Напишите функцию groupElems которая группирует
-- одинаковые элементы в списке (если они идут подряд)
-- и возвращает список таких групп.
-- GHCi> groupElems []
-- []
-- GHCi> groupElems [1,2]
-- [[1],[2]]
-- GHCi> groupElems [1,2,2,2,4]
-- [[1],[2,2,2],[4]]
-- GHCi> groupElems [1,2,3,2,4]
-- [[1],[2],[3],[2],[4]]

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems lst@(x : xs) = equals : groupElems others
  where
    (equals, others) = span (== x) lst

-- 1.5) break - span наоборот, т.е. разбивает список на две части
-- в том месте списка, где впервые значение предиката равно True:
break' :: (a -> Bool)  -> [a] -> ([a], [a])
break' p = span' (not . p)
