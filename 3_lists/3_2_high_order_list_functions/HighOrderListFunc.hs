module HighOrderListFunc where

import Data.Char (isDigit, isUpper, isLower)

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

-- ЗАДАНИЕ:
-- Напишите функцию readDigits, принимающую строку и возвращающую пару строк.
-- Первый элемент пары содержит цифровой префикс исходной строки,
-- а второй - ее оставшуюся часть.
-- GHCi> readDigits "365ads"
-- ("365","ads")
-- GHCi> readDigits "365"
-- ("365","")

readDigits :: String -> (String, String)
readDigits = span isDigit

-- ЗАДАНИЕ:
-- Реализуйте функцию filterDisj, принимающую два унарных предиката и список,
-- и возвращающую список элементов, удовлетворяющих хотя бы
-- одному из предикатов.
-- GHCi> filterDisj (< 10) odd [7,8,10,11,12]
-- [7,8,11]
filterDisj pred1 pred2 = filter (\x -> pred1 x || pred2 x)

-- ЗАДАНИЕ:
-- Напишите реализацию функции qsort. Функция qsort должная принимать
-- на вход список элементов и сортировать его в порядке возрастания
-- с помощью сортировки Хоара: для какого-то элемента x изначального списка
-- (обычно выбирают первый) делить список на элементы меньше и не меньше x,
-- и потом запускаться рекурсивно на обеих частях.
-- GHCi> qsort [1,3,2,5]
-- [1,2,3,5]
-- Разрешается использовать только функции, доступные из библиотеки Prelude.
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort lesser ++ x : qsort others
  where (lesser, others) = (filter (< x) xs, filter (>= x) xs)

-- ФУНКЦИЯ MAP:
-- применить к каждому элементу контейнера некоторую функцию.
-- GHCi> map (+10) [1..5]
-- [11, 12, 13, 14, 15]

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map f xs

-- ФУНКЦИЯ CONCAT:
-- конкатенация списков.
concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs : xss) = xs ++ concat' xss
-- Предложение от LSP:
-- concat' = foldr (++) []

-- ФУНКЦИЯ CONCATMAP:
-- конкатенация обработанных некоторой функцией списков.
concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = concat' . map' f
-- GHCi> concatMap (\x -> [x, x, x]) "ABCD"
-- "AAABBBCCCDDD"

-- ЗАДАНИЕ:
-- Напишите функцию squares'n'cubes, принимающую список чисел,
-- и возвращающую список квадратов и кубов элементов исходного списка.
-- GHCi> squares'n'cubes [3,4,5]
-- [9,27,16,64,25,125]
squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x^2, x^3])

-- ЗАДАНИЕ:
-- Воспользовавшись функциями map и concatMap, определите функцию perms,
-- которая возвращает все перестановки, которые можно получить из данного
-- списка, в любом порядке.
-- GHCi> perms [1,2,3]
-- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
-- Считайте, что все элементы в списке уникальны, и что для пустого списка
-- имеется одна перестановка.
-- ОТВЕТ:
perms :: [a] -> [[a]]
perms = undefined

-- ЕЩЕ ПРО MAP - ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ДЛЯ БУЛЕВЫХ МАССИВОВ:
and', or' :: [Bool] -> Bool

-- функции - && и || для булевых массивов:
and' [] = True
and' (x : xs) = x && and' xs

or' [] = False
or' (x : xs) = x || or' xs
-- Ответ, почему именно так определено на пустых списках:
-- https://stackoverflow.com/questions/25427590/why-and-is-true-and-or-is-false

-- all:
all' :: (a -> Bool) -> [a] -> Bool
all' p = and' . map p

-- any:
any' :: (a -> Bool) -> [a] -> Bool
any' p = or' . map p
-- (.) помогает писать в бесточечном стиле, т.е. было бы так без нее:
-- any' p lst = or' $ map p lst

-- Еще пример map и (.) :
revWords = unwords . map reverse . words
_ = revWords "Abc is not ABC"
-- "cbA si ton CBA"

-- ЗАДАНИЕ:
-- Реализуйте функцию delAllUpper, удаляющую из текста все слова,
-- целиком состоящие из символов в верхнем регистре. Предполагается, что
-- текст состоит только из символов алфавита и пробелов, знаки пунктуации,
-- цифры и т.п. отсутствуют.
-- GHCi> delAllUpper "Abc IS not ABC"
-- "Abc not"
-- Постарайтесь реализовать эту функцию как цепочку композиций,
-- аналогично revWords из предыдущего видео.
-- ОТВЕТ:
delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words
