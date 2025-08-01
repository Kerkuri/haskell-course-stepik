-- Циклов в функциональных ЯП не может быть,
-- т.к. понятия изменяющейся переменной, что значит, что мы не можем
-- отличить одну итерацию цикла от другой.
-- В функциональных ЯП рекурсия - это основной инструмент повторяющихся вычислений
-- по аналогии с циклами в императивных ЯП.

-- Определение функции в Haskell называется РЕКУРСИВНЫМ, если в правой
-- части этого определения присутствует вызов самой определяемой функции.

module Recursion where

-- Пример определения рекурсивной функции - ФАКТОРИАЛ
factorial n
  | n == 0 = 1
  | otherwise = n * factorial (n - 1)

-- Требования к определению рекурсивных функций,
-- чтобы они не зациклились:
-- 1) Рекурсинвый вызов функции в теле ее определения должен происходить
-- на параметрах, отличных от формального параметра функции
-- (т.е. если "factorial n", то рекурсивный вызов уже "factorial (n - 1)")
-- 2) У рекурсивной функции должно быть терминирующее условие

-- Как происходит вычисление рекурсивной функции.
-- Вычисление рекурсии в Haskell происходит путем подстановки:
{-
factorial 2
  ~> if 2 == 0 then 1 else 2 * factorial 1
  ~> 2 * factorial 1
  ~> 2 * (if 1 == 0 then 1 else 1 * factorial 0)
  ~> 2 * 1 * factorial 0
  -- умножение вычисляется сразу:
  ~> 2 * (if 0 == 0 then 1 else 1 * factorial (-1))
  ~> 2 * 1
  ~> 2
-}

-- СОПОСТАВЛЕНИЕ С ОБРАЗЦОМ - аналог ветвления
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

-- Двойной факториал:
-- doubleFact :: Integer -> Integer
-- doubleFact n
--   | n <= 0 = 1
--   | otherwise = n * doubleFact (n - 2)

doubleFact :: Integer -> Integer
doubleFact (-1) = 1
doubleFact 0 = 1
doubleFact n = n * doubleFact (n - 2)

-- RAISE EXCEPTION в Haskell:
-- error "ABC" - выводит сообщение с ошибкой "ABC" в std.err
-- undefined - выводит стандартное сообщение об ошибке в std.err
factorial'' 0 = 1
factorial'' n
  | n < 0 = error "arg must be >= 0"
  | otherwise = n * factorial'' (n - 1)

-- В статической семантике Haskell незавершающася рекурсия
-- и прерывание программы из-за ошибки - это одно и то же. 
-- Считается что в этом случае возвращаемым значением программы
-- является специальный символ Bottom.
-- Функция undefined - способ использовать Bottom.
-- Bottom является значением любого типа, поэтому им
-- можно пользоваться в любом месте программы.
-- В Haskell также принято пользоваться undefined, чтобы
-- маркировать еще не написанные части программы.

-- Пример использования undefined в виде заглушки:
kek x = undefined

-- ОХРАННЫЕ ВЫРАЖЕНИЯ - GUARDS
-- Это символы "|", которыми можно расщеплять выражение на несколько уравнений:
factorial''' 0 = 1
factorial''' n
  | n < 0 = error "arg must be >= 0"
  | n > 0 = n * factorial''' (n - 1)

factorial4 :: Integer -> Integer
factorial4 n
  | n == 0 = 1
  | n > 0 = n * factorial4 (n - 1)
  | otherwise = error "arg must be >= 0"
-- otherwise - это на самом деле константа, равная True
-- т.е. в функции factorial4 последняя строка может быть переписана так:
-- True = error "arg must be >= 0"

fibonacci :: Integer -> Integer
fibonacci n
  | n == (-2) = -1
  | n == (-1) = 1
  | n == 0 = 0
  | n == 1 = 1
  | n > 1 = fibonacci (n - 1) + fibonacci (n - 2)
  | otherwise = fibonacci (n + 2) - fibonacci (n + 1)

-- Аналог реализации факториала с вспомогательной переменной,
-- которая бы аккумулировала значение факториала в цикле:
factorial5 :: Integer -> Integer
factorial5 n
  | n < 0 = error "arg must be >= 0"
  | otherwise = helper 1 n

-- вспомогательная функция-аккумулятор:
helper acc 0 = acc
helper acc n = helper (acc * n) (n - 1)
-- Такой трюк помогает упростить асимптотику в некоторых случаях


-- ЗАМЕР ВРЕМЕНИ
-- вызвать :set +s перед вызовом функции в ghci
fibonacciLinear :: Integer -> Integer
fibonacciLinear = helperFib 0 1

helperFib a b n
  | n == 0 = a
  | n >= 1 = helperFib b (a + b) (n - 1)
  | otherwise = helperFib b (a - b) (n + 1)
