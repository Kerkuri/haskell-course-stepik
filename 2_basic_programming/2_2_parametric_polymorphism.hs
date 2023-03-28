module ParametricPoly2 where
import Data.Function

-- КОМПОЗИЦИЯ ФУНКЦИЙ 

-- Оператор композиции - (.)
compose' f g = f . g

-- Первая версия функции суммы первых элементов пары пар:

pp1 = ((1, 2), (3, 4))
pp2 = ((3, 4), (5, 6))

sumFstFst = (+) `on` fstFst where
  fstFst pp = fst $ fst pp

-- Вторая версия - через лямбда-функцию:
sumFstFst' = (+) `on` (\pp -> fst $ fst pp)

-- Третья версия - лямбда-функция с использованием композиции:
sumFstFst'' = (+) `on` (fst . fst)

{- Цепочка последовательных применений функций может быть заменена композицией:
doIt x = f (g (h x))
doIt = f . g . h
-}

doItYourself = f . g . h

f = logBase 2

g = (^3)

h = max 42

-- Еще один способ задать кортеж:
tuple = (,) True 3 -- tuple = (True, 3)
tuple' = (,,) True 3 "str" -- tuple' = (True, 3, "str")

dup x = (x, x)

-- КАРРИРОВАНИЕ
-- процедура перехода от функций, принимающих аргументы по одному (КАРРИРОВАННЫЕ),
-- к функциям, принимающим аргументы в виде кортежа (НЕКАРРИРОВАННЫЕ),
-- и наоборот.

-- Как сделать функцию каррированной:
fstCurry = curry fst
-- fstCurry 1 2 == fst (1, 2)

-- Как сделать функцию некаррированной:
plusPair = uncurry (+)

-- аналог функции swap из Data.Tuple:
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

swap' = uncurry (flip (,))
