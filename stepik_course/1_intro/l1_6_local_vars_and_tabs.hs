-- Основной принцип отступов в Haskell:
-- увеличение отступов безопасно, уменьшение же может привести к проблемам.
-- Увеличение отступа означает, что мы продолжаем определение,
-- которое было начато на предыдущей строке

module LocalsTabs where

rootsSquare :: Double -> Double -> Double
  -> (Double, Double)

{-
rootsSquare a b c =
  (
    -- Почему ниже подчеркивает `b` и выдает ошибку при компиляции
    -- Непонятно, в чем дело
    (-b - sqrt $ (b ^ 2 - 4 * a * c)) / (2 * a),
    (-b + sqrt $ (b ^ 2 - 4 * a * c)) / (2 * a))
-}

-- LET IN - для локально определенных переменных
rootsSquare a b c =
  let d = sqrt (b ^ 2 - 4 * a * c) in
  (
    (-b - d) / (2 * a),
    (-b + d) / (2 * a)
  )
    

-- Через LET IN можно подставлять
-- произвольное количество локальных переменных:
rootsSquare2 a b c =
  let {
    -- Порядок связываний (определений локальных переменных)
    -- здесь не важен:
    d = sqrt $ b ^ 2 - 4 * a * c;
    x1 = (-b - d) / (2 * a);
    x2 = (-b + d) / (2 * a)
  }
  in (x1, x2)

-- Вместо фигурных скобок можно использовать отступы:
rootsSquare3 a b c =
  let 
    d = sqrt $ b ^ 2 - 4 * a * c;
    x1 = (-b - d) / (2 * a);
    x2 = (-b + d) / (2 * a)
  in (x1, x2)

-- Через LET IN можно определять и локальные функции.
-- Функция с вспомогательной функцией, которая определена отдельно:
factorial5 :: Integer -> Integer
factorial5 n
  | n < 0 = error "arg must be >= 0"
  | otherwise = helper 1 n

helper acc 0 = acc
helper acc n = helper (acc * n) (n - 1)

-- Переписанная функция factorial5 с использованием LET IN:
factorial6 n
  | n >= 0 =
    let
      helper acc 0 = acc
      helper acc n = helper (acc * n) (n - 1)
    in helper 1 n
  | otherwise = error "arg must be >= 0"

-- Еще можно локально распаковывать переменные:
rootsDiff a b c =
  let
    (x1, x2) = rootsSquare3 a b c
  in x2 - x1

-- seqA :: Integer -> Integer
-- seqA n =
--     let
--       helper a b c n
--       | n == 0 = 1
--       | n == 1 = 2
--       | n == 2 = 3
--       | otherwise = helper b c (b + c - 2 * a) (n - 1)
--     in helper 1 2 3 n


-- Первая рабочая версия, но падает по TimeLimit:
-- seqA :: Integer -> Integer
-- seqA n
--   | n == 0 = 1
--   | n == 1 = 2
--   | n == 2 = 3
--   | otherwise = seqA (n - 1) + seqA (n - 2) - 2 * seqA (n - 3)

seqA :: Integer -> Integer
seqA n
    | n == 0 = 1
    | n == 1 = 2
    | n == 2 = 3
    | otherwise = helperSeq 3 2 1 n

helperSeq :: Integer -> Integer -> Integer -> Integer -> Integer 
helperSeq n3 n2 n1 2 = n3
helperSeq n3 n2 n1 n = helperSeq (n3 + n2 - 2*n1) n3 n2 (n-1)



-- Аналог LET IN - WHERE
-- WHERE устроен наоборот:
-- сначала часть с вычислениями,
-- потом часть с локальными связываниями

-- Версия с LET IN:
rootsSquare4 a b c =
  let 
    d = sqrt $ b ^ 2 - 4 * a * c;
    x1 = (-b - d) / (2 * a);
    x2 = (-b + d) / (2 * a)
  in (x1, x2)

-- Версия с WHERE:
rootsSquare5 a b c = (x1, x2) where
  x1 = (-b - d) / aTwice
  x2 = (-b + d) / aTwice
  d = sqrt $ b ^ 2 - 4 * a * c
  aTwice = 2 * a
  
-- Отличие WHERE от LET IN:
-- LET IN является выражением и может быть использовано в других выражениях:
y = (let x = 2 in x^2)^2
-- WHERE не является выражением и в таких конструкциях не может быть использовано.
-- WHERE можно быть использовать только в определении функций

sum'n'counts :: Integer -> (Integer, Integer)
sum'n'counts x
  | x == 0 = (0, 1)
  | otherwise = (sumDigits, countDigits) where
    (sumDigits, countDigits) = helper 0 0 (abs x)
    helper partSum partCount 0 = (partSum, partCount)
    helper partSum partCount part = helper (partSum + (mod part 10)) (partCount + 1) (div part 10)

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = trapezArea where
  h = (b - a) / 1000
  trapezArea = accArea 0 0  a (a+h)
  accArea acc 1000 x1 x2 = acc
  accArea acc i x1 x2 = accArea (acc + accInc) (i+1) x2 (x2+h) where
    accInc = h * (f x1 + f x2) / 2
