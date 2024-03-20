-- В Haskell возможно РАСШИРЕНИЕ КЛАССА ТИПОВ.
-- Это аналог наследования из ООП.
-- Только наследуются не реализации (как в ООП), а интерфейсы.

-- ПРИМЕР РАСШИРЕНИЯ КЛАССА ТИПОВ (НАСЛЕДОВАНИЕ):

-- Пусть это исходный класс (класс-родитель):
{-
class Eq a where
  (==), (/=) :: a -> a -> Bool
  x /= y = not (x == y)
  x == y = not (x /= y)
-}
-- Расшиерние класса Eq (класс-наследник):
{-
class (Eq a) => Ord a where
  (<), (<=) , (>=), (>) :: a -> a -> Bool
  max, min :: a -> a -> a
  compare :: a -> a -> Ordering -- функция для более тщательного сравнения значений в Haskell
{- Minimal complete definition: either compare or <= -} -- Т.е. все остальное задано циклично.
-}

-- Расширение двух классов (МНОЖЕСТВЕННОЕ НАСЛЕДОВАНИЕ):
{-
class (Eq a, Printable a) => MyClass a where
-}

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

-- class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
--     stompOrStab :: a -> a
--     stompOrStab a
--       | doesEnrageMork a && not (doesEnrageGork a) = stomp a
--       | doesEnrageGork a && not (doesEnrageMork a) = stab a
--       | doesEnrageGork a && doesEnrageMork a = stomp . stab $ a
--       | otherwise = a

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
  stompOrStab :: a -> a
  stompOrStab a = (stompIf . stabIf) a where
    stompIf = if doesEnrageMork a then stomp else id
    stabIf = if doesEnrageGork a then stab else id

-- КЛАССЫ ТИПОВ SHOW & READ
_ = show 1 -- Вернет "1"
_ = show [1, 2] -- Вернет "[1, 2]"
_ = read "1" :: Int -- Вернет 1
_ = read "[1, 2]" :: [Int] -- Вернет [1, 2]

-- Класс типов Enum.
-- Аналог встроенного Enum:
class Enum' a where
  succ', pred' :: a -> a -- возвращает следующий или предыдущий элемент Enum'a
  toEnum' :: Int -> a -- переводит номер в соответствующий Enum
  fromEnum' :: a -> Int -- получает номер элемента Enum'a

_ = succ 1 -- вернет 2
_ = pred True -- вернет False
_ = toEnum 1 :: Bool -- вернет True
_ = fromEnum 'a' -- вернет 97

-- Расширение стандартного Enum:
class (Enum a, Bounded a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x
    | x == maxBound = minBound 
    | otherwise = succ x
  spred :: a -> a
  spred x
    | x == minBound = maxBound
    | otherwise = pred x

-- Как объявлять типы представителями расширенного тайпкласса:
instance SafeEnum Bool
instance SafeEnum Int

avg :: Int -> Int -> Int -> Double
avg a b c = (fromIntegral a + fromIntegral b + fromIntegral c) / 3.0
