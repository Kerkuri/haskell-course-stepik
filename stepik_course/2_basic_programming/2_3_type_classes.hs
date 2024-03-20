module TypeClasses where

-- СПЕЦИАЛЬНЫЙ ПОЛИМОРФИЗМ
-- Реализуется через КЛАСС ТИПОВ.
-- Класс типов - это интерфейс, через который реализуется
-- интерфейс для работы функции с разными типами данных.

-- Пример класса типов - Num.
-- Числовые константы:
-- 7 :: Num a => a
-- Справа от "=>" - тип
-- Слева от "=>" - контекст:
  -- Num - имя интерфейса

-- Еще один пример - класс типов Ord.
-- Такой интерфейс реализуется операторами сравнения:
-- (>) :: Ord a => a -> a -> Bool

-- Пример задания КЛАССА ТИПОВ:
class Eq' a where
  (==), (/=) :: a -> a -> Bool -- вместо "a" в конкретных реализациях будет свой тип
  x /= y = not (x TypeClasses.== y) -- реализация метода по умолчанию
-- здесь задали только интерфейс класса через сигнатуры функций
-- NB: приходится везде писать `TypeClasses.==`, потому что статическая проверка Haskell ругается.

-- ПРЕДСТАВИТЕЛЬ КЛАССА ТИПОВ:
instance Eq' Bool where
  True == True = True
  False == False = True
  _ == _ = False

-- ВАЖНЫЙ МОМЕНТ: в Haskell реализация интерфейса и реализация типов данных ОТДЕЛЕНЫ.

-- класс типов Printable:
class Printable a where
  toString :: a -> String

instance Printable Bool where
  toString True = "true"
  toString False = "false"

instance Printable () where
  toString () = "unit type"

-- РЕАЛИЗАЦИЯ КЛАССА ТИПОВ ДЛЯ ПОЛИМОРФНЫХ ТИПОВ
-- Пример: реализация сравнения кортежа длины 2

instance (Eq' a, Eq' b) => Eq'(a, b) where
  p1 == p2 = (fst p1 TypeClasses.== fst p2) && (snd p1 TypeClasses.== snd p2)

instance (Printable a, Printable b) => Printable(a, b) where
  toString (x, y) = "(" ++ toString x ++ "," ++ toString y ++ ")"
