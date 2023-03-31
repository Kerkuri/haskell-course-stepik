module DemoSeq where

-- Здесь нет лишних вычислений,
-- поэтому seq ни на что не влияет
foo 0 x = x
foo n x = let
  x' = foo (n - 1) (x + 1)
  in x' `seq` x'

fooNoSeq 0 x = x
fooNoSeq n x = let
  x' = foo (n - 1) (x + 1)
  in x' 

-- Здесь f - лямбда-абстракция, т.е. она уже в WHNF.
-- Вычисление f' ничего не даст:
bar 0 f = f
bar x f = let
  f' = \a -> f (x + a)
  x' = x - 1
  in f' `seq` x' `seq` bar x' f'

barNoSeq 0 f = f
barNoSeq x f = let
  f' = \a -> f (x + a)
  x' = x - 1
  in barNoSeq x' f'


-- Здесь p уже в WHNF, поэтому seq ни на что не повлияет:
baz 0 (x, y) = x + y
baz n (x, y) = let
  x' = x + 1
  y' = y - 1
  p  = (x', y')
  n' = n - 1
  in p `seq` n' `seq` baz n' p

bazNoSeq 0 (x, y) = x + y
bazNoSeq n (x, y) = let
  x' = x + 1
  y' = y - 1
  p  = (x', y')
  n' = n - 1
  in bazNoSeq n' p

-- А вот здесь seq пресекает THUNK LEAK,
-- поэтому функция работает быстрее:
quux 0 (x, y) = x + y
quux n (x, y) = let
  x' = x + 1
  y' = y - 1
  p  = (x', y')
  n' = n - 1
  in x' `seq` y' `seq` n' `seq` quux n' p


quuxNoSeq 0 (x, y) = x + y
quuxNoSeq n (x, y) = let
  x' = x + 1
  y' = y - 1
  p  = (x', y')
  n' = n - 1
  in quuxNoSeq n' p
