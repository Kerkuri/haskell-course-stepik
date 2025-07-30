sumSquareOrSquareSumBad x y = if (x^2  + y^2) > (x + y)^2
  then (x^2 + y^2)
  else (x + y)^2


-- * лямбда здесь просто в демонстрационных целях
-- ! нужно обязательно обернуть степень в скобки, т.к. у применения функции наивысший приоритет
sumSquareOrSquareSum x y = (\a b -> max a b) (x^2 + y^2) ((x + y)^2)
-- ! лямбда-функции позволяют фактически сделать аналог блока where!
-- * c блоком where функция смотрелась бы следуюущим образом:
sumSquareOrSquareSumWhere x y = max sumSquare squareSum
  where
    sumSquare = x^2 + y^2 
    squareSum = (x + y)^2


doubleDoubleWhere x = 2 * dubs
  where dubs = 2 * x


doubleDoubleLambda n = (\x -> 2 * x) 2 * n

lamF = (\x -> 10 * x)
xTenTimes x = lamF x

sumSquareOrSquareSumLet x y = let
  sumSquare = x^2 + y^2
  squareSum = (x + y)^2
  in
    max sumSquare squareSum

overwrite x = let x = 2
  in let x = 3
    in let x = 4
      in x

overwriteLam x = (\x ->
  (\x ->
    (\x -> x) 4) 3) 2