calcChange owed given = if change > 0
  then change
  else 0
  where change = given - owed

calcChangeMax owed given = max change 0
  where change = given - owed

x = 2
-- ! нельзя переприсвоить значение переменной
-- x = 3


{-
Задача 2.1. Для написания функции calcChange мы использовали конструкцию if then else.
В Haskell все if-выражения должны содержать компонент else. Почему, согласно нашим трём правилам для функций,
if нельзя использовать само по себе?
-}
-- * Из-за того что все функции в Haskell обязаны возвращать значение, if-блок не может использоваться без else-блока:
doubleIfPositive x = if x > 0
  then 2 * x
  else x

{-
Задача 2.2. Напишите функции с названиями inc, double и square, ко торые увеличивают, удваивают и возводят в квадрат аргумент n соответственно.
-}
inc n = n + 1
double n = 2 * n
square n = n ** 2

{-
Задача 2.3. Напишите функцию, которая принимает значение n.
Если n чётное, то функция возвращает n-2, а если нечётное, то 3×n+1.
Чтобы проверить, чётно ли число, вы можете использовать функции even (проверка на чётность) или mod (взятие остатка от деление).
-}
doStuffIfEven n = if even n
  then n - 2
  else 3 * n + 1