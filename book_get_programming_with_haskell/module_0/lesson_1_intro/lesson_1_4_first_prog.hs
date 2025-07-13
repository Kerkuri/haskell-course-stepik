module Main where
{-
* Если вы привыкли быстро написать программу, запустить её, а потом по-быстрому исправить сделанные ошибки, то Haskell вас разочарует.
! Haskell поощряет медитатив­ное сидение и обдумывание встреченных проблем до запуска программы!
* После того как вы наберётесь опыта, мы уверены, это разочарование пре­вратится в вашу любимую особенность Haskell.
* Оборотной стороной одер­жимости корректностью на этапе компиляции является то,
* что ваши про­ граммы будут работать гораздо более предсказуемо, чем вы, возможно, привыкли.
-}

{-
! Секрет спокойного написания кода без частых столкновений с ошибка ми состоит в том,
! чтобы писать его маленькими кусочками и тестировать эти кусочки в процессе написания.
-}

messyMain :: IO ()
messyMain = do
  putStrLn "Who is the receiver?"
  recipient <- getLine
  putStrLn "Title?"
  title <- getLine
  putStrLn "Who is the sender?"
  sender <- getLine
  putStrLn ("This is the " ++ title ++ " for our dearest friend " ++ recipient ++ ". \nWith best regards, " ++ sender)


getTitle :: [Char] -> [Char]
getTitle title = "This is the " ++ title

getRecipient :: [Char] -> [Char]
getRecipient recipient = "for our dearest friend " ++ recipient ++ "."

getSender :: [Char] -> [Char]
getSender sender = "With best regards, " ++ sender

getLetter :: [Char] -> [Char] -> [Char] -> [Char]
getLetter title recipient sender = getTitle title ++ " " ++ getRecipient recipient ++ "\n\n" ++ getSender sender

tmp = getLetter "GYM" "Billy" "Van"

main = do
  putStrLn "Who is the receiver?"
  recipient <- getLine
  putStrLn "Title?"
  title <- getLine
  putStrLn "Who is the sender?"
  sender <- getLine
  putStrLn (getLetter title recipient sender)
