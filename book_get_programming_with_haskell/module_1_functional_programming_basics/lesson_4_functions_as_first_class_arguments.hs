import Data.List (sort, sortBy)

ifEvenDo func x = if even x
  then func x
  else x

-- ifEvenDouble = ifEvenDo (\x -> 2 * x)
ifEvenDouble = ifEvenDo (2 *)

-- ifEvenInc = ifEvenDo (\x -> x + 1)
ifEvenInc = ifEvenDo (+ 1)


names = [
  ("Biba", "Bibovski"),
  ("Pupa", "Pupovski"),
  ("Pupa", "Lupovski"),
  ("Lupa", "Lupovski"),
  ("Boga", "Armanian"),
  ("Zhoga", "Dzhigarhanian")]

sortedByDefaultNames = sort names

-- sortedByLastNames = sortBy (\x y -> if snd x > snd y then GT else if snd x < snd y then LT else EQ) names

-- cmpLastNames nameX nameY = if lastX > lastY
--   then GT
--   else if lastX < lastY
--     then LT
--     else EQ
--   where
--     lastX = snd nameX
--     lastY = snd nameY

-- cmpLastNames nameX nameY
--   | lastX > lastY = GT
--   | lastX < lastY = LT
--   | (lastX == lastY) && (firstX > firstY) = GT
--   | (lastX == lastY) && (firstX < firstY) = LT
--   | otherwise = EQ
--   where
--     lastX = snd nameX
--     lastY = snd nameY
--     firstX = fst nameX
--     firstY = fst nameY
cmpLastNames nameX nameY
  | lastX /= lastY = compare lastX lastY
  | otherwise = compare nameX nameY
  where
    lastX = snd nameX
    lastY = snd nameY
    firstX = fst nameX
    firstY = fst nameY



sortedByLastNames = sortBy cmpLastNames names
