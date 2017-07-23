
luhnAlgorithm :: Int -> Bool
luhnAlgorithm n =
  checkValid (trimNumbers (doubleEveryOther (reverse digitArray)))
    where digitArray = toDigits n

toDigits :: Int -> [Int]
toDigits n =
  if n < 10 then
    [n]
  else
    (toDigits (n `div` 10)) ++ [n `mod` 10]

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther [a] = [a]
doubleEveryOther (a:b:ns) = (a:(b*2):(doubleEveryOther ns))

trimNumbers :: [Int] -> [Int]
trimNumbers ns = map (\x -> if x > 9 then x - 9 else x) ns

checkValid :: [Int] -> Bool
checkValid ns = (sum ns) `mod` 10 == 0
