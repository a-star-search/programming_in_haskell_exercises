
--factorial does not allow negative numbers
factorial :: Int -> Int
factorial n | n < 0 = error "Negative numbers are not allowed."
            | n == 0 = 1
            | otherwise = n * factorial (n-1)

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

expon :: Int -> Int -> Int
expon _  0 = 1
expon m  n = m * (expon m (n-1))

euclidsAlgo :: Int -> Int -> Int
euclidsAlgo n m | n == m = n
                | otherwise = euclidsAlgo (min n m) (max n m - min n m)

-- The following are a handful of functions included in Prelude implemented recursively

recursiveAnd :: [Bool] -> Bool
recursiveAnd [False] = False
recursiveAnd [True] = True
recursiveAnd (False:listTail) = False
recursiveAnd (True:listTail) = recursiveAnd listTail

recursiveConcat :: [[a]] -> [a]
recursiveConcat list  | null list = []
                      | null (list !! 0) = recursiveConcat (tail list)
                      | length (head list) == 1 = head list ++ recursiveConcat (tail list)
                      | otherwise = [head (head list)] ++ recursiveConcat [tail (head list)] ++ recursiveConcat (tail list)


recursiveReplicate :: Int -> a -> [a]
recursiveReplicate 0 _ = []
recursiveReplicate n object = object : recursiveReplicate (n-1) object

recursiveNth :: [a] -> Int -> a
recursiveNth (x:xs) 0 = x
recursiveNth (x:xs) position = recursiveNth xs (position-1)

recursiveIsElem :: Eq a => a -> [a] -> Bool
recursiveIsElem val [] = False
recursiveIsElem val (x:xs) = if val == x then True else recursiveIsElem val xs

mergeSortedLists :: Ord a => [a] -> [a] -> [a]
mergeSortedLists [] list = list
mergeSortedLists list [] = list
mergeSortedLists (x:xs) (y:ys)  | x <= y = x : mergeSortedLists xs (y:ys)
                                | otherwise = y : mergeSortedLists (x:xs) ys

mergeSortAlgo :: Ord a => [a] -> [a]
mergeSortAlgo [] = []
mergeSortAlgo [x] = [x]
mergeSortAlgo list = mergeSortedLists
  (mergeSortAlgo (fst (halve list)))
  (mergeSortAlgo (snd (halve list)))

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve [x] = ([x], [])
halve list = (take halfLength list, drop halfLength list)
  where halfLength = (length list) `div` 2

--Exercise 9

-- Calculate sum of list of numbers

mySum :: Num a => [a] -> a
mySum [] = 0
mySum [x] = x
mySum (x:xs) = x + mySum xs
