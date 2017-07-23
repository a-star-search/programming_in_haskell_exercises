
import qualified Data.Set as S
import Data.Char

combination_list = [(x, y) | x <- [1..3], y <- [x..3]]

--flattens a list of lists
myConcat :: [[a]] -> [a]
myConcat listOfLists = [x | aList <- listOfLists, x <- aList]

myLength :: [a] -> Int
myLength array = sum [1 | _ <- array]

factors :: Int -> [Int]
factors n = [a | a <- [1..n], n `mod` a == 0]

sameElems xs ys = S.fromList xs == S.fromList ys

prime :: Int -> Bool
prime n = sameElems actualFactors expectedFactors
  where actualFactors = factors n
        expectedFactors = [1, n]
        actualFactorCount = length actualFactors
        expectedFactorCount = length expectedFactors

primes :: Int -> [Int]
primes n = [p | p <- [2..n], prime p]

fetchSecondValueFromTuplesWithMatchingFirstValue :: Eq a => a-> [(a, b)] -> [b]
fetchSecondValueFromTuplesWithMatchingFirstValue key xs = [ val | (k, val) <- xs, key == k]

pairAdjacent :: [a] -> [(a, a)]
pairAdjacent xs = zip xs (tail xs)

isSorted :: Ord a => [a] -> Bool
isSorted xs = and [ x <= y | (x, y) <- pairAdjacent xs]

positions :: Eq a => a -> [a] -> [Int]
positions soughtVal xs = [ index | (index, val) <- zip [0..] xs, val == soughtVal]

countLowercaseCharacters :: String -> Int
countLowercaseCharacters str = length [ ch | ch <- str, ch >= 'a' && ch <= 'z']

letterToInt :: Char -> Int
letterToInt letter = ord letter - ord 'a'

intToLetter :: Int -> Char
intToLetter n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n letter = intToLetter (((letterToInt letter) + n) `mod` 26)

encode :: Int -> String -> String
encode shiftValue message = [shift shiftValue letter | letter <- message]

{-
  Exercises
  =========
-}

-- a = sum [ x^2 | x <- [1..100]]

coordinateGrid :: Int -> Int -> [(Int, Int)]
coordinateGrid n m = [(x, y) | x <- [0..n], y <- [0..m]]

{-
  Returns a coordinate square excluding the diagonal
-}
coordinateSquare :: Int -> [(Int, Int)]
coordinateSquare n = [(x, y) | (x, y) <- coordinateGrid n n, x /= y]

myReplicate :: Int -> a -> [a]
myReplicate n a = [ a | _ <- [1..n] ]

pythagoreans :: Int -> [(Int, Int, Int)]
pythagoreans limit = [(x, y, z) |
  x <- [1..limit], y <- [1..limit], z <- [1..limit], (x^2 + y^2) == z^2]

perfectNumbers :: Int -> [Int]
perfectNumbers limit = [ perfect |
  perfect <- [1..limit], (sum (factors perfect) - perfect) == perfect]

a = [(x, y) | x <- [1,2], y <- [3,4]]

b = concat [ [ (x, y) | y <- [3,4] ] | x <- [1, 2]]

c = a == b

-- The positions function using "fetchSecondValueFromTuplesWithMatchingFirstValue"
positions2 :: Eq a => a -> [a] -> [Int]
positions2 soughtVal xs =
  fetchSecondValueFromTuplesWithMatchingFirstValue soughtVal (zip xs [0..])

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct a b = sum (zipWith (*) a b)

insertInOrder :: Ord a => a -> [a] -> [a]
insertInOrder val [] = [val]
insertInOrder val (headVal:xs) =
  if val <= headVal then
    (val:headVal:xs)
  else (headVal:insertInOrder val xs)

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insertInOrder x (insertionSort xs)

myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (a:xsA) (b:xsB) = (a, b) : myZip xsA xsB

myDrop :: Int [a] -> [a]
myDrop dropCount [] = []
myDrop dropCount (_:listTail) = myDrop (dropCount - 1) listTail

myEven :: Int -> Bool
myEven 0 = True
myEven n = (myOdd n - 1)

myOdd :: Int -> Bool
myOdd 0 = False
myOdd n = (myEven n - 1)
