
myeven :: Integral a => a -> Bool
myeven n = n `mod` 2 == 0

mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt n xs = ( take n xs, drop n xs)

myrecip :: Fractional a => a -> a
myrecip n = 1/n

myabs :: Int -> Int
myabs n = if n >= 0 then n else -n

mysignum :: Int -> Int
mysignum n = if n > 0 then 1 else
             if n < 0 then -1 else 0

mysignum2 :: Int -> Int
mysignum2 n | n == 0 = 0
            | n > 0 = 1
            | otherwise = -1

myNot :: Bool -> Bool
myNot False = True
myNot True = False

myAnd :: Bool -> Bool -> Bool
myAnd True True = True
myAnd _ _ = False

myFirst :: (a, b) -> a
myFirst (x, _) = x

mySecond :: (a, b) -> b
mySecond (_, y) = y

-- Note that due to precedence rules, the argument to the function must be
-- parenthesised
myHead :: [a] -> a
myHead (a:_) = a

myTail :: [a] -> [a]
myTail (_:xs) = xs

--lambda expressions are nameless equations
-- \x -> x + x

myOdds :: Int -> [Int]
myOdds n = map f [0..n-1]
           where f x = x*2 + 1

myOdds2 :: Int -> [Int]
myOdds2 n = map (\x -> x*2+1) [0..n-1]

halve :: [a] -> ([a], [a])
halve ns = (take half ns, drop half ns)
  where half = (length ns) `div` 2

third_a :: [a] -> a
third_a (_:xs) =  head (tail xs)

third_b :: [a] -> a
third_b ns = ns !! 2

third_c :: [a] -> a
third_c (_:_:x:_) = x

-- Using a conditional expression
safetail_ce :: [a] -> [a]
safetail_ce ns = if null ns then [] else tail ns

-- Using guarded equations
safetail_ge :: [a] -> [a]
safetail_ge ns | null ns = []
  | otherwise = tail ns

-- Using pattern matching
safetail_pm :: [a] -> [a]
safetail_pm [] = []
safetail_pm (_:xs) =xs

(||) :: Bool -> Bool -> Bool
True || _ = True

bla :: Bool -> Bool -> Bool
bla x y =
  if x then
    if y then True else False
  else False

myMult :: Int -> Int -> Int -> Int
myMult x y z = x*y*z

myMultLambda :: Int -> (Int -> (Int -> Int))
myMultLambda = \x -> (\y -> (\z -> y * z * x))
