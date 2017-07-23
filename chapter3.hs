
--[False, True, False] :: [Bool]
--(False, 'a', True) :: (Bool, Char, Bool)

add :: (Int, Int) -> Int
add (x, y) = x + y

-- This is a curried function, the parenthese can be ommited btw
--  In haskell, all functions with multiple arguments are curried
add' :: Int -> (Int -> Int)
add' x y = x + y

-- For example, a function that increments an integer can be given by the
-- partial application add’1 :: Int -> Int of the curried function add’ with
-- only one of its two arguments.
add'1 :: Int -> Int
add'1 = add' 1

bools :: [Bool]
bools = [False, True]

nums :: [[Int]]
nums = [[0]]

add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f x = f x
