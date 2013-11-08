module Utilities where

-- Takes a tuple of two functions (which takes a single value and returns a single value), and a tuple of two values, and returns a tuple with the first function applied to the first value, and the second function applied to the second value.
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

-- Pattern matches on the value and returns Nothing when the supplied value is Nothing, otherwise it returns a Just with the function applied to the supplied value.
mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

-- Takes a Maybe a and returns the first value if it is not Nothing, otherwise the second value regardless of what it is. 
orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

-- If the supplied function returns Nothing, then the supplied value will be returned, otherwise the result of the function application.
try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

-- Takes a reduction function and a value, if the value is not in its most reduced form, the fix function will be applied with the reduction function to the supplied value until it has been reduced to its base case.
fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

-- Takes a percentage [0.00, 1.00) and returns the element from the list which the supplied percentage maps to. Ex: [1,2,3] => [0.0 - 0.333..., 0.34 - 0.666..., 0.67 - 0.99]
pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs

