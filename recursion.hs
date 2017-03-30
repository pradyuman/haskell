maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty list"
maximum' [a] = a
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Num a, Ord a) => a -> b -> [b]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x

take' :: (Num a, Ord a) => a -> [b] -> [b]
take' _ [] = []
take' n (x:xs)
  | n <= 0 = []
  | otherwise = x: take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]
-- reverse' xs = last xs : reverse' (init xs)

repeat' :: a -> [a]
repeat' a = a : repeat' a

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs)
  | e == x = True
  | otherwise =  elem' e xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  quicksort smaller ++ [x] ++ quicksort bigger
  where smaller = [a | a <- xs, a <= x]
        bigger = [b | b <- xs, b > x]
