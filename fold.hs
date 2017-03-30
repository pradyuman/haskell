sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []
-- reverse' = foldl (\acc x -> x : acc) []

elem' :: (Eq a) => a -> [a] -> Bool
elem' x = foldl (\acc y -> if y == x then True else acc) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

product' :: (Num a) => [a] -> a
product' = foldl1 (*)

head' :: [a] -> a
head' = foldr1 (\x _ -> x)
-- head' = foldl1 (\acc _ -> acc)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)
-- last' = foldr1 (\_ acc -> acc)
