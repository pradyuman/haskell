max' :: (Ord a) => a -> a -> a
max' a b
  | b > a = b
  | otherwise = a

compare' :: (Ord a) => a -> a -> Ordering
compare' a b
  | a > b = GT
  | a < b = LT
  | a == b = EQ

initials :: String -> String -> String
initials (al:_) (bl:_) = [al,bl]
