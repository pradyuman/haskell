data Tree a = EmptyNode | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyNode EmptyNode

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyNode = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a  = Node a (treeInsert x left) right
  | x > a  = Node a left (treeInsert x right)

treeDelete :: (Ord a) => a -> Tree a -> Tree a
treeDelete _ EmptyNode = EmptyNode
treeDelete x (Node a left right)
  | x == a = delete (Node a left right)
  | x < a  = Node a (treeDelete x left) right
  | x > a  = Node a left (treeDelete x right)

delete :: (Ord a) => Tree a -> Tree a
delete (Node _ EmptyNode right) = right
delete (Node _ left EmptyNode)  = left
delete (Node _ left right) = Node maxValue (treeDelete maxValue left) right
  where maxValue = treeMax left

treeMax :: (Ord a) => Tree a -> a
treeMax (Node a _ EmptyNode) = a
treeMax (Node a _ right) = treeMax right

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyNode = False
treeElem x (Node a left right)
  | x == a = True
  | x < a  = treeElem x left
  | x > a  = treeElem x right

fromList :: (Ord a) => [a] -> Tree a
fromList = foldr treeInsert EmptyNode
