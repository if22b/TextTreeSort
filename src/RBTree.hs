module RBTree (buildTree, inOrder) where

data Color = Red | Black deriving (Show, Eq)
data RBTree a = Nil | Node Color (RBTree a) a (RBTree a) deriving (Show)

insertString :: (Ord a) => RBTree a -> a -> RBTree a
insertString Nil x = Node Red Nil x Nil  -- empty tree: create a new red node
insertString (Node color left val right) x
  | x < val = balance color (insertString left x) val right   -- insert left
  | x > val = balance color left val (insertString right x)   -- insert right
  | otherwise = Node color left val right                     -- ignore duplicates

makeBlack :: RBTree a -> RBTree a
makeBlack (Node _ left val right) = Node Black left val right
makeBlack Nil = Nil

insert :: (Ord a) => a -> RBTree a -> RBTree a
insert value tree = makeBlack (insertString tree value)  -- root is black

balance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balance Black (Node Red (Node Red a x b) y c) z d = Node Red (Node Black a x b) y (Node Black c z d) -- Links-Links-Fall
balance Black (Node Red a x (Node Red b y c)) z d = Node Red (Node Black a x b) y (Node Black c z d) -- Links-Rechts-Fall
balance Black a x (Node Red (Node Red b y c) z d) = Node Red (Node Black a x b) y (Node Black c z d) -- Rechts-Links-Fall
balance Black a x (Node Red b y (Node Red c z d)) = Node Red (Node Black a x b) y (Node Black c z d) -- Rechts-Rechts-Fall
balance color left val right = Node color left val right -- no balance needed

inOrder :: RBTree a -> [a]
inOrder Nil = []
inOrder (Node _ left val right) = inOrder left ++ [val] ++ inOrder right

-- "foldr" adds every word one at a time
buildTree :: [String] -> RBTree String
buildTree = foldr insert Nil