fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)


------------------------


fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x -2) . filter even

fun2Helper :: Integer -> Integer
fun2Helper 1 = 0
fun2Helper n | even n = n `div` 2
             | otherwise = 3 * n + 1

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 0) . iterate fun2Helper

getHeight :: Tree a -> Integer
getHeight Leaf = 0
getHeight (Node height _ _ _) = height

treeWithNewLeaf :: a -> Tree a -> Tree a

-- Current tree is just a leaf
treeWithNewLeaf newItem Leaf = Node 0 Leaf newItem Leaf

-- Current tree's subtrees are both leaves
treeWithNewLeaf newItem (Node height Leaf topItem Leaf)
  = Node (height + 1) (Node height Leaf newItem Leaf) topItem Leaf

-- Current tree's subtrees are both trees
treeWithNewLeaf newItem (Node
                          height
                          (Node leftHeight leftLeftTree leftItem leftRightItem)
                          topItem
                          (Node rightHeight rightLeftTree rightLeftItem rightRightItem))
  | leftHeight == rightHeight = (Node ((getHeight rightSubtreeWithNewItem) + 1)
                                  leftSubtree topItem rightSubtreeWithNewItem)
  | leftHeight > rightHeight  = (Node height leftSubtree topItem
                                  rightSubtreeWithNewItem)
  | otherwise                 = (Node height leftSubtreeWithNewItem topItem
                                  rightSubtree)
    where leftSubtree = Node leftHeight leftLeftTree leftItem leftRightItem
          rightSubtree = Node rightHeight rightLeftTree rightLeftItem rightRightItem
          rightSubtreeWithNewItem = treeWithNewLeaf newItem (Node rightHeight rightLeftTree rightLeftItem rightRightItem)
          leftSubtreeWithNewItem = treeWithNewLeaf newItem (Node leftHeight leftLeftTree leftItem leftRightItem)

-- Current tree's left subtree is a leaf, and right subtree is a tree
treeWithNewLeaf newItem (Node height Leaf topItem rightTree)
  = Node height (Node (height - 1) Leaf newItem Leaf) topItem rightTree

-- Current tree's left subtree is a tree, and right subtree is a leaf
treeWithNewLeaf newItem (Node height leftTree topItem Leaf)
  = Node height leftTree topItem (Node (height - 1) Leaf newItem Leaf)

foldTree :: [a] -> Tree a
foldTree = foldr treeWithNewLeaf Leaf

main :: IO ()
main = do
  print $ fun1 [5, 6, 7, 8]
  print $ fun1' [5, 6, 7, 8]
  print $ fun2 42
  print $ fun2 41
  print $ fun2' 42
  print $ fun2' 41
  print $ foldTree "ABCDEFGHIJ"
