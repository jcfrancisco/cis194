import Data.Monoid
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty           = mempty
tag (Single m _)    = m
tag (Append m _ _)  = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty     +++ joinList  = joinList
joinList  +++ Empty     = joinList
l1        +++ l2        = Append (tag l1 <> tag l2) l1 l2

-- indexJ i j: find the nth element of JoinList j
-- if the index is out of bounds, return Nothing
indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ _ Empty                          = Nothing
indexJ 0 (Single _ a)                   = Just a
indexJ _ (Single _ a)                   = Nothing
-- "indexJ 3 (Append 3 ... ...)" is out of bounds
indexJ i (Append m _ _)   | Size i >= size m
  = Nothing
-- if L1 has i+1 or more elements, find the ith element in l1
indexJ i (Append _ l1 _)  | size (tag l1) > Size i
  = indexJ i l1
-- if L1 has i or fewer elements, find it in L2 (adjust the index)
-- Ex. indexJ 4 (l1 with size 4) l2 means I should look for 0th element in l2
indexJ i (Append _ l1 l2)
  = indexJ (i - getSize(size (tag l1))) l2

dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ n l | n <= 0   = l
dropJ _ Empty        = Empty
dropJ _ (Single _ _) = Empty
-- if l1 has more than n elements, dropJ n l1
dropJ n (Append _ l1 _) | size (tag l1) > Size n
  = dropJ n l1
-- if l1 has n or fewer elements, dropJ (n - tag l1) l2
dropJ n (Append _ l1 l2)
  = dropJ (n - getSize(size (tag l1))) l2

takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0   = Empty
takeJ _ Empty        = Empty
takeJ _ (Single m a) = Single m a
-- if l1 has n or more elements, takeJ n l1
takeJ n (Append m l1 _) | size (tag l1) >= Size n
  = takeJ n l1
-- if l1 has fewer than n elements, takeJ n l1 +++ takeJ (n - tag l1) l2
takeJ n (Append m l1 l2) | size (tag l1) < Size n
  = takeJ n l1 +++ takeJ (n - getSize(size (tag l1))) l2

main :: IO ()
main = do
  -- print $ "The next line should look like Append (Product 21) (...) (...)"
  -- print $ Single (Product 7) 'a' +++ Single (Product 3) 'b'
--   print $ indexJ 0 (Single (Size 1) 'a' +++ Single (Size 1) 'b')
--   print $ indexJ 1 (Single (Size 1) 'a' +++ Single (Size 1) 'b')
  print $ "indexJ 2 of a list of size 2 (should be Nothing)"
  print $ indexJ 2 (Single (Size 1) 'a' +++ Single (Size 1) 'b')
  print $ "DROPS"
  print $ dropJ  0 (Single (Size 1) 'a' +++ Single (Size 1) 'b')
  print $ dropJ  1 (Single (Size 1) 'a' +++ Single (Size 1) 'b')
  print $ dropJ  2 (Single (Size 1) 'a' +++ Single (Size 1) 'b')
  print $ dropJ  5 (Single (Size 1) 'a' +++ Single (Size 1) 'b')
  print $ "TAKES"
  print $ "Take 0"
  print $ takeJ  0 (Single (Size 1) 'a' +++ Single (Size 1) 'b')
  print $ "Take 1"
  print $ takeJ  1 (Single (Size 1) 'a' +++ Single (Size 1) 'b')
  print $ "Take 2"
  print $ takeJ  2 (Single (Size 1) 'a' +++ Single (Size 1) 'b')
  print $ "Take 5"
  print $ takeJ  5 (Single (Size 1) 'a' +++ Single (Size 1) 'b')
