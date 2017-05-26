import Data.List

-----------
-- SKIPS --
-----------

-- A helper function we use twice. Converts a list of any type into a list of tuples,
-- where the indices are attached to each element.
-- e.g. listWithIndices "ab" = [("a", 1), ("b", 2)]
--
-- Note that the indices are 1-indexed. This is because it helps with math later on.
listWithIndices :: [a] -> [(a, Int)]
listWithIndices x = zip x [1..(length x + 1)]

-- Given a number and a tuple, where the tuple's second element is also a number,
-- tell us if that tuple is a multiple of the number.
-- e.g.
-- elementIndexIsMultipleOf 3 ("Foo", 4) = False
-- elementIndexIsMultipleOf 3 ("Foo", 6) = True
elementIndexIsMultipleOf :: Int -> (a, Int) -> Bool
elementIndexIsMultipleOf n (_, index) = (index) `mod` n == 0

-- A helper function that uses the two above. Given a list of tuples where the first
-- element is some list, and the second element is a number, return
-- the "skipped-through" list based on that number. Best illustrated by example:
-- skipsHelper ("ABCD", 1) = ("ABCD")
-- skipsHelper ("ABCD", 2) = ("BD")
-- skipsHelper ("ABCD", 3) = ("C")
-- skipsHelper ("ABCD", 4) = ("D")
skipsHelper :: ([a], Int) -> [a]
skipsHelper (list, n) = fst(unzip (filter (elementIndexIsMultipleOf n) (listWithIndices list)))

-- Tying everything together. Given a list of length n,
-- generate n of that list. "AB" becomes ["AB", "AB"].
-- Then give it indices, so: [("AB", 1), ("AB", 2)]
-- Then use our powerful helper above.
skips :: [a] -> [[a]]
skips x = map skipsHelper (listWithIndices (take (length x) (repeat x)))

-----------------
-- LOCALMAXIMA --
-----------------

middleIsLocalMaximum :: [Integer] -> Bool
middleIsLocalMaximum (a:b:c:_) = b > a && b > c
middleIsLocalMaximum (_) = False

secondElement :: [Integer] -> Integer
secondElement (a:b:_) = b

-- First convert [1, 3, 2, 4] to [[1, 3, 2, 4], [3, 2, 4], [2, 4], [4], []] using 'tails'
-- Then take the first three of each of those lists, so [[1, 3, 2], [3, 2, 4], [2, 4], [4], []]
-- Then filter out the ones that don't have the middle element bigger than the other two
--   so [[1, 3, 2]]
-- Then just take the second element from each, so [3]
localMaxima :: [Integer] -> [Integer]
localMaxima n = map secondElement (filter middleIsLocalMaximum (map (take 3) (tails n)))

main :: IO ()
main = do
  print $ skips "ABCD"
  print $ skips "Hello!"
  print $ skips [1]
  print $ skips [True, False]
  print $ length(skips [])
  print $ localMaxima [2,9,5,6,1]
  print $ localMaxima [2,3,4,1,5]
  print $ localMaxima [1,2,3,4,5]
