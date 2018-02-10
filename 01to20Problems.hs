import Data.List

--1
--(*) Find the last element of a list.
myLast :: [a] -> a

myLast [] = error "Empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

myLast' = head.reverse

myLast'' :: [a] -> a
myLast''  = foldr1 (const id)

--2
--(*) Find the last but one element of a list.

myButLast :: [a] -> a
myButLast = head . tail . reverse

myButLast' :: [a] -> a
myButLast' []  = error "Empty list"
myButLast' [x,_]  = x
myButLast' (_ : xs)  = myButLast' xs

--3
--(*) Find the K'th element of a list. The first element in the list is number 1.

elementAt :: [a] -> Int -> a
elementAt xs n = valueAt xs (n - 1)

valueAt :: [a] -> Int -> a
valueAt (x:xs) 0 = x
valueAt (x:xs) n = valueAt xs (n - 1)

--4
--(*) Find the number of elements of a list.
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' = foldr (\_ a -> a + 1) 0
--myLength' = foldr (const (1 +)) 0  --From http://pointfree.io/

--5
--(*) Reverse a list.
myReverse :: [a] -> [a]
--myReverse = foldl (\a x -> x : a) []
myReverse = foldl (flip (:)) [] 

--6
--(*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs


--7
--(**) Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]

flatten (Elem a) = [a]
flatten (List xs) = concatMap flatten xs

flatten' (Elem a) = [a]
flatten' (List xs) = foldr (++) [] $ map flatten' xs


--8
--(**) Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a] 
compress = map head . group

--9
--(**) Pack consecutive duplicates of list elements into sublists.
pack [] = []
pack (x : xs) = let (fst, rst) = span (==x) xs 
                in (x:fst) : pack rst

 --10
 --(*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

encode cs = map (\xs -> (length xs, head xs)) $ pack cs


--11
--(*) Modified run-length encoding.

data Count a = Single a | Multiple Int a 
        deriving (Show)

list2Count [] = error "error"
list2Count [x] = Single x
list2Count xs = Multiple (length xs) (head xs)

encodeModified xs = map list2Count $ pack xs 

--12
--(**) Decode a run-length encoded list.
decode (Single a) = [a]
decode (Multiple n x) = replicate n x

decodeModified :: [Count a] -> [a]
decodeModified = concatMap decode

--13
--(**) Run-length encoding of a list (direct solution).
--Optimial
encode' :: Eq a => [a] -> [(Int,a)]
encode' = foldr helper []
        where 
         helper x [] = [(1,x)]
         helper x (m@(c,y):ms)
          | x == y = (c+1, y) : ms
          | otherwise = (1, x) : m : ms

encodeTuple (1,x) = Single x
encodeTuple (n,x) = Multiple n x

encodeDirect xs = map encodeTuple $ encode' xs 

--14
--(*) Duplicate the elements of a list.

dupli :: [a] -> [a]
dupli = foldr (\x xs -> x : x : xs) []

--15
--(**) Replicate the elements of a list a given number of times.

repli :: Int -> [a]  -> [a]
repli n = foldMap $ replicate n

--16
--(**) Drop every N'th element from a list.

dropEvery :: [a] -> Int -> [a]

dropEvery xs n = map fst $ filter (\(a,b) -> mod b n /= 0) $ zip xs [1..]

--17
--(*) Split a list into two parts; the length of the first part is given.

--split "abcdefghik" 3
split :: Int -> [a] -> ([a],[a])

split n [] = ([],[])
split n p@(x:xs)
        | n > 0 = let (f,l) = split (n-1) xs in (x : f , l)
        | otherwise = ([], p)
        