{- Problems taken mostly from https://wiki.haskell.org/index.php?title=H-99:_Ninety-Nine_Haskell_Problems -}

import Data.List

data NestedList a = Elem a | List [NestedList a]
    deriving (Eq, Show)
    
nlista = List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]

lista = [1,2,3,4,5]
listb = [8, 9, 10, 11, 12, 13, 14, 15]
listc = [12, 3, 7, 9, 14, 6, 11, 2]
listd = ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']


------------------------------------------------------

-- Find the last element of a list.
myLast :: [a] -> a
myLast xs = head $ drop (myLength xs - 1) xs


-- Find the last-but-one (or second-last) element of a list.
myButLast :: [a] -> a
myButLast xs = head $ drop (myLength xs - 2) xs


-- Find the K'th element of a list where the first element in the list is 1.
bangBang :: Int -> [a] -> a
bangBang i xs = myLast $ take i xs


-- Find the numbver of elements in a list.
myLength :: [a] -> Int
myLength [] = 0
myLength xs = myLength (tail xs) + 1


-- Reverse a list.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = (myLast xs) : (myReverse $ init xs)


-- Find out whether a list is a palindrome.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome []  = True
isPalindrome [a] = True
isPalindrome (x:xs)
    | x == (myLast xs) = isPalindrome $ tail $ init xs
    | otherwise        = False
    

-- Flatten a nested list structure
myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List xs) = concatMap myFlatten xs


-----------------------------------------------------------------

-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl f v []     = v
-- foldl f v (x:xs) = foldl f (f v x) xs

-- Eliminate consecutive duplicates of list elements
compress :: Eq a => [a] -> [a]
compress xs = reverse $ foldl go [] xs
    where
        go nxs x
            | elem x nxs = nxs
            | otherwise  = x : nxs


-- official solution
compressOff :: Eq a => [a] -> [a]
compressOff = map head . group


-- standard non-fold recursive solution
compressRec :: Eq a => [a] -> [a]
compressRec (x:y:ys)
    | x == y    = compress ys
    | otherwise = x : compress ys
compressRec xs = xs

-----------------------------------------------------------------

-- Pack consecutive duplicates of list elements into sublists.

pack :: Eq a => [a] -> [[a]]
pack []  = []
pack [a] = [[a]]
pack (x:y:xs)
            | x == y    = ([x,y] ++ (takeWhile (== x) xs)) : (pack $ dropWhile (== x) xs)
            | otherwise = [x] : pack (y:xs)



geminiPack :: Eq a => [a] -> [[a]]
geminiPack []     = []
geminiPack (x:xs) = let (first, rest) = span (== x) (x:xs)
              in first : geminiPack rest

-----------------------------------------------------------------

-- Run-length encoding of a list. Solutions

-- Use the result of pack to implement the so-called 
-- run-length encoding data compression method. Consecutive 
-- duplicates of elements are encoded as lists (N E) where
-- N is the number of duplicates of the element E. 

-- EXAMPLE
-- encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')] 

encode :: Eq a => [a] -> [(Int, a)]
encode xs = 
    let
        ps = pack xs
        ls = [myLength p | p <- ps]
        cs = compress xs
        ts = zip ls cs
    in
        ts

-----------------------------------------------------------------

-- Modified run-length encoding.

-- Modify the result of encoding in such a way that 
-- if an element has no duplicates it is simply copied 
-- into the result list. Only elements with duplicates 
-- are transferred as (N E) lists. 

-- encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

data ListItem a = Single a | Multiple Int a
    deriving (Show)
    
encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified xs = map transform (encode xs)
    where
        transform = \(x, a) -> if x == 1 
                               then Single a 
                               else Multiple x a

-----------------------------------------------------------------

-- Decode a run-length encoded list. Solutions
-- Given a run-length code list generated as specified in problem 11.
-- Construct its uncompressed version.

-- EXAMPLE:
-- decodeModified 
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"

decodeModified :: [ListItem a] -> [a]
decodeModified [] = []
decodeModified (Single x : xs)     = x : decodeModified xs
decodeModified (Multiple n x : xs) = replicate n x ++ decodeModified xs 



-- Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
  
{-
encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect = map transform . processList

transform :: (Int, a) -> ListItem a
transform (1, e) = Single e
transform (n, e) = Multiple n e

processList :: Eq a => [a] -> [(Int, a)]
processList xs = zip (pCount xs) (compress xs)

pCount :: Eq a => [a] -> [Int]
pCount ys@(x:xs) = foldr step [] ys
    where
        step new (count:ys)
            | new == x  = step 
            | otherwise = 1 : ys


-- ys@(x:xs) makes ys a name for the pattern-matched (x:xs)
-- foldrMap = (\x recur -> f x : recur) [] xs
-}














-- foldr is an abstraction of any function that operates
-- on the head of a list and makes a recursive call to
-- the tail of the list. 
-- foldr f v xs -> f = recursive function, return on base case, list






stutter :: [a] -> [a]
stutter [] = []
-- stutter xs = map (replicate' 2) xs
-- stutter xs = replicate' 2 xs ++ stutter 2 (tail xs)
-- stutter (x:xs) = x : x : stutter xs
stutter xs = xs >>= go
    where
        go x = [x, x]


filter' :: (a -> Bool) -> [a] -> [a]
-- filter' p xs = flatten (map go xs)
filter' p xs = xs >>= go
    where
        go x = if p x then [x] else []


---- QUIZ 9 PROBLEMS

topN :: Int -> [(String, Int)] -> [String]
topN n ts = [str | (str, i) <- ts, i > n]

-- ALTERNATIVE WITH LAMBDAS
-- topN n ts = map (\(str, i) -> str) ls
--     where
--         ls = filter (\(str, i) -> (i > n)) ts


riffle :: [a] -> [a] -> [a]
riffle [] ys         = ys
riffle xs []         = xs 
riffle (x:xs) (y:ys) = [x,y] ++ riffle xs ys 



replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x : replicate' (n - 1) x 



(!!!) :: [a] -> Int -> a
(!!!) xs n = go (0, n) xs
    where
        go (i, n) (x:xs)
            | i == n    = x
            | otherwise = go (i + 1, n) xs



matrixMap :: (a -> b) -> [[a]] -> [[b]]
matrixMap f xss = map (map f) xss  




-- FALL 2025 EXAM 2 CODING PROBLEMS

isSuffix :: Eq a => [a] -> [a] -> Bool
isSuffix suff xs =
    let
        lenSuff = length suff
        dropLen = (length xs) - lenSuff
        dropxs  = drop dropLen xs
    in
        suff == dropxs


keepIf :: (a -> Bool) -> [a] -> [b] -> [b]
keepIf _ [] _ = []
keepIf _ _ [] = []
keepIf p (x:xs) (y:ys)
    | p x       = y : keepIf p xs ys
    | otherwise = keepIf p xs ys




