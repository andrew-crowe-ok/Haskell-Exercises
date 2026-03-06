{-------
--------             POINTER-CHASING ALGORITHM
--------    (CS-341L BOMB LAB - PHASE 5 DEFUSE HELPER)
-------}

{--- PROBLEM
Given an unsorted integer array of length n whose elements are the values 0..n-1, 
what starting indices will result in landing on the largest value in the array (n-1)
on the n-1'th iteration of a pointer-chasing algorithm? Additionally, what is the sum
of all values visited on that path? 

In the context that this code was written for, the values returned are needed to 
"defuse a bomb." It successfully solved the problem that I wrote it for, and it 
became a good exercise in parsing, transforming, and passing data between functions
in Haskell. -}


{--- EXAMPLE -}

arr :: [Int]
arr = [10, 2, 14, 7, 8, 12, 15, 11, 0, 4, 1, 13, 3, 9, 6, 5]

{- Using the hardcoded array arr as a reference, if arr and 10
are the starting arguments, then the algorithm will access
arr !! 10 and read the value 1. It will then access arr !! 1,
read the value 2, access arr !! 2, read value 14, and so on
until it has executed n times. -}



{--- HOW TO USE

For a direct solution:

    ghci> defuse arr
    [(5,115)]

arr can be replaced with any user-defined list that meets 
the conditions described in the problem definition.

All other helper functions can be run independently to see 
what the algorithm is doing at any given point.

The general forms for calling most functions here:

    foo xs
    foo xs i

All functions with 'loop' in the name require a list as an
argument. Loop functions with the suffix 'i' require at least 
one additional argument that is an integer. -}



{- Produces a list of tuples where each tuple is a valid
set of two numbers that will defuse phase 5 of the bomb. 
Returns empty list if no valid numbers are found. -}
defusePhase5 :: [Int] -> [(Int, Int)]
defusePhase5 xs = go (lprGL xs) 0
    where
        go [] i = []
        go ((x, y, z):xs) i
            | x == z    = (i, (lprS arr i)) : go xs (i + 1)
            | otherwise = go xs (i + 1)
defuse = defusePhase5
-- ghci> defusePhase5 arr
-- [(5,115)]



{----
 ----      HELPER FUNCTIONS
 ----}



{- Executes a single step into the array. If loopi is used in iso-
lation then the value of n, the iteration counter, is irrelevant.
Outputs a tuple that labels the iteration counter, current index,
and the value at that index that will be used for the next iteration. -} 
loopi :: [Int] -> Int -> Int -> (String, Int, String, Int, String, Int)
loopi xs n i = ("loop", (n + 1), "ind", i, "val", (xs !! i))
-- ghci> loopi arr 0 3
-- ("loop",1,"ind",3,"val",7)



{- Produces a list of tuples that are the result of running 
loopi repeatedly based on a given list and starting index. 
Runs until the number of iterations matches the largest 
value in the given list. -}
loopr :: [Int] -> Int -> [(String, Int, String, Int, String, Int)]
loopr [] _ = []
loopr xs i = go mx i
    where
        mx = maximum xs
        go 0 _ = []
        go n i = (loopi xs (mx - n) i) : (go (n - 1) (xs !! i))
{- ghci> loopr arr 0
[("loop",1,"ind",0,"val",10),("loop",2,"ind",10,"val",1),("loop",3,"ind",1,"val",2),("loop",4,"ind",2,"val",14),("loop",5,"ind",14,"val",6),("loop",6,"ind",6,"val",15),("loop",7,"ind",15,"val",5),("loop",8,"ind",5,"val",12),("loop",9,"ind",12,"val",3),("loop",10,"ind",3,"val",7),("loop",11,"ind",7,"val",11),("loop",12,"ind",11,"val",13),("loop",13,"ind",13,"val",9),("loop",14,"ind",9,"val",4),("loop",15,"ind",4,"val",8)] -}

    
    
{- Runs loopr n times, where n is the maximum value in the list.
Generates every permutation of loopr xs i, where i is each value
0..n. -}
looprList :: [Int] -> [[(String, Int, String, Int, String, Int)]]
looprList xs = go 0
    where 
        n = maximum xs
        go i
            | i < n     = (loopr xs i) : go (i + 1)
            | otherwise = []
lprL = looprList
{- ghci> looprList arr -}



{- Isolates the last tuple from each list of tuples within 
looprList's output. Then it extracts the integer values
from each of those tuples, and outputs them as a list of
tuples. -}  
looprGetLasts :: [Int] -> [(Int, Int, Int)]
looprGetLasts xs = map extr $ map last $ lprL xs
lprGL = looprGetLasts
{- ghci> looprGetLasts arr
[(15,4,8),(15,0,10),(15,10,1),(15,5,12),(15,13,9),(15,6,15),(15,2,14),(15,12,3),(15,9,4),(15,11,13),(15,8,0),(15,3,7),(15,15,5),(15,7,11),(15,1,2)] -}



{- Produces a list of the values visited during one 
iteration of loopr, in the order that they were visited. -}
looprLastsi :: [Int] -> Int -> [Int]
looprLastsi xs i = dblExtr $ loopr xs i
    where dblExtr ys = map (\(x, y, z) -> z) $ map extr ys
lprLi = looprLastsi
{- ghci> looprLastsi arr 8
[11,13,9,4,8,0,10,1,2,14,6,15,5,12,3] -}



-- Sums all of the values visited during one iteration of loopr.
looprSumi :: [Int] -> Int -> Int
looprSumi xs i = sum $ lprLi xs i
lprS = looprSumi
{- ghci> looprSumi arr 13
   107 -}



-- Extracts the integer values from the output of loopi.
extractor :: (String, Int, String, Int, String, Int) -> (Int, Int, Int)
extractor (xs, x, ys, y, zs, z) = (x, y, z)
extr = extractor
{- ghci> extractor (loopi arr 0 5)
   (1, 5, 12) -}