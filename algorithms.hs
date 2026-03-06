--------
--------             DATA STRUCTURES AND ALGORITHMS
--------                  CS-361L SPRING 2026
--------
import Data.List


{-- CS-361 HW 2.1 - TOWERS OF HANOI -}


{- Largest accepted value is 20. Runtimes start to become unreasonable when n > 20.
 - Returns ([404],[404],[404], 0) if n < 1 or n > 20.
 - The first three elements of the output tuple are lists that represent the three rods.
 - The last element of the output tuple is an integer that is the number of recursive
   calls it took to solve the puzzle.
 - Performs the first move operation in the initial call to towers. -}
hanoi :: Int -> ([Int],[Int],[Int], Int)
hanoi n
    | n == 2          = ([], [], [1,2], 3)
    | n < 1 || n > 20 = ([404],[404],[404], 0)
    | otherwise       = towers ([2..n] ++ [0]) [1,0] [0] 1



{- Explicitly simulates the decision-making and movements required to solve the puzzle.
 - Counts iterations and changes behavior depending on whether the count is even or odd. -}
towers :: [Int] -> [Int] -> [Int] -> Int -> ([Int],[Int],[Int], Int)
towers [0] tB [0] k = ([], init tB, [], k)
towers [0] [0] tC k = ([], [], init tC, k)
towers tA@(a:popA) tB@(b:popB) tC@(c:popC) k
    | (k `mod` 2) > 0 = oddOp
    | otherwise       = evenOp
    where
        kpp = k + 1
        oddOp = moveTop tA tB tC kpp
        evenOp
            | a == 1 = towers popA (a:tB) tC kpp
            | b == 1 = towers tA popB (b:tC) kpp
            | c == 1 = towers (c:tA) tB popC kpp



{- Called when the iteration count is odd.
 - Determines where the top/smallest element should go next. -}
moveTop :: [Int] -> [Int] -> [Int] -> Int -> ([Int],[Int],[Int], Int)
moveTop tA@(a:popA) tB@(b:popB) tC@(c:popC) k
    | a == 1 = bAndC
    | b == 1 = aAndC
    | c == 1 = aAndB
    where
        secondSmall i j = minThree i j 0
        bAndC
            | (secondSmall b c) == b = towers tA popB (b:tC) k
            | otherwise              = towers tA (c:tB) popC k
        aAndC
            | (secondSmall c a) == c = towers (c:tA) tB popC k
            | otherwise              = towers popA tB (a:tC) k
        aAndB
            | (secondSmall a b) == a = towers popA (a:tB) tC k
            | otherwise              = towers (b:tA) popB tC k
    
    

-- Helper that determines the minimum of three integers.
minThree :: Int -> Int -> Int -> Int
minThree x 0 0 = x
minThree 0 y 0 = y
minThree 0 0 z = z
minThree x y 0 = min x y
minThree x 0 z = min x z  
minThree 0 y z = min y z
minThree x y z = min x $ min y z





{--- CS-361 HW 1.8 - MAXIMUM AVERAGE OF A SUBARRAY
---- Find the maximum average value of a contiguous sub-array of length k. -}

-- INCOMPLETE - this implementation cheats because it assumes a sorted list.
-- This is more of an exercise in currying and composition than a real solution.
maxContAvg :: [Int] -> Int -> Int
maxContAvg [] _ = 0
maxContAvg xs k = divk . sum . flipDrop . subk . length $ xs
    where
        divk     = (`div` k)
        flipDrop = \x -> flip drop xs x
        subk     = subtract k
  
-- maxContAvg xs k = (div) (sum $ drop (myLength xs - k) xs) k

{-

merge :: Ord a => [a] -> [a] -> [a]


mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = merge (mergeSort left) (mergeSort right)

-}





{--- CS-361 HW 2.4 - TWO-STACK QUEUE
---- Implement a queue using two stacks. 

-}


