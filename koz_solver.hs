
import Data.List
import Data.Maybe

pmt xs = [x:ys | x <- xs, ys <- pmt (delete x xs)]

{-

Kingdom of Zed solver

example input: 
    zed ([2,2,1],[1,2,2],[3,1,2],[2,1,3])

solution:
    [[1,2,3],[3,1,2],[2,3,1]]

diagram of above puzzle:

    | 2 | 2 | 1 |
 ___|___|___|___|___
  3 | 1 | 2 | 3 | 1 
 ___|___|___|___|___ 
  1 | 3 | 1 | 2 | 2 
 ___|___|___|___|___ 
  2 | 2 | 3 | 1 | 2 
 ___|___|___|___|___ 
    | 2 | 1 | 3 | 
    |   |   |   |



In general, it will take the form:

example input: 
    zed ([a,b,c],[d,e,f],[g,h,i],[j,k,l])

solution:
    [[1,2,3],[4,5,6],[7,8,9]]

diagram of above puzzle:

    | a | b | c |
 ___|___|___|___|___
  l | 1 | 2 | 3 | d 
 ___|___|___|___|___ 
  k | 4 | 5 | 6 | e 
 ___|___|___|___|___ 
  j | 7 | 8 | 9 | f 
 ___|___|___|___|___ 
    | i | h | g | 
    |   |   |   |



-}

{-
* This is the primary function - takes a tuple of all 4 sides and generates a solution.
* for now, assume that a solution will exist, will make code more robust after.
-}

zed conds@(top,_,_,_) = zedHelper conds initBoard
    where
      n = length top
      row = take n (repeat 0)
      initBoard = take n (repeat row)

-- zedHelper :: Integral a => (a,a,a,a) -> [[a]] -> [[a]]
zedHelper conds board
  | null board = []
  | isBoardSolved conds board = board
  | otherwise = nextBoardsSolver conds (getNextBoards board)

-- nextBoardsSolver :: Integral a => (a,a,a,a) -> [[a]] -> [[a]]
nextBoardsSolver conds boards
  | null boards = []
  | otherwise = 
    let x = (zedHelper conds (head boards))
    in
      if (not (null x))
      then x
      else (nextBoardsSolver conds (tail boards))

-------------------------------------------------
-------------------------------------------------

{-
 - Helper to see if a board is solved
 - First checks if the board is full, then checks if it is valid
 -}

isBoardSolved conds board = 
    if (isFull board) 
    then (isBoardValid conds board)
    else False

-------------------------------------------------
-------------------------------------------------

{-
 - Helper to see if a board is valid
 -}
isBoardValid (top,right,bottom,left) board = topIsValid && rightIsValid && bottomIsValid && leftIsValid 
    where
        ns = [0..(length top)-1]
        cols = getAllCols board
        topIsValid    = (and [(dirValid (top!!n) (cols!!n))                          | n <- ns])
        rightIsValid  = (and [dirValid (right!!n) (reverse (board!!n))               | n <- ns])
        bottomIsValid = (and [(dirValid ((reverse bottom) !! n) (reverse (cols!!n))) | n <- ns])
        leftIsValid   = (and [dirValid ((reverse left)!!n) (board!!n)                | n <- ns])


-------------------------------------------------
-------------------------------------------------

{-
* Helper to see if the board is full.
* This function checks whether any entry is 0 .
* 0 is not a valid entry in KoZ, and can be used as a placeholder for a 
   spot on the board that hasn't been filled yet.
* The initial board will be completely populated with zeroes.
-}
isFull :: Integral a => [[a]] -> Bool
isFull [] = True
isFull (x:xs) = (isFullHelper x) && (isFull xs)


{-
* Helper function for isFull.
* Checks if a single row is full - a row is full if every element /= 0 .
-}
isFullHelper :: Integral a => [a] -> Bool
isFullHelper lst = foldr (\ e r -> e /= 0 && r) True lst 

-------------------------------------------------
-------------------------------------------------

{-
* Check if a single row/col is valid from one direction - use reverse on 
  the row/col to check the other direction.
-}
dirValid :: Integral a => a -> [a] -> Bool

dirValid _ [] = True
dirValid n [x,y]
  | x == y = False -- boards with two of the same element are invalid
  | n > 2  = False
  | n == 1 = x > y
  | otherwise = x < y

dirValid n lst@(x:y:xs)
  | hasDuplicates lst = False
  | n == 1 = (x > y) && (dirValid 1 (x:xs))
  | x < y = dirValid (n - 1) (y:xs)
  | x > y = dirValid n (x:xs)

{-
* helper to check if a list has duplicates
-}
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates []  = False
hasDuplicates [_] = False
hasDuplicates (x:xs) = (x `elem` xs) || (hasDuplicates xs)



-------------------------------------------------
-------------------------------------------------
{-
 - Helper to get a list of all columns
 -}
getAllCols lst = [(getCol x lst) | x <- [0..((length lst)-1)]]

-------------------------------------------------
-------------------------------------------------

{-
* Constructs a list of the elements in a given column from a board, is 0 indexed
* e.g. with board = [[1,2,3],[4,5,6],[7,8,9]], which corresponds to the board:
     


  Col num:      0   1   2 

              | a | b | c |
           ___|___|___|___|___
            l | 1 | 2 | 3 | d 
           ___|___|___|___|___ 
            k | 4 | 5 | 6 | e 
           ___|___|___|___|___ 
            j | 7 | 8 | 9 | f 
           ___|___|___|___|___ 
              | i | h | g | 
              |   |   |   |

    
    (getCol n board) should return the nth column as a list.

    so (getCol 1 board) returns [2,5,8]

-}
-- getCol n board = [Int]

-- !!! add in type declaration?
--assume valid board, valid n
--end case
getCol n [] = []

--non empty board
getCol n board = 
    ((head(board))!!n):(getCol n (tail(board)))




-------------------------------------------------
-------------------------------------------------

{-
* In scanwise order constructs a list of all the next possible board combinations.
* May either prune for valid boards in this function, or later on.

e.g. calling (nextBoards [[0,0,0], [0,0,0], [0,0,0]])
     which corresponds to the initial 3x3 board, should return:
  [
     [[1,0,0], [0,0,0], [0,0,0]]
     [[2,0,0], [0,0,0], [0,0,0]]
     [[3,0,0], [0,0,0], [0,0,0]]
  ]

  and calling on a partially completed board: (nextBoards [[3,2,1], [2,1,0], [0,0,0]])

  [
    [[3,2,1], [2,1,1], [0,0,0]] <- note that this board is not valid, but it is 
    [[3,2,1], [2,1,2], [0,0,0]]    generated regardless, and should be pruned later by dirValid
    [[3,2,1], [2,1,3], [0,0,0]] 
  ]

-}
-- nextBoards [row 1, row 2,...,row n] = [board 1, board 2,...,board n]

--to do in this function
--1. find the row with the first zero
--2. find the element at which this zero occurs
--3. make a list of all possible alterations to the first zero

--need a helper function to tell if there's a zero in a row

--helper that checks if there is 0 in a list
--should include a type declaration!!!
containsZero lst = 0 `elem` lst

-- helper so n does not have to be passed every time
getNextBoards board = nextBoards (length (board!!0)) board

--takes:
--n: max value
--board: an incomplete list of lists, containing at least one 0 inside the lists
--there are checks for boards without zeros or incorrect n values, but not for empty boards or faulty boards
nextBoards n board = (nextBoardsHelper n board (findFirstZeroListIndx(board)) (retList(board)) 1)
--n: max value
--board: board to be modified
--listIndx: the list element with the first zero
--elemIndx: the element of list elem with the first zero
--acc: what value we are switching into the first zero, start at 1, go to n
--add a board with acc, stopping after acc = n
nextBoardsHelper n board listIndx elemIndx acc
   | (n < 1 || listIndx < 0 || elemIndx < 0 || acc < 1) = [[]]
   | acc == n = (elemQtoXList listIndx elemIndx acc board):[]
   | otherwise = (elemQtoXList listIndx elemIndx acc board):(nextBoardsHelper n board listIndx elemIndx (acc+1))

--find the index of in the first zero in the the first list a zero appears in
--so if [[1,2], [0,4]], return 0
--if there is no 0, return -1
--this function is basically useless outside of nextBoards... it is used so the function does not fail if findFirstZeroListIndx returns -1
--wouldnt compile without this.
--bad name, rename to something smarter if youd like
retList [] = 0
retList board
   | findFirstZeroListIndx(board) == -1 = 0
   | otherwise = findFirstZero(board!!(findFirstZeroListIndx(board)))

--find first list in listoflist with zero, returns indx of said list
findFirstZeroListIndx [] = -1
findFirstZeroListIndx listoflist = (findFirstZeroListIndxHelper listoflist 0)
findFirstZeroListIndxHelper [] idx = -1
findFirstZeroListIndxHelper listoflist idx
   | (containsZero (head listoflist)) = idx
   | otherwise = findFirstZeroListIndxHelper (tail listoflist) (idx+1)


--finds the index of the first zero in the list, -1 if no zero
findFirstZero [] = -1
findFirstZero list = findFirstZeroHelper list 0
findFirstZeroHelper [] idx = -1
findFirstZeroHelper list idx
   | (head list) == 0 = idx
   | otherwise = (findFirstZeroHelper (tail list) (idx+1))


