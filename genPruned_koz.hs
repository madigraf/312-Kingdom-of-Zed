import Data.List

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

input: 
    zed ([a,b,c],[d,e,f],[g,h,i],[j,k,l])

solution:
    [[1,2,3],[4,5,6],[7,8,9]]


 -}

---------------------------------------------------------------------
---------------------------------------------------------------------
-- PRIMARY FUNCTION -------------------------------------------------

{-
 - This is the primary function - takes a tuple of all 4 sides and generates a solution.
 - If a solution does not exist, this function will throw an exception.
 -}
zed conds@(top,_,_,_) = 
  let n = (length top)
  in head (filter (isBoardValid conds) (genPrunedBoards n))



---------------------------------------------------------------------
---------------------------------------------------------------------
-- FUNCTIONS TO CHECK IF A BOARD IS A SOLUTION ----------------------

{-
 - Helper to check if a given, filled board is a valid solution
 -}
isBoardValid :: Integral a => ([a],[a],[a],[a]) -> [[a]] -> Bool
isBoardValid (top,right,bottom,left) board = topIsValid && rightIsValid && bottomIsValid && leftIsValid 
    where
        ns = [0..(length top)-1]
        topIsValid    = (and [dirValid (top!!n) (getCol n board)                        | n <- ns])
        rightIsValid  = (and [dirValid (right!!n) (reverse (board!!n))                  | n <- ns])
        bottomIsValid = (and [dirValid ((reverse bottom)!!n) (reverse (getCol n board)) | n <- ns])
        leftIsValid   = (and [dirValid ((reverse left)!!n) (board!!n)                   | n <- ns])


{-
 - Given a condition and a list, it checks whether the puzzle property is true for that list.
 -
 - E.g. dirValid 2 [3,2,1,4] is true, because only 2 squares can be "seen" from the list 
 - in this direction.
 - 
 - E.g. dirValid 3 [4,1,2,3] is false, because only 1 square can be seen in this direction.
 -}
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



---------------------------------------------------------------------
---------------------------------------------------------------------
-- FUNCTIONS TO GENERATE A PRUNED LIST OF N x N BOARDS---------------

{-
 - Helper to generate the pruned list of all valid boards of size n x n
 -}
genPrunedBoards :: Integral a => a -> [[[a]]] -- note: a single board has type [[a]]
genPrunedBoards n = pruneCols (genBoards n n [])

{-
 - Helper to getPrunedBoards - filters a list for only boards with valid columns.
 -}
pruneCols :: Integral a => [[[a]]] -> [[[a]]]
pruneCols boards = 
  filter noDupCols boards

{-
 - Helper to pruneCols - returns true if all columns of a board are valid
 
 e.g. this board:

              | a | b | c |
           ___|___|___|___|___
            l | 1 | 2 | 3 | d 
           ___|___|___|___|___ 
            k | 2 | 3 | 1 | e 
           ___|___|___|___|___ 
            j | 3 | 1 | 2 | f 
           ___|___|___|___|___ 
              | i | h | g | 
              |   |   |   |

    is valid, since a number is not repeated in a column, whereas:

              | a | b | c |
           ___|___|___|___|___
            l | 1 | 2 | 3 | d 
           ___|___|___|___|___ 
            k | 2 | 3 | 1 | e 
           ___|___|___|___|___ 
            j | 1 | 3 | 2 | f 
           ___|___|___|___|___ 
              | i | h | g | 
              |   |   |   |

    is not, since 3 is repeated in column b and 1 is repeated in column a.
 -}
noDupCols :: Integral a => [[a]] -> Bool
noDupCols board = 
  not (or (map hasDuplicates (getAllCols board)))


{-
 - generates a list of possible n x n boards, completely pruned row-wise but
 - only partially pruned column-wise.
 -}
genBoards :: Integral a => a -> a -> [[[a]]] -> [[[a]]]
genBoards n 0 acc = acc
genBoards n c [] = genBoards n (c-1) (genChildBoards n [])
genBoards n c acc = 
    foldr (++) [] (map (genChildBoards n) (genBoards n (c-1) acc))

{-
 - Helper to genBoards, generates a list of all child boards for a given board,
 - partially pruned.
 -}
genChildBoards :: Integral a => a -> [[a]] -> [[[a]]]
genChildBoards n board = 
    [x:board | x <- permutations [1..n], not (x `elem` board)]



---------------------------------------------------------------------
---------------------------------------------------------------------
-- MISC HELPER FUNCTIONS --------------------------------------------

{-
 - helper to check if a list has duplicates
 -}
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates []  = False
hasDuplicates (x:xs) = (x `elem` xs) || (hasDuplicates xs)


{-
 - Helper to get a list of all columns from a given board
 -}
getAllCols board = [(getCol n board) | n <- [0..((length board)-1)]]


{-
 - Helper to get a single column from a board as a list of int
 -}
getCol n [] = []
getCol n board = 
    ((head(board))!!n):(getCol n (tail(board)))


---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
----dear god, make this a module
--interactive version of project

--start by creating main menu
main = do
    putStrLn $ "Hi, welcome to the Kingdom of Zed Game and Solver"
    putStrLn $ "There are three options."
    putStrLn $ "1. Play a game of KOZ"
    putStrLn $ "2. Give a set of clues, and have the solver find the solution for you"
    putStrLn $ "3. Exit"
    choice <- getLine
    if(choice == "1" || choice == "2") then (startup choice)  else (putStrLn $ "Exiting...")



startup choice = do
   putStrLn "Please enter n, the max value.  n should be at least 2"
   p <- getLine
   let n = head (readNumbers p)
   if(n < 2) then (return ()) else do
   --convert n to a number
   putStrLn $ "Your max value is " ++ (show n)
   putStrLn $ "Enter the given clues now, in the expected format."
   putStrLn $ "Enter each value with a space between them, ie for a n=3 board: 1 2 3"
   putStrLn $ "Enter the first clue, for the north side"
   nClueUP <- getLine
   let nClue = readNumbers nClueUP

   putStrLn $ "Enter the second clue, for the east side"
   eClueUP <- getLine
   let eClue = readNumbers eClueUP

   putStrLn $ "Enter the third clue, for the south side"
   sClueUP <- getLine
   let sClue = readNumbers sClueUP

   putStrLn $ "Enter the fourth clue, for the west side"
   wClueUP <- getLine
   let wClue = readNumbers wClueUP

   let clueList = [nClue] ++ [eClue] ++ [sClue] ++ [wClue]
   if(not (validityCheck n clueList)) then (return ())
        else do
            if(choice == "1") then (do
                playGame n clueList (generateEmptyBoard n))
                else (do 
                    let soln = zed (listToTuple clueList)
                    putStrLn $ (printMap clueList soln)                    
                    )



 {-}  if (not (validityCheck n clueList)) then (return ()) else (do
    if (choice == "1") then (do playGame n clueList (generateEmptyBoard n)) 
    else (do (zed (listToTuple clueList)))   -}

playGame n clueList board = do
    putStrLn $ (printMap clueList board)
    putStrLn $ "What cell would you like to modify?"
    putStrLn $ "Note: the cell position is based on 0 based indexing, in format 'r c'"
    putStrLn $ "If you would like the top, left-most cell the address is '0 0'"
    address <- getLine
    let row = (head (readNumbers address))
    let col = (last (readNumbers address))
    if(not (validAddress row col n)) then (do
        putStrLn $ "Invalid Address."
        playGame n clueList board) else do
    putStrLn $ "What value would you like to change it to?"
    putStrLn $ "Note: the value must be between 1 and n"
    value <- getLine
    let val = head (readNumbers value)
    if(val < 1 || val > n) then (do
        putStrLn $ "Invalid Value."
        playGame n clueList board)
        else do
    let newBoard = changeCell row col board val
    if(not (isFull newBoard)) then (do
        putStrLn $ "Your point has been modified."
        playGame n clueList newBoard)
        else do
    if(isBoardValid (listToTuple clueList) newBoard) then (do
        putStrLn $ "You have reached a valid solution!  Congratulations!"
        putStrLn $ "Add a function that gives you to print the solution three different ways")
        else do
        putStrLn $ "Unfortunately, this is not a valid solution."
        putStrLn $ "Try changing up some of your values."
        playGame n clueList newBoard

--isBoardValid
--essentially just a renamed version of elemQtoXList but I thought the specific name
--made the program more readable
changeCell row col board value = elemQtoXList row col value board 

listToTuple [a,b,c,d] = (a,b,c,d)


checkValid n clueList board = True

--checks the validity of an address
validAddress row col n = if ((row >= 0) && (row < n) && (col >= 0) && (col < n)) then True else False
   
--    if ((length firstClue) /= n) then putStrLn "fuck." else putStr "nisu."

rInt :: String -> Int
rInt = read

generateEmptyBoard n = generateEmptyBoardH n n
generateEmptyBoardH n 0 = []
generateEmptyBoardH n acc = (generateEmptyList n):(generateEmptyBoardH n (acc-1))

generateEmptyList 0 = []
generateEmptyList n = 0:(generateEmptyList (n-1))

readNumbers str = map rInt (words str)

--check for correct number n and each value is 1-n
validityCheck n lol = (correctNoVal n lol) && (correctValRangeLOL n lol)

--check to make sure the
correctNoVal n [] = True
correctNoVal n lol
    | (length (head lol)) == n = correctNoVal n (tail lol)
    | otherwise = False

correctValRangeLOL n [] = True
correctValRangeLOL n lol
    | correctValRange n (head lol) = correctValRangeLOL n (tail lol)
    | otherwise = False
--check to make sure each value in a list is between 1 and n
correctValRange n [] = True
correctValRange n list
    | (head list) < 1 = False
    | (head list) > n = False
    | otherwise = correctValRange n (tail list)






--function: input graph, output picture to screen
--min graph is n =1

--clues: 
--zedToScreen = do
  
--the way things are printed:
--spaces with top clue then spaces
--1st piece of left clue, a space and a | then print row1 of the map then 1st piece of right clue
--do this until the end of n
--then add the last clue to the bottom


--map to output string
--mtos clues zedMap =
--  topline ++ (printRows clues zedMap) ++ bottomline

--make a list of ints into a list of strings

{- 
   A B C   
  -------
L |A B C| D
K |D E F| E
J |G H I| F
  -------
   I H G
-}

fn1 clues = [(reverse (last clues))] ++ [(head (tail clues))]
rmvHeadClues clues = [(tail (head clues))] ++ [(tail (last clues))]



--below prints a board2
printMap clues board = "   " ++ (intListToStr (head clues)) ++ "\n"
 ++ (middleSectionStr clues board) ++ "   " ++ (reverse (intListToStr (clues!!2))) ++ "\n"

 --go until board is []
middleSectionStr clues [] = ""
middleSectionStr clues board = middleSectionStrh ([(reverse (last clues))] ++ [(head (tail clues))]) board
middleSectionStrh clues [] = ""
middleSectionStrh clues board = (intSB (head (head clues))) ++ (intListToStr (head board)) ++ (reverse (intSB (head (last clues))))
 ++ "\n" ++ (middleSectionStrh (rmvHeadClues clues) (tail board))




--int list = map (\x -> (show x) ++ " ")

--printElements :: [String] -> IO ()
--printElements = mapM_ (\x -> putStr (x++" "))  

--remove last character in a string
removeLast str = init str

--remove the last character of the last string in a string list
removeLastList strls = (init strls)  ++[(removeLast (last strls))]

intToStrL intList = (removeLastList (map (\x -> ((show x) ++ " ")) intList))


--go till the list is empty

--i wanna write a function that takes a list of strings and combines it to one string
--to do this, we must add all the characters in the reverse order 
cmbin lofst = cmbinh lofst []
cmbinh [] acc = acc
cmbinh lofst acc = cmbinh (init lofst) ((last lofst) ++ acc)



intListToStr intList = cmbin (intToStrL intList)

--take a string and append " |" to it
intSB inte = appendSB (show inte)
appendSB str = str ++ " |"


--return modified list, changing element q to value x
--0 indexing
elemQtoX q x [] = []
elemQtoX q x list = elemQtoXHelper q x 0 list
elemQtoXHelper q x acc [] = []
elemQtoXHelper q x acc list
   | q == acc = x:(tail list)
   | otherwise = (head list):(elemQtoXHelper q x (acc+1) (tail list))

--return modified list of lists, changing specified list-element (p) indx q to be x
elemQtoXList p q x listoflist = (elemQtoXListHelper p q x listoflist 0)
elemQtoXListHelper p q x listoflist acc
   | (p == acc) = (elemQtoX q x (head listoflist)):(tail listoflist)
   | otherwise = (head listoflist):(elemQtoXListHelper p q x (tail listoflist) (acc+1))





-----pulled from koz_solver.hs

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