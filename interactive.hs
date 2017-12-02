
--interactive version of project

--start by creating main menu
main = do
    putStrLn $ "Hi, welcome to the program"
    putStrLn $ "There are three options currently."
    putStrLn $ "1. Play a game of KOZ"
    putStrLn $ "2. Listen to various barking noises"
    putStrLn $ "3. Exit"
    choice <- getLine
    if(choice == "1") then startup else (putStrLn $ "woof! bark! ruff!")



startup = do
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
   putStrLn $ show nClue

   putStrLn $ "Enter the second clue, for the east side"
   eClueUP <- getLine
   let eClue = readNumbers eClueUP
   putStrLn $ show eClue

   putStrLn $ "Enter the third clue, for the south side"
   sClueUP <- getLine
   let sClue = readNumbers sClueUP
   putStrLn $ show sClue

   putStrLn $ "Enter the fourth clue, for the west side"
   wClueUP <- getLine
   let wClue = readNumbers wClueUP
   putStrLn $ show wClue

   let clueList = [nClue] ++ [eClue] ++ [sClue] ++ [wClue]

   if (not (validityCheck n clueList)) then (return ()) else do
    (playGame n clueList (generateEmptyBoard n))

playGame n clueList zed = do
    putStrLn $ (printMap clueList zed)
    putStrLn $ "What cell would you like to modify?"
    putStrLn $ "Note: the cell position is based on 0 based indexing, in format 'r c'"
    putStrLn $ "If you would like the top, left-most cell the address is '0 0'"
    address <- getLine
    let row = (head (readNumbers address))
    let col = (last (readNumbers address))
    putStrLn $ (show row)
    putStrLn $ (show col)
    if(not (validAddress row col n)) then (do
        putStrLn $ "Invalid Address."
        playGame n clueList zed) else do
    putStrLn $ "What value would you like to change it to?"
    putStrLn $ "Note: the value must be between 1 and n"
    value <- getLine
    let val = head (readNumbers value)
    if(val < 1 || val > n) then (do
        putStrLn $ "Invalid Value."
        playGame n clueList zed)
        else do
    let newZed = changeCell row col zed val
    putStrLn $ (printMap clueList newZed)
    if(not (checkFilled newZed)) then (do
        putStrLn $ "Your point has been modified."
        playGame n clueList newZed)
        else do
    if(checkValid n clueList newZed) then (do
        putStrLn $ "You have reached a valid solution!  Congratulations!"
        putStrLn $ "Add a function that gives you to print the solution three different ways")
        else do
        putStrLn $ "Unfortunately, this is not a valid solution."
        putStrLn $ "Try changing up some of your values."
        playGame n clueList newZed

checkFilled board = False

--essentially just a renamed version of elemQtoXList but I thought the specific name
--made the program more readable
changeCell row col board value = elemQtoXList row col value board 



checkValid n clueList zed = True

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
printMap clues zed = "   " ++ (intListToStr (head clues)) ++ "\n"
 ++ (middleSectionStr clues zed) ++ "   " ++ (reverse (intListToStr (clues!!2))) ++ "\n"

 --go until zed is []
middleSectionStr clues [] = ""
middleSectionStr clues zed = middleSectionStrh ([(reverse (last clues))] ++ [(head (tail clues))]) zed
middleSectionStrh clues [] = ""
middleSectionStrh clues zed = (intSB (head (head clues))) ++ (intListToStr (head zed)) ++ (reverse (intSB (head (last clues))))
 ++ "\n" ++ (middleSectionStrh (rmvHeadClues clues) (tail zed))




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