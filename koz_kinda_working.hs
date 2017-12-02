


import Data.List

-- assume a solution exists
zed conds@(top,_,_,_) = 
  let n = (length top)
  in head (filter (isBoardValid conds) (genBoards n n []))


isBoardValid (top,right,bottom,left) board = topIsValid && rightIsValid && bottomIsValid && leftIsValid 
    where
        ns = [0..(length top)-1]
        topIsValid    = (and [dirValid (top!!n) (getCol n board)                        | n <- ns])
        rightIsValid  = (and [dirValid (right!!n) (reverse (board!!n))                  | n <- ns])
        bottomIsValid = (and [dirValid ((reverse bottom)!!n) (reverse (getCol n board)) | n <- ns])
        leftIsValid   = (and [dirValid ((reverse left)!!n) (board!!n)                   | n <- ns])

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


-- generates a list of possible n x n boards, partially pruned
genBoards n 0 acc = acc
genBoards n c [] = genBoards n (c-1) (genChildBoards n [])
genBoards n c acc = 
    foldr (++) [] (map (genChildBoards n) (genBoards n (c-1) acc))


genChildBoards n board = 
    [x:board | x <- permutations [1..n], not (x `elem` board)]


-- !!! add in type declaration?
--assume valid board, valid n
--end case
getCol n [] = []

--non empty board
getCol n board = 
    ((head(board))!!n):(getCol n (tail(board)))





