


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

dirValid cond [x,y]
    | x == y = False
    | cond > 2 = False
    | cond == 1 = x > y
    | otherwise = x < y 

dirValid cond lst@(x:y:xs)
    | x == y = False
    | cond == 1 = (x == length lst)
    | x < y = dirValid (cond-1) (y:xs)
    | x > y = dirValid cond (x:xs)


genBoards n 0 acc = acc
genBoards n c [] = genBoards n (c-1) (genChildBoards n [])
genBoards n c acc = 
    foldr (++) [] (map (genChildBoards n) (genBoards n (c-1) acc))


genChildBoards n board = 
    [x:board | x <- permutations [1..n], not (x `elem` board)]

-- genBoards = [[r1, r2, r3,r4] | r1 <- n, 
--                                r2 <- (delete r1 n), 
--                                r3 <- (delete r1 (delete r2 n)),
--                                r4 <- (delete r1 (delete r2 (delete r3 n))),
--                                (r1!!0) /= (r2!!0),
--                                (r1!!0) /= (r3!!0),
--                                (r1!!0) /= (r4!!0),
--                                (r2!!0) /= (r3!!0),
--                                (r2!!0) /= (r4!!0),
--                                (r3!!0) /= (r4!!0),

--                                (r1!!1) /= (r2!!1),
--                                (r1!!1) /= (r3!!1),
--                                (r1!!1) /= (r4!!1),
--                                (r2!!1) /= (r3!!1),
--                                (r2!!1) /= (r4!!1),
--                                (r3!!1) /= (r4!!1),

--                                (r1!!2) /= (r2!!2),
--                                (r1!!2) /= (r3!!2),
--                                (r1!!2) /= (r4!!2),
--                                (r2!!2) /= (r3!!2),
--                                (r2!!2) /= (r4!!2),
--                                (r3!!2) /= (r4!!2)
--                                ]
--     where n = permutations [1..4]

-- !!! add in type declaration?
--assume valid board, valid n
--end case
getCol n [] = []

--non empty board
getCol n board = 
    ((head(board))!!n):(getCol n (tail(board)))





