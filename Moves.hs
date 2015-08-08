-- Moves.hs

-- Queen's moves

-- {-# OPTIONS_GHC -Wall #-}

module Moves (
    moves,     -- Int -> Pos -> S.Set Pos
    rowMoves   -- Int -> Pos -> [Pos]
) where

-- import Debug.Trace (trace)

import qualified Data.Set as S (Set, fromList, toList)
import Board (
    Board,             -- :: [Row]
    Pos,               -- :: (Int, Int)
    fromList,          -- :: Int -> [Pos] -> Board
    placeE,            -- :: Pos -> Board -> Board
    placeQ,            -- :: Pos -> Board -> Board
    placeX,            -- :: Pos -> Board -> Board
    putBoard,          -- :: Board -> IO ()
    putBoardFromList,  -- :: Int -> [Pos] -> IO ()
    showBoard          -- :: Board -> String
  )

{- Starting from a location on a board, where can a queen move?
     Along the same row.
     Along the same column.
     Along both diagonals.
-}

----------------------------------------
-- All moves from one position
----------------------------------------

moves :: Int -> Pos -> S.Set Pos
moves size p = S.fromList $ rowMoves  size p
                         ++ colMoves  size p
                         ++ diagMoves size p

showMoves :: Int -> Pos -> String
showMoves size p =
  showBoard . placeX p . fromList size . S.toList $ moves size p

putMoves :: Int -> Pos -> IO ()
putMoves size p = putStr $ showMoves size p

----------------------------------------
-- Row moves
----------------------------------------

rowMoves :: Int -> Pos -> [Pos]
rowMoves size (curRow, curCol) = zip (repeat curRow) [0 .. maxIdx]
  where
    maxIdx = size - 1

showRowMoves :: Int -> Pos -> String
showRowMoves size p =
  showBoard . placeX p . fromList size $ rowMoves size p

putRowMoves :: Int -> Pos -> IO ()
putRowMoves size p = putStr $ showRowMoves size p

----------------------------------------
-- Column moves
----------------------------------------

colMoves :: Int -> Pos -> [Pos]
colMoves size (curRow, curCol) = zip [0 .. maxIdx] (repeat curCol)
  where
    maxIdx = size - 1

showColMoves :: Int -> Pos -> String
showColMoves size p =
  showBoard . placeX p . fromList size $ colMoves size p

putColMoves :: Int -> Pos -> IO ()
putColMoves size p = putStr $ showColMoves size p

----------------------------------------
-- Diagonal moves
----------------------------------------

diagMoves :: Int -> Pos -> [Pos] 
diagMoves size p = diag1 size p ++ diag2 size p

showDiagMoves :: Int -> Pos -> String
showDiagMoves size p =
  showBoard . placeX p . fromList size $ diagMoves size p

putDiagMoves :: Int -> Pos -> IO ()
putDiagMoves size p = putStr $ showDiagMoves size p
 
-------------------------------------------------
-- Diagonal moves running down from left to right
-------------------------------------------------

diag1 :: Int -> Pos -> [Pos]
diag1 size (curRow, curCol) = 
    if curRow < curCol
      then
        zip [0               .. maxIdx]
            [curCol - curRow .. maxIdx]
      else
        zip [curRow - curCol .. maxIdx]
            [0               .. maxIdx]
  where
    maxIdx = size - 1

showDiag1 :: Int -> Pos -> String
showDiag1 size p = showBoard . placeX p . fromList size $ diag1 size p

putDiag1 :: Int -> Pos -> IO ()
putDiag1 size p = putStr $ showDiag1 size p

-----------------------------------------------
-- Diagonal moves running up from left to right
-----------------------------------------------

diag2 :: Int -> Pos -> [Pos]
diag2 size (curRow, curCol) =
    let sumIdxs = curRow + curCol
    in  if sumIdxs < size
          then
            zip [sumIdxs, sumIdxs - 1     .. 0]
                [0                        .. maxIdx]
          else
            zip [size - 1, size - 2       .. 0]
                [curCol + curRow - maxIdx .. maxIdx]
  where
    maxIdx = size - 1

showDiag2 :: Int -> Pos -> String
showDiag2 size p = showBoard . placeX p . fromList size $ diag2 size p

putDiag2 :: Int -> Pos -> IO ()
putDiag2 size p = putStr $ showDiag2 size p

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

boardSize :: Int
boardSize = 8

qlist, qlist2 :: [Pos]
qlist  = [(1,3), (4,2), (5,3), (0,0), (6,4), (4, 6)]
qlist2 = [(0,2),(1,1),(1,5),(3,2),(4,1)]

b1 :: Board
b1 = fromList boardSize qlist

bads :: [S.Set Pos]
bads = map S.fromList
    [ [(0,0)],
      [(1,0), (1,1)],
      [(2,0), (2,1), (2,2), (2,3)],
      [(3,0), (3,1), (3,2), (3,3), (3,4), (3,5), (3,6), (3,7)] ]

test01,test02,test03,test04,test05,test06,test07,test08,
  test09,test10 :: IO ()
test01 = putBoard b1
test02 = putMoves 8 (0, 0)
test03 = putMoves 8 (1, 4)
test04 = putMoves 8 (4, 3)
test05 = putMoves 8 (7, 7)
test06 = putMoves 8 (2, 7)
test07 = putMoves 8 (5, 4)
test08 = putMoves 8 (2, 6)
test09 = putMoves 8 (6, 2)
test10 = putMoves 8 (4, 4)

test :: IO ()
test = do test01
          test02
          test03
          test04
          test05
          test06
          test07
          test08
          test09
          test10
