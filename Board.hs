-- Board.hs

-- Functions for creating and showing a chess board of any size.

-- {-# OPTIONS_GHC -Wall #-}

module Board (
    Board,
    Pos,
    Space(E, Q, X),
    at,                -- :: Board -> Pos -> Space
    fromList,          -- :: Int -> [Pos] -> Board
    inRange,           -- :: Int -> Int -> Bool
    placeE,            -- :: Pos -> Board -> Board
    placeQ,            -- :: Pos -> Board -> Board
    placeX,            -- :: Pos -> Board -> Board
    putBoard,          -- :: Board -> IO ()
    putBoardFromList,  -- :: Int -> [Pos] -> IO ()
    showBoard,         -- :: Board -> String
    showBoardFromList  -- :: Int -> [Pos] -> String
  ) where

import Data.List (intercalate, intersperse)

--------------------------------------------------------------------------------
-- Board creation and manipulation
--------------------------------------------------------------------------------
type Board = [Row]      -- Exported.
type Row   = [Space]
type Pos   = (Int, Int) -- (Row index, Column index)

-- A Space on the board can be
--   E = blank
--   Q = a queen
--   X = starting point of all of the queen's moves from that point

data Space = E | Q | X 

instance Show Space where
  show E = "   "
  show Q = " Q "
  show X = " X "

{-
 - Note:
 -
 - I tried the following for displaying a queen:
 - 
 -     show Q = " \x2655 "
 -
 - I didn't like the result.  It was too small to look good.
 -}

-- Exported.
-- Check if n is in [0, m).
inRange :: Int -> Int -> Bool
inRange m n = 0 <= n && n < m

-- Create an empty board.
empty :: Int -> Board
empty n = replicate n $ newRow n

-- Create a row of empty spaces.
newRow :: Int -> Row
newRow n = replicate n E

-- Exported.
-- Contents of space at a postion.
at :: Board -> Pos -> Space
at rs (curRow, curCol) = let r = rs !! curRow
                         in  r !! curCol

-- Exported:
placeE, placeQ, placeX :: Pos -> Board -> Board
placeE = place E    -- make a square empty
placeQ = place Q    -- place a queen in a space
placeX = place X    -- place an X in a space

--    :: value -> (rownum, colnum) -> board -> newboard
place :: Space -> Pos -> Board -> Board
place s (curRow, curCol) rs
  | inRange size curRow = let rows1       = take curRow rs
                              (row:rows2) = drop curRow rs
                          in  rows1 ++ [placeInRow s curCol row] ++ rows2
  | otherwise           = error "place: row out of range"
  where
    size = length $ head rs

--         :: value -> colnum -> row -> newrow
placeInRow :: Space -> Int -> Row -> Row
placeInRow s curCol ss
  | inRange size curCol = let spaces1 = take curCol       ss
                              spaces2 = drop (curCol + 1) ss
                          in  spaces1 ++ [s] ++ spaces2
  | otherwise = error "placeInRow: column out of range"
  where
    size = length ss

-- Exported.
-- Create a board with queens at the locations indicated in a list of pairs
-- of (rownum, colnum).  The first parameter determines the board size.
fromList :: Int -> [Pos] -> Board
fromList = foldr placeQ . empty

--------------------------------------------------------------------------------
-- Show functions
--------------------------------------------------------------------------------

showBoard :: Board -> String
showBoard rs = line ++ "\n" ++ rows ++ line ++ "\n"
  where
    w    = 1 + 4 * length (head rs)
    line = replicate w '-'
    rows = unlines . intersperse line . map showRow $ rs

putBoard :: Board -> IO ()
putBoard = putStr . showBoard

showBoardFromList :: Int -> [Pos] -> String
showBoardFromList n ps = showBoard $ fromList n ps

putBoardFromList :: Int -> [Pos] -> IO ()
putBoardFromList n ps = putStr $ showBoardFromList n ps

showRow :: Row -> String
showRow ss = "|" ++ (intercalate "|" . map show) ss ++ "|"

putRow :: Row -> IO ()
putRow = putStr . showRow

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

boardSize :: Int
boardSize = 8

qlist :: [Pos]
qlist = [(1,3), (4,2), (5,3), (0,0), (6,4), (4, 6)]

b1 :: Board
b1 = foldr placeQ (empty boardSize) qlist

test1 :: IO ()
test1 = putBoard b1

