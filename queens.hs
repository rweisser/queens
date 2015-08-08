-- queens09/queens.hs

-- Find a solution to the n queens problem.  The problem is to place
-- n queens on an n x n chessboard in such a way that no queen is
-- threatening any other queen.
--
-- Change(s):
--     Uses Data.Set instead of lists for bad moves.
--     Uses the List monad for sequencing.
--     Does not use debug trace.
--     Changes the return values of findQueens from [Pos] to [[Pos]]

-- The board is an n by n grid.

-- {-# OPTIONS_GHC -Wall #-}

import Control.Monad (forM_, guard, unless, when)
import Data.Char (isDigit, toLower)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S (Set, fromList, member)
import Debug.Trace (trace)
import System.Console.GetOpt (getOpt, ArgOrder(Permute), OptDescr(Option),
           ArgDescr(NoArg, OptArg), usageInfo)
import System.Environment (getArgs)
import System.Exit(exitFailure, exitSuccess)
import System.IO(hPutStrLn, stderr)

import Board
  ( Board,             -- :: [Row]
    Pos,               -- :: (Int, Int)
    fromList,          -- :: Int -> [Pos] -> Board
    putBoard,          -- :: Board -> IO ()
    putBoardFromList ) -- :: Int -> [Pos] -> IO ()

import Moves
  ( moves,     -- Int -> Pos -> S.Set Pos
    rowMoves ) -- Int -> Pos -> [Pos]

data QState = QS { qsSize     :: Int,
                   qsCurPos   :: Pos,
                   qsBadMoves :: [S.Set Pos],
                   qsQueens   :: [Pos] }

instance Show QState where
  show qs = "\n  Queen State"
         ++ "\n    size: " ++ show (qsSize qs)
         ++ "\n    curPos: " ++ show (qsCurPos qs)
         ++ "\n    badMoves:"
         ++ concatMap (("\n      " ++) . show) (qsBadMoves qs)
         ++ "\n    queens: " ++ show (reverse (qsQueens qs))
         ++ "\n"

-- Note:  badMoves has to be a list of sets of Pos.  Otherwise there
-- is no way to back out the most recent set of bad moves when backtracking.

-- Creates initial state of the queens programs.
-- n is the number of rows in the board.
startState :: Int -> QState
startState n = QS n (0, 0) [] []

--------------------------------------------------------------------------------
-- Solution
--------------------------------------------------------------------------------

maxSolutions :: Int
maxSolutions = 1000000

allSolutions :: Int -> [[Pos]]
allSolutions n = findQueens $ startState n

firstSolution :: Int -> [Pos]
firstSolution n = let res = allSolutions n
                  in case res of
                    []  -> []
                    pss -> head pss

displayAllBoards :: Int -> IO ()
displayAllBoards n =
    do let res = allSolutions n
       case res of
         []  -> putStrLn "No solution found"
         pss -> let c_pss = zip [1 .. maxSolutions] pss
                    rest  = drop maxSolutions pss
                in do forM_ c_pss $ \(c, ps) -> do
                        putStrLn ""
                        putStrLn $ "Solution " ++ show c
                        putBoardFromList n ps
                      unless
                        (null rest)
                        (do putStrLn ""
                            putStrLn $ "NOTE:  Solutions above "
                                    ++ show maxSolutions
                                    ++ " not listed"
                            putStrLn "")

displayOneBoard :: Int -> IO ()
displayOneBoard n =
    do let res = firstSolution n
       case res of 
          [] -> putStrLn "No solution found"
          ps -> do putStrLn ""
                   putBoardFromList n ps

displayCount :: Int -> IO ()
displayCount n =
    do let res = allSolutions n
       case res of
         []  -> putStrLn "0"
         pss -> let c_pss = zip [1 .. maxSolutions] pss
                    rest  = drop maxSolutions pss
                    n     = fst . last $ c_pss
                in print n


displayAllPairs :: Int -> IO ()
displayAllPairs = mapM_ print . allSolutions

displayFirstPairs :: Int -> IO ()
displayFirstPairs = print . firstSolution

-- Find the solutions.  Find the usable spots in the current row.  If
-- placing a queen in one of them  makes a list of n queens, return
-- the list into the result.  If no spots can be found, backtrack if
-- possible.  Otherwise fail.
findQueens :: QState -> [[Pos]]
findQueens qs@(QS size pos@(curRow, curCol) badMoves queens)
    | length queens >= size = return queens
--  | otherwise = findQs qs
    | otherwise = do
        q <- findMoves qs
        let newPos    = (curRow + 1, 0)
            newBads   = moves size q : badMoves
            newQueens = q:queens
        findQueens $ QS size newPos newBads newQueens

{- Extracting this function from findQueens does not seem to be useful.

findQs qs@(QS size pos@(curRow, curCol) badMoves queens) = do
        q <- findMoves qs
        let newPos    = (curRow + 1, 0)
            newBads   = moves size q : badMoves
            newQueens = q:queens
        findQueens $ QS size newPos newBads newQueens

-}

-- Find all columns in the row where it is possible to place a queen,
-- if there are any.
findMoves :: QState -> [Pos]
findMoves (QS size pos@(curRow, curCol) badMoves _) =
    do move <- rowMoves size pos
       guard $ (not . any (S.member move)) badMoves
       return move
                     
--------------------------------------------------------------------------------
-- Argument processing
--------------------------------------------------------------------------------

data Flag = Help
          | Display HowMany
          | Pairs   HowMany
          | Count
    deriving (Eq, Show)

data HowMany = One | All | Wrong String deriving (Eq, Show)

howMany :: String
howMany = "1 or all"

instance Read HowMany where
    readsPrec = parseHowMany

-- Note: s contains only the argument for a single option.
-- s should be completely consumed by the parse.
parseHowMany :: a -> String -> [(HowMany, String)]
parseHowMany _ s
    | s   == "1"   = [(One,     "")]
    | lcs == "all" = [(All,     "")] 
    | otherwise    = [(Wrong s, "")]
  where
    lcs = map toLower s

options :: [OptDescr Flag]
options =
  [ Option "h?"
           ["help"]
           (NoArg Help)
           "this help message",

    Option "d"
           ["display"]
           (OptArg dispToFlag howMany)
           "output board layout(s)",

    Option "p"
           ["pairs"]
           (OptArg pairToFlag howMany)
           "output positions as pairs",

    Option "c"
           ["count"]
           (NoArg Count)
           "count solutions" ]

dispToFlag :: Maybe String -> Flag
dispToFlag = Display . read . fromMaybe "1"

pairToFlag :: Maybe String -> Flag
pairToFlag = Pairs . read . fromMaybe "1"

processOpts :: [String] -> ([Flag], [String], [String])
processOpts = getOpt Permute options

usage :: String
usage = "usage: queens [ options ] boardsize\noptions:"

usageMsg :: String
usageMsg = usageInfo usage options

checkErrors :: [String] -> IO ()
checkErrors errors =
    unless (null errors)
      (putErr $ concat errors ++ usageMsg)

-- If Help is one of the opts, return it.  Otherwise, all the opts
-- are mutually exclusive.  If more than one are present, the last
-- one overrides the others.
selectOpt :: [Flag] -> IO Flag
selectOpt opts =
    if Help `elem` opts then
      return Help
    else
      case opts of
        [] -> return $ Display One
        _  -> do mapM_ checkOpt opts
                 return (last opts)

checkOpt :: Flag -> IO ()
checkOpt opt =
    case opt of
      Display (Wrong s) ->
        putErr $ "invalid display value " ++ s ++ "\n" ++ usageMsg
      Pairs (Wrong s) ->
        putErr $ "invalid pairs value " ++ s ++ "\n" ++ usageMsg
      _ -> return ()

getBoardsize :: [String] -> IO Int
getBoardsize args = do
    when (null args)
      (putErr $ "boardsize must be specified\n" ++ usageMsg)
    when (length args > 1)
      (putErr $ "too many arguments" ++ usageMsg)
    let s = head args
    unless (all isDigit s)
      (putErr $ "invalid boardsize: " ++ s ++ " (must be numeric)\n" ++ usageMsg)
    return $ read s

putErr :: String -> IO ()
putErr s = do hPutStrLn stderr s
              exitFailure

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    argv <- getArgs
    let (opts, args, errors) = processOpts argv
    checkErrors errors
    opt <- selectOpt opts
    if opt == Help then
      do putStrLn usageMsg
         exitSuccess
    else do
      boardSize <- getBoardsize args
      case opt of
        Help        -> putStrLn usage
        Display One -> displayOneBoard   boardSize
        Display All -> displayAllBoards  boardSize
        Pairs   One -> displayFirstPairs boardSize
        Pairs   All -> displayAllPairs   boardSize
        Count       -> displayCount      boardSize
        badOpt      -> putErr $ "bad opt " ++ show badOpt ++ "\n" ++ usageMsg
      exitSuccess
  
--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testBoardSize :: Int
testBoardSize = 8

qlist :: [Pos]
qlist  = [(1,3), (4,2), (5,3), (0,0), (6,4), (4, 6)]

qlist2 :: [Pos]
qlist2 = [(0,2),(1,1),(1,5),(3,2),(4,1)]

b1 :: Board
b1 = fromList testBoardSize qlist

bads :: [S.Set Pos]
bads = map S.fromList [ [(0,0)],
                        [(1,0), (1,1)],
                        [(2,0), (2,1), (2,2), (2,3)],
                        [(3,0), (3,1), (3,2), (3,3), (3,4), (3,5), (3,6), (3,7)] ]

testFindNext :: Int -> Int -> [Pos]
testFindNext n m = findMoves $ QS n (m, 0) bads []

test1 :: IO ()
test1 = putBoard b1

test2 :: [[Pos]]
test2 = findQueens $ startState 8

test3 :: [Pos]
test3 = testFindNext 8 3

