
import Control.Monad

import Data.Char
import Data.List
import Data.Maybe

import System.IO



-- | Rose trees

data Rose a = MkRose a [Rose a]
    deriving (Eq, Show)


-- Exercise 1

root :: Rose a -> a
root (MkRose v rs) = v 

children :: Rose a -> [Rose a]
children (MkRose v rs) = rs


-- Exercise 2

size :: Rose a -> Int
size (MkRose _ rs) = 1 + sumSizes rs
  where sumSizes = sum . map size

leaves :: Rose a -> Int
leaves (MkRose _ []) = 1
leaves (MkRose _ rs) = 0 + sumLeaves rs
  where sumLeaves = sum . map leaves


-- | State representation
-- * Players

data Player = P1 | P2
    deriving (Eq, Ord)

instance Show Player where
    show P1 = "Player 1"
    show P2 = "Player 2"
    

-- Exercise 3
    
nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1


-- * Board

data Field = X | O | B
    deriving (Eq, Ord)

instance Show Field where
    show X = "X"
    show O = "O"
    show B = " "


-- Exercise 4

symbol :: Player -> Field
symbol P1 = X
symbol P2 = O

type Row   = (Field, Field, Field)
type Board = (Row, Row, Row)

-- Exercise 5

verticals :: Board -> (Row, Row, Row)
verticals ((f1,f2,f3), (f4, f5, f6), (f7, f8, f9)) 
  = ((f1, f4, f7), (f2, f5, f8), (f3, f6, f9))

diagonals :: Board -> (Row, Row)
diagonals ((f1,f2,f3), (f4, f5, f6), (f7, f8, f9)) 
  = ((f1, f5, f9), (f3, f5, f7))


-- Exercise 6

emptyBoard :: Board
emptyBoard = ((B, B, B), (B, B, B), (B, B, B))

-- Exercise 7


printRow :: Row -> String
printRow (fx, fy, fz) = show fx ++"|"++ show fy ++"|"++ show fz ++ "\n"

printBoard :: Board -> String
printBoard (rx, ry, rz) = printRow rx ++ breakLine ++ printRow ry ++ breakLine ++ printRow rz
  where breakLine = "-+-+-\n"



-- | Move generation
-- Exercise 8

traverseFst :: (a -> [d]) -> (a,b,c) -> [(d,b,c)]
traverseFst mr (r1, r2, r3) = [(r1Res, r2, r3) | r1Res <- mr r1]

traverseSnd :: (b -> [d]) -> (a,b,c) -> [(a,d,c)]
traverseSnd mr (r1, r2, r3) = [(r1, r2Res, r3) | r2Res <- mr r2]

traverseThd :: (c -> [d]) -> (a,b,c) -> [(a,b,d)]
traverseThd mr (r1, r2, r3) = [(r1, r2, r3Res) | r3Res <- mr r3]


traverseAll :: (Row -> [Row]) -> Board -> [Board]
traverseAll mr board = traverseFst mr board ++ traverseSnd mr board ++ traverseThd mr board


movesRow :: Player -> Row -> [Row]
movesRow player (fx, fy, fz) = mutFieldFst ++ mutFieldSnd ++ mutFieldThd
  where 
    playerSymbol = symbol player 
    mutFieldFst = case fx of {B -> [(playerSymbol, fy, fz)]; _ -> []}
    mutFieldSnd = case fy of {B -> [(fx, playerSymbol, fz)]; _ -> []}
    mutFieldThd = case fz of {B -> [(fx, fy, playerSymbol)]; _ -> []}
    
             
moves :: Player -> Board -> [Board]
moves player = traverseAll (movesRow player)



-- | Gametree generation
-- Exercise 9

pairToList :: (a,a) -> [a]
pairToList (a,b) = [a,b]

tripletToList :: (a, a, a) -> [a]
tripletToList (a, b, c) = [a,b,c]

tripletValuesAllEqual :: Eq a => a -> (a,a,a) -> Bool
tripletValuesAllEqual x (v1, v2, v3) 
  | v1 == x && v2 == x && v3 == x   = True
  | otherwise                       = False

hasWinner :: Board -> Maybe Player
hasWinner board@(r1,r2,r3)
  | playerHasWon P1     = Just P1
  | playerHasWon P2     = Just P2
  | otherwise           = Nothing
  where 
    lines = [r1, r2, r3] ++ tripletToList (verticals board) ++ pairToList (diagonals board)
    playerHasWon player = any (tripletValuesAllEqual (symbol player)) lines
    


-- Exercise 10

gameTree :: Player -> Board -> Rose Board
gameTree player board
  | gameHasEnded        = MkRose board []
  | otherwise           = MkRose board subGameTrees -- ph
  where 
    gameHasEnded = isJust (hasWinner board)
    subBoards = moves player board
    makeSubGameTree = gameTree (nextPlayer player)
    subGameTrees = map makeSubGameTree subBoards



-- | Game complexity
-- Exercise 11

gameTreeComplexity :: Int
gameTreeComplexity = leaves (gameTree P1 emptyBoard)



-- | Minimax
-- Exercise 12
minimax' :: Player -> Player -> Rose Board -> Rose Int
minimax' curPlayer mmPlayer (MkRose board [])
  = case hasWinner board of
      Just player -> if player == mmPlayer then MkRose 1 [] else MkRose (-1) [] 
      Nothing -> MkRose 0 []
minimax' curPlayer mmPlayer (MkRose board subGameTrees) 
  | curPlayer == mmPlayer       = MkRose maxScore subMinimaxTrees
  | otherwise                   = MkRose minScore subMinimaxTrees
  where 
    makeSubMinimax    = minimax' (nextPlayer curPlayer) mmPlayer
    subMinimaxTrees   = map makeSubMinimax subGameTrees
    maxScore          = maximum' (map root subMinimaxTrees)
    minScore          = minimum' (map root subMinimaxTrees)
    

minimax :: Player -> Rose Board -> Rose Int
minimax p = minimax' p p


-- * Lazier minimum and maximums
-- Exercise 13

minimum' :: [Int] -> Int
minimum' lst
  | (-1) `elem` lst   = -1
  | 0 `elem` lst      = 0
  | otherwise         = 1

maximum' :: [Int] -> Int
maximum' lst
  | 1 `elem` lst      = 1
  | 0 `elem` lst      = 0
  | otherwise         = -1

-- | Gameplay
-- Exercise 14

makeMove :: Player -> Board -> Maybe Board
makeMove player board
  | gameHasEnded || noMovesLeft   = Nothing
  | otherwise                     = Just nextBoard
  where
    gameHasEnded = isJust (hasWinner board)
    noMovesLeft = null (children gTree)
    gTree = gameTree player board
    mmTree = minimax player gTree
    bestScore = root mmTree
    subTreeScores = map root (children mmTree)
    bestMoveIndices = [i  | (i,score) <- zip [0..] subTreeScores, score == bestScore]
    nextBoard = root (children gTree !! head bestMoveIndices)



-- | Main

data PlayerType = Human | Computer

instance Show PlayerType where
    show Human    = "H"
    show Computer = "C"

main :: IO ()
main = do
    typeOfP1 <- askFor "Should Player 1 be a (H)uman or a (C)omputer player?"
                       [Human, Computer]
    typeOfP2 <- askFor "Should Player 2 be a (H)uman or a (C)omputer player?"
                       [Human, Computer]

    let playerType :: Player -> PlayerType 
        playerType P1 = typeOfP1
        playerType P2 = typeOfP2

        gameLoop :: Player -> Board -> IO ()
        gameLoop p b = do
            putStrLn ("\n" ++ printBoard b)
            case hasWinner b of
                Just p  -> putStrLn (show p ++ " has won!")
                Nothing -> do
                    putStr   ("It's " ++ show p ++ "'s turn. ")
                    mb' <- case playerType p of
                        Human    -> humanMove    p b
                        Computer -> computerMove p b
                    case mb' of
                        Nothing -> do putStr   "No more moves are possible. "
                                      putStrLn "It's a draw."
                        Just b' -> gameLoop (nextPlayer p) b'

        humanMove :: Player -> Board -> IO (Maybe Board)
        humanMove p b =
            case moves p b of
              [] -> return Nothing
              possibleMoves -> do
                putStrLn "Possible moves are:"
                putStrLn (listMoves possibleMoves)
                i <- askFor "Make your choice:" [1..length possibleMoves]
                return (Just (possibleMoves !! (i-1)))

        computerMove :: Player -> Board -> IO (Maybe Board)
        computerMove p b = do
            putStrLn "Thinking..."
            return (makeMove p b)

        listMoves :: [Board] -> String
        listMoves = intercalate "\n"
                    . map (intercalate "    ")
                    . transpose
                    . map lines
                    . map (\(i,b) -> "(" ++ show i ++ "): \n" ++ printBoard b) 
                    . zip [1 :: Integer ..]

    gameLoop P1 emptyBoard

askFor :: Show a => String -> [a] -> IO a
askFor m xs = do
    putStr (m ++ " ")
    hFlush stdout
    i <- getLine
    case find ((map toLower i ==) . map toLower . show) xs of
        Nothing -> do putStrLn $ "I didn't understand you. Enter one of: "
                                 ++ intercalate ", " (map show xs) ++ "."
                      askFor m xs
        Just y  -> return y