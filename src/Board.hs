module Board where

import Data.Maybe
import Debug.Trace
import Control.Monad

data Col = Black | White
    deriving (Show, Eq)

data Direction = NorthEast | North | NorthWest | West | SouthWest | South | SouthEast | East
    deriving Show

--AllDirections = [NorthEast, North, NorthWest, West, SouthWest, South, SouthEast, East]

other :: Col -> Col
other Black = White
other White = Black

type Position = (Int, Int)


-- A Board is a record containing the board size (a board is a square grid,
-- n * n), the number of pieces in a row required to win, and a list 
-- of pairs of position and the colour at that position.  So a 10x10 board 
-- for a game of 5 in a row with a black piece at 5,5 and a white piece at 8,7
-- would be represented as:
--
-- Board 10 5 [((5, 5), Black), ((8,7), White)]

data Board = Board { size :: Int,
                     target :: Int,
                     pieces :: [(Position, Col)]
                   }
  deriving Show

-- Default board is 19x19, 5 in a row and no pieces.
initBoard = Board 19 5 []

-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).

-- List of moves probably not needed as the list of pieces (in the board) is an ordered 
-- history of moves anyway. 
data World = World { board :: Board,
                     turn :: Col,
                     winner :: Maybe Col }

initWorld = World initBoard Black Nothing

-- Uses command line arguments to determine settings of the game.
-- Assumes following order of commands: size of board, pieces in a row required to win, AI player, extra rules active.
getSettingsFromCmdLine :: IO [String] -> IO World
getSettingsFromCmdLine xs = do
    ys <- xs
    let xslen = length ys
        returnWorld = World (Board (getIntFromString (ys !! 0)) (getIntFromString (ys !! 1)) []) Black Nothing
    return returnWorld

-- Gets sizes from IO String coming from cmd line
getIntFromString :: String -> Int
getIntFromString string = read string :: Int

-- Attempts to play a move. If move is successful return updated world. Include winner if there is one.
-- If move wasnt successful, return unchanged world.
attemptMove :: Position -> World -> World
attemptMove pos world
    | isNothing updatedBoard = world
    | otherwise              = isWinner $ World (fromJust updatedBoard) (other $ turn world) (winner world)
    where updatedBoard = makeMove (board world) (turn world) pos

-- Checks if the world's board has a winner, and adds that player as the winner in the world if thats the case.
-- A win is not possible before the 9th move - therefore the game does not check for them until then.
isWinner :: World -> World
isWinner world
    | (length $ pieces $ board $ world) < earliestWin = world
    | isNothing possibleWinner                        = world
    | otherwise                                       = World (board world) (turn world) (possibleWinner)
    where possibleWinner = checkWon (board world)
          earliestWin = ((target $ board $ world) * 2) - 1

-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove board color pos
    | isCellValidMove color pos board = Just (Board (size board) (target board) ((pos, color):(pieces board)))
    | otherwise = Nothing

-- Checks if a position is valid to move to.
-- If cell is within bounds and empty.
isCellValidMove :: Col -> Position -> Board -> Bool
isCellValidMove color pos board
    | (posWithinBounds pos board) && (cellAtPosIsEmpty pos board)
        && not (checkFourAndFour (pos,color) board)                       = True
        {-&& not (checkThreeAndThree (pos,color) board)-}                     
    | otherwise                                                           = False

-- Checks if position is within bounds of board.
posWithinBounds :: Position -> Board -> Bool
posWithinBounds pos board
    | fst pos < 0              = False
    | snd pos < 0              = False
    | fst pos >= size board    = False
    | snd pos >= size board    = False
    | otherwise                = True

-- Checks if position is empty on board (does not guarantee cell is within bounds of board)
cellAtPosIsEmpty :: Position -> Board -> Bool
cellAtPosIsEmpty pos board
    | isNothing piece      = True
    | otherwise            = False
    where piece = pieceAtPos pos board

-- Gets piece at position, if there isn't a piece in that position, returns Nothing
pieceAtPos :: Position -> Board -> Maybe (Position, Col)
pieceAtPos pos board
        | null matchingPos      = Nothing
        | otherwise             = Just $ head matchingPos
        where matchingPos = [x | x <- (pieces board), fst x == pos]

-- Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won
checkWon :: Board -> Maybe Col
checkWon board
    | True `elem` winningRows                     = Just $ snd latestPiece
    | otherwise                                   = Nothing
    where listOfPieces = [x | x <- (pieces board), snd x == snd latestPiece]
          latestPiece = head $ pieces board
          winningRows = map (checkWonAllDirections board) listOfPieces

-- Runs the checking functions in all directions looking for a winning condition for the piece provided.
-- Could (should) be optimized to not have to run all directions - just one direction returning true is enough.
checkWonAllDirections :: Board -> (Position, Col) -> Bool
checkWonAllDirections board piece
    | True `elem` xs        = True
    | otherwise             = False
    where xs = [checkWonDirection x piece board | x <- xss]
          xss = [NorthEast, North, NorthWest, West, SouthWest, South, SouthEast, East]

-- Checks if the correct amount of pieces in a row in a direction are present.
-- Returns true if that is the case
checkWonDirection :: Direction -> (Position, Col) -> Board -> Bool
checkWonDirection dir piece board
    | checkConsecutiveInDirection dir (Just piece) board == (target board) = True
    | otherwise                                                            = False

-- Checks if the rule "four and four" is being broken by making a move at the given position.
-- Possibly a too simple implementation?
checkFourAndFour :: (Position, Col) -> Board -> Bool
checkFourAndFour piece board
    | length (filter (==4) finalList) >= 2              = True
    | otherwise                                         = False
    where xs = [checkConsecutiveInDirection x (Just piece) board | x <- xss]
          xss = [NorthEast, North, NorthWest, West, SouthWest, South, SouthEast, East]
          ys = [checkConsecutiveInDirection y (Just piece) board | y <- yss]
          yss = [SouthWest, South, SouthEast, East, NorthEast, North, NorthWest, West]
          zippedList = zipWith (+) xs ys
          finalList = map (\x -> x - 1) zippedList

-- Checks if the rule "three and three" is being broken by making a move at the given position.
checkThreeAndThree :: (Position, Col) -> Board -> Bool
checkThreeAndThree piece board
    | length (filter (==3) finalList) >= 2             = True
    | otherwise                                        = False
    where xs = [checkConsecutiveInDirectionWithOpenSpace x (Just piece) board | x <- xss]
          xss = [NorthEast, North, NorthWest, West, SouthWest, South, SouthEast, East]
          ys = [checkConsecutiveInDirectionWithOpenSpace y (Just piece) board | y <- yss]
          yss = [SouthWest, South, SouthEast, East, NorthEast, North, NorthWest, West]
          zippedList = zipWith (+) xs ys
          finalList = map (\x -> x - 1) zippedList


-- Checks how many pieces in a row in a direction.
-- The color of the piece is derived from the given piece.
-- Adds one at the end because there will always be at least one in a row.
-- (Because of the initial piece being the player's own)
checkConsecutiveInDirection :: Direction -> Maybe (Position, Col) -> Board -> Int
checkConsecutiveInDirection dir piece board
    | isNothing nextpiece                                   = 1
    | isSameColor (fromJust piece) (fromJust nextpiece)     = 1 + checkConsecutiveInDirection dir nextpiece board
    | otherwise                                             = 1
    where nextpiece = getPieceInDirection dir (fromJust piece) board

-- Checks for rows of pieces, with an open space at end.
-- If the row is blocked at the end, the function returns a negative value.
checkConsecutiveInDirectionWithOpenSpace :: Direction -> Maybe (Position, Col) -> Board -> Int
checkConsecutiveInDirectionWithOpenSpace dir piece board
    | isNothing nextpiece                                   = 1
    | isSameColor (fromJust piece) (fromJust nextpiece)     = 1 + checkConsecutiveInDirectionWithOpenSpace dir nextpiece board
    | otherwise                                             = -1000
    where nextpiece = getPieceInDirection dir (fromJust piece) board

-- Function to return whatever is in the position in a direction from the given piece.
-- Returns nothing if the position is empty or out of bounds.
getPieceInDirection :: Direction -> (Position, Col) -> Board -> Maybe (Position, Col)
getPieceInDirection dir piece board 
    | isNothing newpiece      = Nothing
    | otherwise               = newpiece
    where position = getPosInDirection dir (fst piece)
          newpiece = pieceAtPos position board 

-- Takes a position and returns a position in the direction provided
getPosInDirection :: Direction -> Position -> Position
getPosInDirection NorthEast (x,y) = (x + 1, y + 1)
getPosInDirection North (x,y)     = (x, y + 1)
getPosInDirection NorthWest (x,y) = (x - 1, y + 1)
getPosInDirection West (x,y)      = (x - 1, y)
getPosInDirection SouthWest (x,y) = (x - 1, y - 1)
getPosInDirection South (x,y)     = (x, y - 1)
getPosInDirection SouthEast (x,y) = (x + 1, y - 1)
getPosInDirection East (x,y)      = (x + 1, y)

-- Checks if two pieces are of the same color.
isSameColor :: (Position, Col) -> (Position, Col) -> Bool
isSameColor piece1 piece2
    | snd piece1 == snd piece2 = True
    | otherwise                = False


-- Helper function to get the color at a position on the board
getColAtPos :: Position -> Board -> Maybe Col
getColAtPos pos board
    | isNothing piece       = Nothing
    | otherwise             = Just $ snd $ fromJust piece
    where  piece = pieceAtPos pos board

-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Col -> Int
evaluate = undefined



