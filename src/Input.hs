module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI

import Data.Maybe

import Debug.Trace

boardHeight = 600
boardWidth = 600
windowLeft = (-boardWidth/2)
windowTop = boardHeight/2

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
--
-- trace :: String -> a -> a
-- 'trace' returns its second argument while printing its first argument
-- to stderr, which can be a very useful way of debugging!

-- Prints uneeded information for testing purposes. Should be removed later.
handleInput :: Event -> World -> World
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) b 
    = trace ("Left button pressed at: " ++ show (x,y) ++ " and was translated to click on coords: " ++ show (getBoardCoordFromClick (x,y) (board b))) (handleMouseClick  (x,y) b)
-- Could use spacebar as the button to revert one move (extension).
handleInput (EventKey (Char k) Up _ _) b
    = trace ("Key " ++ show k ++ " up") b
handleInput e b = b

-- Function to handle mouse clicks from the user.
-- IMPORTANT: Needs to also check whether the current turn belongs to a human player.
-- Still allows mouse clicks to make moves at any time - needs to be blocked when its the computers turn.
handleMouseClick :: (Float, Float) -> World -> World
handleMouseClick clickPos world = attemptMove userClick world
    where userClick = getBoardCoordFromClick clickPos (board world)
    {- Here is where there should be a check if current turn is for the AI
    | isNothing updatedBoard = world
    | otherwise              = World (fromJust updatedBoard) (other $ turn world) (winner world)
    where updatedBoard = makeMove (board world) (turn world) (getBoardCoordFromClick clickPos (board world))-}

-- Translates window coordinates of a mouse click in to coordinates used by the board.
-- For detecting in which square the player has clicked.
getBoardCoordFromClick :: (Float, Float) -> Board -> Position
getBoardCoordFromClick (x, y) board = ((floor $ distanceFromLeft/pixelsPerColumnX),(floor $ distanceFromTop/pixelsPerColumnY))
    where numberOfColumns = fromIntegral $ size board
          pixelsPerColumnX = boardWidth/numberOfColumns
          pixelsPerColumnY = boardHeight/numberOfColumns
          distanceFromLeft = x-windowLeft
          distanceFromTop = windowTop-y 