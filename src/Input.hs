module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI

import Data.Maybe

import Debug.Trace

windowHeight = 600
windowWidth = 600
windowLeft = (-windowWidth/2)
windowTop = windowHeight/2

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
handleInput (EventKey (Char k) Up _ _) b
    = trace ("Key " ++ show k ++ " up") b
handleInput e b = b

-- Function to handle mouse clicks from the user.
-- Needs to also check whether the current turn belongs to a human player.
handleMouseClick :: (Float, Float) -> World -> World
handleMouseClick clickPos world
    -- Here is where there should be a check if current turn is for the AI
    | isNothing updatedBoard = world
    | otherwise              = World (fromJust updatedBoard) (other $ turn world) (winner world)
    where updatedBoard = makeMove (board world) (turn world) (getBoardCoordFromClick clickPos (board world))

-- Translates window coordinates of a mouse click in to coordinates used by the board.
-- For detecting in which square the player has clicked.
getBoardCoordFromClick :: (Float, Float) -> Board -> Position
getBoardCoordFromClick (x, y) board = ((floor $ distanceFromLeft/pixelsPerColumnX),(floor $ distanceFromTop/pixelsPerColumnY))
    where numberOfColumns = fromIntegral $ size board
          pixelsPerColumnX = windowWidth/numberOfColumns
          pixelsPerColumnY = windowHeight/numberOfColumns
          distanceFromLeft = x-windowLeft
          distanceFromTop = windowTop-y 