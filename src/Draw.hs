module Draw(drawWorld) where

import Graphics.Gloss
import Board

boardHeight = 600
boardWidth = 600

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: World -> Picture
drawWorld w = drawGrid w

-- Draws the pieces on the board.
drawPieces :: World -> Picture
drawPieces world = pictures $ map drawCircleOnGrid (pieces $ board $ world)

-- Draws one circle on the grid
drawCircleOnGrid :: (Position, Col) -> Picture
drawCircleOnGrid piece = undefined

-- Draws the grid of the board
drawGrid :: World -> Picture
drawGrid world = pictures [(drawGridHorizontal world), (drawGridVertical world)]

-- draws the horizontal lines of the grid
drawGridHorizontal :: World -> Picture
drawGridHorizontal world = pictures $ map line (getHorizontalLineCoordinates gameBoardSize gameBoardSize)
    where gameBoardSize = size $ board $ world

-- Gets  a list of paths (coordinates) to create the horizontal lines of the playing grid.
-- Divisor for the height coordinates -> board size
getHorizontalLineCoordinates :: Int -> Int -> [Path]
getHorizontalLineCoordinates 0 boardSize = [((-boardWidth/2),heightCoord),(boardWidth/2,heightCoord)]:[]
    where heightCoord = (-boardHeight/2)
getHorizontalLineCoordinates div boardSize = [((-boardWidth/2),heightCoord),(boardWidth/2,heightCoord)]:(getHorizontalLineCoordinates (div-1) boardSize)
    where heightCoord = (-boardHeight/2) + ((boardHeight/(fromIntegral boardSize))*(fromIntegral div))

-- draws the vertical lines of the grid
drawGridVertical :: World -> Picture
drawGridVertical world = pictures $ map line (getVerticalLineCoordinates gameBoardSize gameBoardSize)
    where gameBoardSize = size $ board $ world

-- Gets  a list of paths (coordinates) to create the horizontal lines of the playing grid.
-- Divisor for the height coordinates -> board size
getVerticalLineCoordinates :: Int -> Int -> [Path]
getVerticalLineCoordinates 0 boardSize = [(widthCoord,(-boardHeight/2)),(widthCoord,boardHeight/2)]:[]
    where widthCoord = (-boardWidth/2)
getVerticalLineCoordinates div boardSize = [(widthCoord,(-boardHeight/2)),(widthCoord,boardHeight/2)]:(getVerticalLineCoordinates (div-1) boardSize)
    where widthCoord = (-boardWidth/2) + ((boardWidth/(fromIntegral boardSize))*(fromIntegral div))