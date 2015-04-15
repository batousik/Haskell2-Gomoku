module Draw(drawWorld) where

import Graphics.Gloss
import Board

import Data.Maybe

boardHeight = 600
boardWidth = 600

boardLeft = (-boardWidth/2)
boardTop = boardHeight/2

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: World -> Picture
drawWorld w = pictures $ [drawGrid w,drawPieces w,drawBottomText w,drawTopText w]

drawTopText :: World -> Picture
drawTopText world
    | isNothing (winner world)   = Blank
    | otherwise                  = Translate 0 (boardTop + 40) $ Scale 0.2 0.2 $ Color white $ Text ("Winner is: " ++ (show $ fromJust $ winner world))

drawBottomText :: World -> Picture
drawBottomText world = Translate 0 (boardTop - boardHeight - 50) $ Scale 0.2 0.2 $ Color white $ Text ("Current turn: " ++ (show $ turn world))

-- Draws the pieces on the board.
drawPieces :: World -> Picture
drawPieces world = pictures $ map (drawCircleOnGrid $ board world) (pieces $ board $ world)

-- Draws one circle on the grid
drawCircleOnGrid :: Board -> (Position, Col) -> Picture
drawCircleOnGrid board piece = Translate circleX circleY (getCircleWithColor board piece)
    where circleX = getCircleXCoord board $ fst piece
          circleY = getCircleYCoord board $ fst piece
          {-circlePic =  (getCircleColor (snd piece)) $ Circle circleRadius
          circleRadius = ((boardWidth/(fromIntegral $ size board))/2)*0.75-}

-- Gets the X coordinate of a circle in given position
getCircleXCoord :: Board -> Position -> Float
getCircleXCoord board pos = boardLeft + (squareWidth*(fromIntegral $ fst pos)) + (squareWidth/2)
    where squareWidth = boardWidth/(fromIntegral $ size board)

-- Gets the Y coordinate of a circle in given position
getCircleYCoord :: Board -> Position -> Float
getCircleYCoord board pos = boardTop - (squareHeight*(fromIntegral $ snd pos)) - (squareHeight/2)
    where squareHeight = boardHeight/(fromIntegral $ size board)

getCircleWithColor :: Board -> (Position, Col) -> Picture
getCircleWithColor board (x, White) = Color white $ ThickCircle circleRadius 30
    where circleRadius = ((boardWidth/(fromIntegral $ size board))/2)*0.50
getCircleWithColor board (x, Black) = Color black $ ThickCircle circleRadius 30
    where circleRadius = ((boardWidth/(fromIntegral $ size board))/2)*0.50

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