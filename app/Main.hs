module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


-- window and gameField size (gridheight is based on gridwidth, can be decoupled if required)
windowWidth = 1000
windowHeight = windowWidth
-- //windowHeight = 1000
gridWidth = 40 -- amount of cells the grid is wide
gridHeight = gridWidth
-- //gridHeight = 40
padding = 100

-- further sizes are calculated based on above values
fieldWidth = windowWidth - padding -- the width of the actual 'playfield' 
fieldHeight = windowHeight - padding  -- " height "
-- cell size is based on the size of the field divided by how many cells there are, width and height are separate values
cellWidth = (fieldWidth - padding) / gridWidth 
cellHeight = (fieldHeight - padding) / gridHeight

-- life values (dead is former alive, lifeless has never been alive)
aliveCell = green
deadCell = greyN 0.8
lifelessCell = black




main :: IO ()
main = do
    putStrLn "henlo"



-- window = InWindow "Game of Life" (width, height) (0,0)


-- -- padding = 25

-- w = (fromIntegral width) - padding
-- h = (fromIntegral height) - padding


-- cell = rectangleSolid (w/x) (h/y)
-- grid = rectangleSolid (w) (h)

-- x = 50
-- y = 50

