import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (randomRIO)
import Control.Monad (replicateM)

-- user changeable vars ###########################################################################################
amountOfStartingCells :: Int
amountOfStartingCells = 20 -- The amount of living cells a new board starts with


-- ################################################################################################################

windowHeight, windowWidth :: Int
windowWidth = 800
windowHeight = 800

gridWidth, gridHeight :: Float
gridWidth = 6 -- amount of cells the grid is wide
gridHeight = gridWidth
-- //gridHeight = 40
padding :: Float
padding = 200

-- further sizes are calculated based on above values
fieldWidth, fieldHeight :: Float
fieldWidth = fromIntegral windowWidth - padding -- the width of the actual 'playfield' 
fieldHeight = fromIntegral windowHeight - padding  -- " height "
-- cell size is based on the size of the field divided by how many cells there are, width and height are separate values
cellWidth, cellHeight :: Float
cellWidth = fieldWidth / gridWidth 
cellHeight = fieldHeight / gridHeight

-- life values (dead is former alive, lifeless has never been alive)
aliveCell = green
deadCell = greyN 0.8
lifelessCell = black

type Cell = (Int, Int)
type Board = [Cell]
type GameState = Board

-- Sets up the initial board at start
initialState :: IO GameState
-- initialState = [(1,0), (2,1), (0,2), (1,2), (2,2)]
initialState = generateRandomCoords amountOfStartingCells

-- Convert grid coords to pixel coords
toPixelCoords (x,y) = 
    let fx = fromIntegral x * cellWidth - fieldWidth /2
        fy = fromIntegral y * cellHeight - fieldHeight /2
    in (fx, fy)

drawCell :: (Int, Int) -> Picture
drawCell pos = 
    let (x,y) = toPixelCoords pos
    in translate (x + cellWidth / 2) (y + cellHeight / 2) $ color aliveCell $ rectangleSolid cellWidth cellHeight

-- Draws the playfield for the gol
drawGridLines :: Picture
drawGridLines = color (black) $ pictures (verticalLines ++ horizontalLines)
  where
    halfFieldWidth = fieldWidth / 2
    halfFieldHeight = fieldHeight / 2

    verticalLines = 
        [line [ (x, -halfFieldHeight), (x, halfFieldHeight)] 
        | i <- [0 .. gridWidth]
        , let x = (i * cellWidth) - halfFieldWidth
        ]  

    horizontalLines = 
      [ line [ (-halfFieldWidth, y), (halfFieldWidth, y) ]
      | i <- [0 .. gridHeight]
      , let y = (i * cellHeight) - halfFieldHeight
      ]

draw :: GameState -> Picture
draw board = pictures [drawGridLines, drawCells board]

drawCells :: GameState -> Picture
drawCells board = pictures (map drawCell board) 

-- draw :: GameState -> Picture
-- draw board = pictures (map drawCell board) 

handleInput :: Event -> GameState -> GameState
handleInput _ s = s

update :: Float -> GameState -> GameState
update seconds x = x

toInt :: Float -> Int
toInt = round

-- Generates random integer between low and high
generateRandomIntBetween :: Int -> Int -> IO Int
generateRandomIntBetween low high = randomRIO (low, high)

-- Generates random coordinates on the board using generateRandomIntBetween
getRandomCoords :: IO Cell
getRandomCoords = do
    x <- generateRandomIntBetween 0 (toInt gridWidth - 1) -- Generates a random X coord between 0 and 2
    y <-  generateRandomIntBetween 0 (toInt gridHeight - 1) -- Generates a random Y coord between 0 and 2
    return (x, y)

-- Generates an n number of random coords
generateRandomCoords :: Int -> IO [Cell]
generateRandomCoords n = replicateM n getRandomCoords


main :: IO ()
main = do
    state <- initialState
    play
      (InWindow "Game of Life" (windowWidth, windowHeight) (0,0))
      white
      60
      state
      draw
      handleInput
      update
