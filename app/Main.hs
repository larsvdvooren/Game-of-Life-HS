import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (randomRIO)
import Control.Monad (replicateM)
import qualified Data.Set as Set

-- user changeable vars ###########################################################################################
amountOfStartingCells :: Int
--amountOfStartingCells = 20 -- The amount of living cells a new board starts with
amountOfStartingCells = toInt (gridWidth * gridHeight / 1)

windowHeight, windowWidth :: Int
windowWidth = 800
windowHeight = 800

gridWidth, gridHeight :: Float
gridWidth = 40 -- amount of cells the grid is wide
gridHeight = gridWidth -- Can be untied from one another
--gridHeight = 40

padding :: Float
padding = 100

-- ################################################################################################################

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

-- TODO: NEED a better way to store cells, Set probably the way to go

-- Sets up the initial board at start
initialState :: IO GameState
-- initialState = [(1,0), (2,1), (0,2), (1,2), (2,2)]
initialState = generateRandomCoords amountOfStartingCells

-- Convert grid coords to pixel coords
toPixelCoords (x,y) = 
    let fx = fromIntegral x * cellWidth - fieldWidth /2
        fy = fromIntegral y * cellHeight - fieldHeight /2
    in (fx, fy)

-- Draws individual cell on the board (Coordinates start at bottom left (0,0) and end at top right(gridWidth,gridHeight))
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

-- Placeholder until interacton is implemented
handleInput :: Event -> GameState -> GameState
handleInput _ s = s

-- Placeholder untill updates are implemented
update :: Float -> GameState -> GameState
update _ = calculateNextGeneration

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
-- TODO: remove duplicates in GameState

-- Counts the amount of coordinates in current state
printListLength :: Show a => String -> [a] -> IO ()
printListLength label xs = putStrLn $ label ++ ": " ++ show (length xs)

-- Removes duplicates from Gamestate
removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = go Set.empty
  where
    go _   []     = []
    go set (x:xs)
      | x `Set.member` set = go set xs
      | otherwise          = x : go (Set.insert x set) xs

-- ################################ Life cooking here ################################## --

-- Counts the amount of neighbors a cell has
countCellNeighbors :: Cell -> GameState -> Int
countCellNeighbors (x, y) board = length aliveNeighbors
  where
      neighborCoords = [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0,0)]
      aliveNeighbors = filter (`elem` board) neighborCoords

      -- TODO: break up into 2 functions, pointToNeighbors and countNeighbors

calculateNextGeneration :: GameState -> GameState
calculateNextGeneration board = removeDuplicates $ survivors ++ births
  where
    -- If a cell has 2 or 3 neighbors they survive to the next generation
    survivors = [cell | cell <- board, let c = countCellNeighbors cell board, c == 2 || c == 3]   

    -- if a dead/empty cell has 3 living neighbors that cell comes alive(again)
    neighborCoords = [(x + dx, y + dy) | (x, y) <- board, dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0,0)]
    deadNeighbors = filter (\cell -> cell `notElem` board) neighborCoords
    births = [cell | cell <- deadNeighbors, countCellNeighbors cell board == 3]

-- TODO: Police cells that leave the gamefield (get rid of padding if no worky)

-- ######################################### EOL ####################################### --

main :: IO ()
main = do
    uncleanedState <- initialState
    printListLength "coord count pre removeDuplicates" uncleanedState
    let state = removeDuplicates uncleanedState
    printListLength "coord count post removeDuplicates" state
    -- putStrLn (show (countCellNeighbors (0,0) state))
    play
      (InWindow "Game of Life" (windowWidth, windowHeight) (0,0))
      white
      2
      state
      draw
      handleInput
      update