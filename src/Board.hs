module Board (generateGameBoard) where

import System.Random ( randomRIO )
import Control.Monad (foldM)

data BlockState = Hit | NotHit | Unknown deriving (Show,Eq)

type BlockPosition = (Int, Int)

data BlockWithPosition = BlockWithPosition {position :: BlockPosition, hit :: BlockState} deriving (Show,Eq)

type Matrix = [[BlockState]]

type Ship = [BlockWithPosition]

data Orientation = Horizontal | Vertical deriving (Show,Eq)

type ShipSize = Int


data GameBoard = GameBoard {matrix :: Matrix, ships :: [Ship]} deriving (Show,Eq)


generateMatrixPerBoard :: Int -> Int -> Matrix
generateMatrixPerBoard maxX maxY = replicate maxY (replicate maxX NotHit)

shipsByLength :: [Int]
shipsByLength = [2,2,3,3,4,4,5,5]
-- There are usually 10 ships. 2 destroyers of length 2, 2 cruisers and submarines of length 3, 2 battleships of length 4, and 2 carriers of length 5.

generateRandomPosition :: Int ->  IO BlockPosition
generateRandomPosition maxValue = mapM (\_ -> randomRIO (0,maxValue))  (0,1)

generateRandomOrientation :: IO Orientation
generateRandomOrientation =  intToBool <$> randomRIO (0,1)
  where intToBool:: Int -> Orientation
        intToBool 0 = Vertical
        intToBool 1 = Horizontal
        intToBool _ = error "Value should never happen"


fits :: Int -> Orientation -> BlockPosition -> Int -> Bool
fits mv o p size
  | o == Horizontal = mv - fst p >= size
  | o == Vertical =  mv - snd p >= size
  | otherwise = error "Should never be caught"


shipIntersects :: [Ship] -> Ship -> Bool
shipIntersects existingShips newShip =
    any (any (checkBlockIntersection newShip)) existingShips



checkBlockIntersection :: Ship -> BlockWithPosition -> Bool
checkBlockIntersection ship block =
    block `elem` ship


addShiptoBoard :: GameBoard -> Ship ->  IO GameBoard
addShiptoBoard b sh = return  b { ships = sh : ships b }

generateShipPerBoard :: GameBoard -> ShipSize -> IO GameBoard
generateShipPerBoard board size = do
    let maxSize = length $ matrix board

    start <- generateRandomPosition maxSize

    orientation <- generateRandomOrientation

    let canFit = fits maxSize orientation start size
    if canFit
        then do
            let shipPositions = if orientation == Horizontal
                    then [(x, snd start) | x <- [fst start .. fst start + size - 1]]
                    else [(fst start, y) | y <- [snd start .. snd start + size - 1]]
            let ship = [BlockWithPosition {position = pos, hit = NotHit} | pos <- shipPositions]
            let shipIntersecting = shipIntersects (ships board)  ship
            if not shipIntersecting
            then addShiptoBoard board ship
            else generateShipPerBoard board size
        else generateShipPerBoard board size

generateGameBoard :: Int -> Int -> IO GameBoard
generateGameBoard maxX maxY = do
    let initialMatrix = generateMatrixPerBoard maxX maxY
    let initialBoard = GameBoard { matrix = initialMatrix, ships = [] }
    foldM generateShipPerBoard initialBoard shipsByLength