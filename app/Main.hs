module Main (main) where

import Board(generateGameBoard,hitBoard,shipsByLength,GameBoard,areAllShipsDead,ships,matrix,BlockPosition)
import Solver(getProbabilityPerBoardState,getProbabibilityGridFromBoard,mergeMatrix,getMostProbableHit,GridCell(..),Grid)
import Data.Foldable (Foldable(fold, foldl'))


main :: IO ()
main = do
    board <- generateGameBoard 10 10
    gameLoop board

gameLoop :: GameBoard -> IO ()
gameLoop board = do
    putStrLn "Enter the coordinates for your shot (row, column):"
    input <- getLine
    let coordinates = parseInput input
    case coordinates of
        Just (row, col) -> do
            let positionToHit = (row, col)
            let newBoard = hitBoard board positionToHit
            printShipsState newBoard
            let gridCell = getProbabilityGridFromBoard newBoard
            let calculatedGrid = getProbabilityPerBoardState gridCell shipsByLength
            print "Predicted Value for the Next Block:"
            print $ getMostProbableHit calculatedGrid
            if areAllShipsDead newBoard
                then putStrLn "Congratulations! You've sunk all the ships!"
                else gameLoop newBoard
        Nothing -> do
            putStrLn "Invalid input. Please enter coordinates in the format 'row, column'."
            gameLoop board

parseInput :: String -> Maybe BlockPosition
parseInput input = case words input of
    [row, col] -> Just (read row, read col)
    _ -> Nothing

printShipsState :: GameBoard -> IO ()
printShipsState board = do
    putStrLn "Ship States:"
    mapM_ print (ships board)

-- add a maybe type for the value and add nothing here 
getProbabilityGridFromBoard :: GameBoard -> Grid
getProbabilityGridFromBoard board = map (map mappedBlock) (matrix board)
    where mappedBlock block = GridCell { state = block, value = 0 }
