module Main (main) where

import Board (BlockPosition, GameBoard, areAllShipsDead, generateGameBoard, hitBoard, matrix, ships, shipsByLength)
import Data.Foldable (Foldable (fold, foldl'))
import Solver (Grid, GridCell (..), getMostProbableHit, getProbabibilityGridFromBoard, getProbabilityPerBoardState, mergeMatrix)
import Text.Printf (printf)

main :: IO ()
main = do
  board <- generateGameBoard 10 10
  --   lets start with middle of board
  let middleSpot = (5,5)
  gameLoop board middleSpot 0

gameLoop :: GameBoard -> BlockPosition -> Int -> IO ()
gameLoop board positionToHit tries = do
  let newBoard = hitBoard board positionToHit
  if areAllShipsDead newBoard
    then printf "Congratulations you have sunk all the ships in %d tries" tries
    else do
      let gridCell = getProbabilityGridFromBoard newBoard
      let calculatedGrid = getProbabilityPerBoardState gridCell shipsByLength
      let newEntryToHit = getMostProbableHit calculatedGrid
      print newEntryToHit
      print tries
      print $ ships board 
      gameLoop newBoard newEntryToHit (tries + 1)

-- add a maybe type for the value and add nothing here
getProbabilityGridFromBoard :: GameBoard -> Grid
getProbabilityGridFromBoard board = map (map mappedBlock) (matrix board)
  where
    mappedBlock block = GridCell {state = block, value = 0}
