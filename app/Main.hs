module Main (main) where

import Board (BlockPosition, GameBoard, areAllShipsDead, generateGameBoard, hitBoard, shipsByLength)
import Solver (getMostProbableHit, getProbabibilityGridFromBoard, getProbabilityPerBoardState)
import Text.Printf (printf)

main :: IO ()
main = do
  board <- Board.generateGameBoard 10 10
  initialMove <- calculateMove board
  gameLoop board initialMove 0

calculateMove :: Board.GameBoard -> IO Board.BlockPosition
calculateMove board = do
  let gridCell = getProbabibilityGridFromBoard board
  let calculatedGrid = getProbabilityPerBoardState gridCell Board.shipsByLength
  let newEntryToHit = getMostProbableHit calculatedGrid
  return newEntryToHit

gameLoop :: Board.GameBoard -> Board.BlockPosition -> Int -> IO ()
gameLoop board positionToHit tries = do
  let newBoard = Board.hitBoard board positionToHit
  let newTries = tries + 1
  if Board.areAllShipsDead newBoard
    then do
      printf "Congratulations, you have sunk all the ships in %d tries" newTries
      
    else do
      move <- calculateMove newBoard
      gameLoop newBoard move newTries