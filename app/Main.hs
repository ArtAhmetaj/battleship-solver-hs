module Main (main) where

import Board(generateGameBoard,hitBoard)
import Data.Foldable (Foldable(fold, foldl'))

matrixIndexes :: Int -> Int -> [(Int, Int)]
matrixIndexes numRows numCols = [(i, j) | i <- [0..numRows-1], j <- [0..numCols-1]]


main :: IO ()
main = do
    board <- generateGameBoard 10 10
    let coordinates = matrixIndexes 10 10
    let finalBoard =  foldl hitBoard board coordinates
    let boardText = show finalBoard 
    print boardText
