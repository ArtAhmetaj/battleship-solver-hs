module Main (main) where

import Board(generateGameBoard,hitBoard)

main :: IO ()
main = do
    board <- generateGameBoard 10 10
    print (hitBoard board (0,0))
