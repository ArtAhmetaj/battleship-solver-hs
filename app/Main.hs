module Main (main) where

import Solver(getProbabilityPerBoardState,initializeGrid)

main :: IO ()
main = do
    let result =  getProbabilityPerBoardState (initializeGrid 10 10) [2,2,3,3,4,4,5,5]
    putStrLn $ show result
