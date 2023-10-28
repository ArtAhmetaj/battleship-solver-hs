module Lib where

import System.Random

data BlockState = Hit | NotHit
type BlockPosition = (Int,Int)

data BlockWithPosition = BlockWithPosition{position::BlockPosition, hit::BlockState}
type Matrix = [[BlockState]]
type Ship = [BlockWithPosition]
data GameBoard = GameBoard{matrix::Matrix, ships::[Ship]}

maxMatrixSize  = 64

-- randomInRange :: (Random a, RandomGen g) => a -> a -> g -> (a, g)
-- randomInRange low high = randomR (low, high)

-- createRandomShipForMatrix :: Matrix -> Int -> Ship
-- createRandomShipForMatrix m v = []
--     where randomNumber = randomInRange 0 64
--           isFreeBlock p = not shipPartExistsInBlock p m


-- shipPartExistsInBlock  :: BlockPosition -> Matrix -> Bool
-- shipPartExistsInBlock p m =   any $ map shipContainsBlock m p 



-- shipContainsBlock :: Ship -> BlockPosition -> Bool
-- shipContainsBlock sh b = any $ filter (==b) sh 