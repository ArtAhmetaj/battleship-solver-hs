module Solver (getProbabilityPerBoardState,initializeGrid) where

-- duplicated types will see what i do about it later
type ShipSize = Int
data CellState = Hit | NotHit | Unknown deriving (Show,Eq)
data GridCell = GridCell {state::CellState, value::Int} deriving (Show,Eq)
type Grid = [[GridCell]]


initializeGrid :: Int -> Int -> Grid
initializeGrid width height = replicate height (replicate width GridCell {state=NotHit, value=0})




transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)



-- increaseBy2 :: [Int] -> [Int]
-- increaseBy2 [] = []  -- Base case: an empty list results in an empty list.
-- increaseBy2 (x:y:rest) = (x+1) : (y+1) : increaseBy2 rest
-- increaseBy2 (x:rest) = [x]  -- Handle the case where there's only one element left.    

increaseIfNoHits :: [GridCell] -> [GridCell]
increaseIfNoHits cells
  | notHitInRange   = cells
  | anyHitInRange =   [x { value = value x + 2, state=NotHit } | x <- cells]
  | otherwise = [x { value = value x + 1, state=NotHit } | x <- cells]
  where notHitInRange = any (\x -> state x == NotHit) cells
        anyHitInRange = any (\x -> state x == Hit) cells


increaseProbabilityHorizontallyRow :: [GridCell] -> Int -> [GridCell]
increaseProbabilityHorizontallyRow [] _ = []
increaseProbabilityHorizontallyRow row v = increaseIfNoHits a ++ increaseProbabilityHorizontallyRow b v
    where (a,b) = splitAt v row


incHorizontally :: [[GridCell]] -> Int -> [[GridCell]]
incHorizontally grid v = map (`incRow` v) grid
    where incRow = increaseProbabilityHorizontallyRow

incVertically :: [[GridCell]] -> Int -> [[GridCell]]
incVertically grid v = map (`incRow` v) $ transpose grid
    where incRow = increaseProbabilityHorizontallyRow


merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

mergeMatrix :: [[a]] -> [[a]] -> [[a]]
mergeMatrix  xs [] = xs
mergeMatrix [] ys = ys
mergeMatrix (x:xs) (y:ys) = merge x y  : merge xs ys


getProbabilityPerShip :: [[GridCell]] -> ShipSize -> [[GridCell]]
getProbabilityPerShip grid size=  mergeMatrix horizontal vertical
    where horizontal = incHorizontally grid size
          vertical =  incVertically grid size


getProbabilityPerBoardState :: [[GridCell]] -> [ShipSize] -> [[GridCell]]
getProbabilityPerBoardState initial sizes = foldl mergeMatrix initial probabilityGrids
  where probabilityGrids = map (getProbabilityPerShip initial) sizes
