module Solver (getProbabilityPerBoardState,getProbabibilityGridFromBoard,mergeMatrix,getMostProbableHit,GridCell(..),Grid,getProbabibilityGridFromBoard) where
-- a shared type file proably
import Board (BlockState (Hit, NotHit, Unknown), ShipSize,GameBoard,matrix, BlockPosition)

data GridCell = GridCell {state::BlockState, value::Int} deriving (Show,Eq)
type Grid = [[GridCell]]
type MaxEntry = (Int,(Int,Int))


instance Ord GridCell where
    compare (GridCell _ value1) (GridCell _ value2)
        | value1 < value2 = LT
        | value1 > value2 = GT
        | otherwise       = EQ


getProbabibilityGridFromBoard :: GameBoard -> Grid
getProbabibilityGridFromBoard board =  map (map mappedBlock) (matrix board)
    where mappedBlock block = GridCell{state=block,value=0}




transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)


-- this is horrible with time complexity but I do not know enough haskell for now lol
getMostProbableHit :: Grid -> BlockPosition
getMostProbableHit grid = snd  $ getBiggestEntry (minBound::Int, (-1,-1)) flattenedEntries
    where flattenedEntries = [ (value cellValue, (i, j))
                            | (i, row) <- zip [0..] grid
                            , (j, cellValue) <- zip [0..] row,
                            state cellValue == Unknown
                        ]
          getBiggestEntry::MaxEntry -> [MaxEntry] -> MaxEntry              
          getBiggestEntry maxEntry [] =  maxEntry
          getBiggestEntry currentEntry (x:xs)
            | fst currentEntry >= fst x  = getBiggestEntry currentEntry xs 
            | fst currentEntry < fst x = getBiggestEntry x xs   
            | otherwise = getBiggestEntry x xs 


increaseIfNoHits :: [GridCell] -> [GridCell]
increaseIfNoHits cells
  | notHitInRange = cells 
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
incVertically grid v =  transpose $ map (`incRow` v) $ transpose grid
    where incRow = increaseProbabilityHorizontallyRow


mergeMatrix :: Grid -> Grid -> Grid
mergeMatrix grid1 grid2 =
    [ [ combineCells cell1 cell2 | (cell1, cell2) <- zip row1 row2 ]
    | (row1, row2) <- zip grid1 grid2
    ]

-- Combine two cells by adding value from the first cell and preserving the state
combineCells :: GridCell -> GridCell -> GridCell
combineCells (GridCell state1 value1) (GridCell _ value2) = GridCell state1 (value1 + value2)

-- add a maybe type for the value and add nothing here
getProbabilityGridFromBoard :: GameBoard -> Grid
getProbabilityGridFromBoard board = map (map mappedBlock) (matrix board)
  where
    mappedBlock block = GridCell {state = block, value = 0}


getProbabilityPerShip :: Grid -> ShipSize -> Grid
getProbabilityPerShip grid size=  mergeMatrix horizontal vertical
    where horizontal = incHorizontally grid size
          vertical =  incVertically grid size


getProbabilityPerBoardState :: Grid -> [ShipSize] -> Grid
getProbabilityPerBoardState initial sizes = foldl mergeMatrix initial probabilityGrids
  where probabilityGrids = map (getProbabilityPerShip initial) sizes
