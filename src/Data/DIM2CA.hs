module Data.DIM2CA where
import qualified Data.Array.Repa as Rp
import qualified Data.Vector.Unboxed as V
import Data.List
import Control.Monad.ST
import Debug.Trace

type Field = Rp.Array Rp.U Rp.DIM2 Bool
data Dim2CA = Dim2CA {field::Field,rule :: Rule , isTorused :: Bool, generation :: Int}
data NeighborType = Neumann | Moore deriving Eq
data Rule = Rule{neighbor :: NeighborType,birthCellNeighbor :: [Int],activeCellNeighbor :: [Int]}
mapIndex :: (Int -> Int) -> Rp.DIM2 -> Rp.DIM2
mapIndex f (Rp.Z Rp.:.i Rp.:.j) = Rp.Z Rp.:.(f i) Rp.:.(f j)
getWidth :: Dim2CA -> Int
getWidth = (\(Rp.Z Rp.:._ Rp.:.j) -> j) . Rp.extent . field
getHeight :: Dim2CA -> Int
getHeight = (\(Rp.Z Rp.:.i Rp.:._) -> i) . Rp.extent . field
torusedIndex :: Int -> Int -> Int
torusedIndex i j
  | i == -1 = j - 1 
  | otherwise = i `mod` j
--torusedIndex :: Dim2CA -> Rp.DIM2 -> Rp.DIM2
--torusedIndex ca index@(Rp.Z Rp.:.i Rp.:.j)
--  | isTorused ca && i == -1 && j == -1 = (Rp.Z Rp.:. (1- getHeight ca) Rp.:.(1- getWidth ca))
--  | isTorused ca && i == -1 = (Rp.Z Rp.:.(1 - getHeight ca) Rp.:.j)
--  | isTorused ca && j == -1 = (Rp.Z Rp.:.i Rp.:.(1 - getWidth ca))
--  | isTorused ca  = (Rp.Z Rp.:.(i `mod` (getHeight ca)) Rp.:.(j `mod` (getWidth ca)))
--  | otherwise = index

neighbors :: Dim2CA -> Rp.DIM2 -> [Rp.DIM2]
neighbors ca (Rp.Z Rp.:.x Rp.:. y) = if (neighbor $  rule ca) == Moore 
                                             then ([tuple2Index (i+x,j+y)|i<-[-1,0,1],j<-[-1,0,1]] \\ [tuple2Index (x,y)])
                                             else  map (tuple2Index) [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

tuple2Index :: (Int,Int) -> Rp.DIM2 
tuple2Index (i,j)  = Rp.Z Rp.:.i Rp.:.j

index2Tuple :: Rp.DIM2 -> (Int,Int)
index2Tuple (Rp.Z Rp.:. i Rp.:. j) = (i,j)

applyRule :: Dim2CA -> Bool -> [Bool] -> Bool
applyRule ca  cellStatus bools = trace(show(activeCellCount))$ if cellStatus then isActive (activeCellNeighbor$ rule ca) activeCellCount else isBirth (birthCellNeighbor$ rule ca) activeCellCount
  where
    isActive ::  [Int] -> Int -> Bool
    isActive ns n = n `elem` ns
    isBirth :: [Int] -> Int -> Bool
    isBirth ns n = n `elem` ns
    activeCellCount = length $ filter (True ==) bools

updateLines :: Dim2CA -> Dim2CA
updateLines ca = runST $ do
  updatedlines <- Rp.computeUnboxedP $ Rp.traverse (field ca) id (cellUpdate ca)
  pure ca{field = updatedlines}
    where
      cellUpdate :: Dim2CA -> (Rp.DIM2 -> Bool) -> Rp.DIM2 ->Bool
      cellUpdate ca _ index = applyRule ca ((field ca) Rp.! index) (map ((field ca Rp.!) . (\(Rp.Z Rp.:.i Rp.:.j) -> (Rp.Z Rp.:.(torusedIndex i (getHeight ca) ) Rp.:.(torusedIndex j (getWidth ca))))) (neighbors ca index))
reformField :: Dim2CA -> Dim2CA
reformField  = id
incrementGeneration :: Dim2CA -> Dim2CA
incrementGeneration ca = ca{generation = 1 + generation ca}
fieldContentUpdate  :: Dim2CA -> Dim2CA
fieldContentUpdate  = incrementGeneration . reformField . updateLines

cellChangedField :: Dim2CA -> Rp.DIM2 -> Dim2CA 
cellChangedField ca index = runST $ do
  fieldContent' <- Rp.computeUnboxedP $! Rp.traverse (field ca) id $ changeCell (field ca) index
  pure ca{field = fieldContent'}
  where
    changeCell :: Field -> Rp.DIM2 -> (Rp.DIM2 -> Bool) -> Rp.DIM2 -> Bool
    changeCell field index _ index' = if index == index' then  not $ (field) Rp.! index else (field) Rp.! (index')
