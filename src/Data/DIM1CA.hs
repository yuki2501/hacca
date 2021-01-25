module Data.DIM1CA where
import qualified Data.Array.Repa as Rp
import Data.Word
import qualified Data.Vector.Unboxed as V
import Control.Monad.ST
type Field = Rp.Array Rp.U Rp.DIM2 Bool
data Dim1CA = Dim1CA{field :: Field, wolframCode::Word8,generation :: Int}
getUpdateLine :: Dim1CA -> Int-- 更新するべき列を取得(DIM1ならラスト1列、DIM2なら全部)
getUpdateLine ca =  ((\(Rp.Z Rp.:.i Rp.:._) -> i-1)$Rp.extent$field ca)
applyRule :: Dim1CA -> [Bool] -> Bool -- ルールの適用
applyRule ca bools = let nowPattern = bools2Int $ bools
                         rules = V.fromList $ int2Rule $ wolframCode ca
                        in (rules) V.! nowPattern
  where
    bools2Int :: [Bool] -> Int
    bools2Int = binary2Int . map (bool2Int)
    bool2Int :: Bool -> Int
    bool2Int b = if b then 1 else 0
    binary2Int :: [Int] -> Int
    binary2Int [] = 0
    binary2Int (x:xs) = if x == 1 then 2 ^ (length (x:xs) - 1) + binary2Int xs else binary2Int xs
updateAndAddLine :: Dim1CA -> Int ->  Dim1CA-- (applyRuleして更新した列を全体に加える,DIM2ならidでいいですが)
updateAndAddLine ca line = runST $ do
  updatedline <- let newline= (runST $ Rp.computeUnboxedP $ Rp.slice (field ca) (Rp.Z Rp.:.line Rp.:.Rp.All))
                  in Rp.computeUnboxedP $ Rp.traverse (runST $ Rp.computeUnboxedP $ Rp.slice (field ca) (Rp.Z Rp.:.line Rp.:.Rp.All)) id (cellUpdate (generation ca) newline)
  addedLine <- Rp.computeUnboxedP $ (Rp.traverse2 (field ca) updatedline (\(Rp.Z Rp.:.i Rp.:.j)-> \(Rp.Z Rp.:.j) -> (Rp.Z Rp.:.i+1 Rp.:.j))) (updateCell ca updatedline)
  pure ca{field = addedLine}
    where
      cellUpdate :: Int -> Rp.Array Rp.U Rp.DIM1 Bool -> (Rp.DIM1 -> Bool) -> Rp.DIM1 -> Bool
      cellUpdate gen line _ index@(Rp.Z Rp.:.i) = (applyRule ca) ( map ((line Rp.!) .(\i -> (Rp.Z Rp.:.i)) . ((flip circledField (gen)))) (neighbors ca i))
      updateCell :: Dim1CA -> Rp.Array Rp.U Rp.DIM1 Bool -> (Rp.DIM2 -> Bool) -> (Rp.DIM1 -> Bool) -> Rp.DIM2 -> Bool
      updateCell ca line _ _ index@(Rp.Z Rp.:.i Rp.:.j) = if (i >= generation ca) then line Rp.! (Rp.Z Rp.:.(circledField j (generation ca))) else (field ca) Rp.! (Rp.Z Rp.:.(circledField i (generation ca)) Rp.:.(circledField j (generation ca)))
reformField :: Dim1CA -> Dim1CA
reformField ca = ca{field = runST $ Rp.computeUnboxedP $ Rp.traverse (field ca) (\(Rp.Z Rp.:.i Rp.:.j) -> (Rp.Z Rp.:.i Rp.:.j+2)) (slideCell ca)}
  where
    slideCell :: Dim1CA -> (Rp.DIM2 -> Bool) -> Rp.DIM2 -> Bool
    slideCell ca _ (Rp.Z Rp.:.i Rp.:.j) = if j == 0 || j == 2 *((generation ca) +1) then False else (field ca) Rp.! (Rp.Z Rp.:.i Rp.:. (j-1))
neighbors :: Dim1CA -> Int -> [Int]
neighbors _  i = [i-1,i,i+1]
incrementGeneration :: Dim1CA -> Dim1CA
incrementGeneration ca = ca{generation = 1 + (generation ca)}
fieldContentUpdate :: Dim1CA -> Dim1CA
fieldContentUpdate  ca = incrementGeneration $ reformField  $ updateAndAddLine  ca $ getUpdateLine ca 

int2Rule :: Word8 -> [Bool]
int2Rule i = int2nbit (fromIntegral i) 8
  where
    -- nbitの場合を実装する
    int2nbit :: Int -> Int -> [Bool]
    int2nbit _ 0 = []
    int2nbit i n = let (d,m) = divMod i 2
                    in int2bool m : int2nbit d (n-1)
    int2bool :: Int -> Bool
    int2bool i = i == 1
circledField :: Int -> Int -> Int
circledField i gen 
  |i < 0 = (1+(2*gen)) -1
  |i > (1+(2*gen)) - 1 = 0
  |otherwise = i
