module Data.CA(CA(getUpdateLine,updateAndAddLine,reformField,applyRule,neighbors,fieldContentUpdate,incrementGeneration)) where
import qualified Data.Array.Repa as Rp
class CA a where
  getUpdateLine :: a -> Int-- 更新するべき列を取得(DIM1ならラスト1列、DIM2なら全部)
  updateAndAddLine :: a -> Int ->  Rp.Array Rp.U Rp.DIM2 Bool-- (applyRuleして更新した列を全体に加える,DIM2ならidでいいですが)
  reformField :: a -> Rp.Array Rp.U Rp.DIM2 Bool -> a -- 変形する必要があるならここで
  applyRule :: a -> [Bool] -> Bool -- ルールの適用
  neighbors :: a -> Int -> [Int] -- 近傍の取得
  fieldContentUpdate :: a -> a -- フィールドを更新し、更新されたフィールドを持つaを返す
  fieldContentUpdate ca = reformField ca (updateAndAddLine ca (getUpdateLine ca))

  incrementGeneration :: a -> a
