module CA(CA) where

class CA a where
  getUpdateLine :: a -> f -- 更新するべき列を取得(DIM1ならラスト1列、DIM2なら全部)
  updateAndAddLine :: a -> f -> f -- (applyRuleして更新した列を全体に加える,DIM2ならidでいいですが)
  reformField :: a -> f -> a -- 変形する必要があるならここで
  applyRule :: a -> [b] -> b -- ルールの適用
  neighbors :: a -> b -> [b] -- 近傍の取得
  fieldContentUpdate :: a -> a -- フィールドを更新し、更新されたフィールドを持つaを返す
  fieldContentUpdate ca = reformField ca (updateAndAddLine ca (getUpdateLine ca))
