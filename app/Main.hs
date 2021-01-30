module Main where
import Data.DIM1CA
import Gloss.Render
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.ST
import Debug.Trace
import qualified Data.Array.Repa as Rp
-- example of DIM1 Cellular Automaton (rule110)
main :: IO()
main = do
  let field = initfield
  let rule110 = Dim1CA field 110 1 
  play (InWindow "Rule110" (1000,1000)(120,120)) black 60 rule110 renderField eventHandler tickHandler

eventHandler :: Event -> Dim1CA -> Dim1CA
eventHandler _ ca = ca

tickHandler :: Float -> Dim1CA -> Dim1CA
tickHandler _ ca = trace(show(generation ca))fieldContentUpdate ca

initfield :: Rp.Array Rp.U Rp.DIM2 Bool
initfield = let falsecell =  Rp.computeUnboxedS $ Rp.fromFunction (Rp.Z Rp.:.1 Rp.:.1) (\ _ -> False)
             in Rp.computeUnboxedS $ falsecell Rp.++ Rp.computeUnboxedS (Rp.fromFunction (Rp.Z Rp.:.1 Rp.:.1) (\ _ -> True)) Rp.++ falsecell

renderField :: Dim1CA -> Picture
renderField ca = renderArray white 1 (Rp.computeUnboxedS $ Rp.map (bool2Int) (field ca))
  where
    bool2Int b 
      | b = 1 :: Int
      | otherwise = 0 :: Int


