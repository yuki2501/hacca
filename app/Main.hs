module Main where
import Data.CA
import Data.DIM1CA
import Gloss.Render
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Array.Repa as Rp
import Control.Monad.ST
main :: IO()
main = do
  let field = initfield
  let rule110 = Dim1CA field 110 1 
  play (InWindow "Rule110" (1000,1000)(120,120)) black 10 rule110 renderField eventHandler tickHandler

eventHandler :: Event -> Dim1CA -> Dim1CA
eventHandler _ ca = ca

tickHandler :: Float -> Dim1CA -> Dim1CA
tickHandler _ ca = fieldContentUpdate ca

initfield :: Rp.Array Rp.U Rp.DIM2 Bool
initfield = let falsecell =  Rp.computeUnboxedS $ Rp.fromFunction (Rp.Z Rp.:.1 Rp.:.1) mkFalse
             in Rp.computeUnboxedS $ falsecell Rp.++ Rp.computeUnboxedS (Rp.fromFunction (Rp.Z Rp.:.1 Rp.:.1) mkTrue) Rp.++ falsecell
            where
              mkTrue _ = True
              mkFalse _ = False

renderField :: Dim1CA -> Picture
renderField ca = renderArray white 1 (field ca)


