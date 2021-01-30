module Gloss.Render(renderArray) where
import Graphics.Gloss
import qualified Data.Array.Repa as Rp
import qualified Data.Vector.Unboxed as V
import Control.Monad.ST
type CellSize = Int
activeCellIndex :: Rp.Array Rp.U Rp.DIM2 Int -> (V.Vector (Int,Int))
activeCellIndex field = let withIndexArray = runST $ Rp.computeUnboxedP $ Rp.traverse field id (withIndex field)
                         in V.map snd $ V.filter ((1 ==) . fst ) (Rp.toUnboxed withIndexArray)
                         
  where
    withIndex :: Rp.Array Rp.U Rp.DIM2 Int -> (Rp.DIM2 -> Int) -> Rp.DIM2 -> (Int,(Int,Int))
    withIndex  field _ index@(Rp.Z Rp.:.i Rp.:.j) = (field Rp.! index,(i+1,j+1))
renderArray :: Color -> CellSize -> Rp.Array Rp.U Rp.DIM2 Int -> Picture
renderArray cellColor cellSize field = let field4Render = activeCellIndex field
                                       in pictures (map cell2Picture (V.toList field4Render))
  where
    cell2Picture :: (Int,Int) -> Picture
    cell2Picture (i,j) = color cellColor $ cellTranslate (i,j) $ rectangleSolid (fromIntegral cellSize) (fromIntegral cellSize)
    cellTranslate :: (Int,Int) -> Picture -> Picture
    cellTranslate (i,j) = translate (fromIntegral ((i*cellSize)) - fromIntegral(((\(Rp.Z Rp.:.i Rp.:._)-> i)(Rp.extent field))`div`2*cellSize))(fromIntegral((((\(Rp.Z Rp.:._ Rp.:.j) -> j)(Rp.extent field))`div`2*cellSize)) + fromIntegral(-(j*cellSize)))

