--
-- Uwe R. Zimmer
-- Australia 2012
--

module Drawing.Worlds.For_Ordered_Lists_2D ( 
   draw_world -- :: Ordered_Lists_2D Cell -> Float -> Picture
) where

import Data.Cell (Cell)
import Data.Ordered_Lists_2D (Ordered_Lists_2D, map_Ordered_Lists_2D_w_context_to_list)
import Drawing.Cell (draw_cell)
import Graphics.Gloss (Picture (Pictures))

draw_world :: Ordered_Lists_2D Cell -> Float -> Picture
draw_world world cell_size = Pictures (map_Ordered_Lists_2D_w_context_to_list draw_cell cell_size world)
