--
-- Uwe R. Zimmer
-- Australia 2012
--

module Drawing.Worlds.For_List_2D ( 
   draw_world -- :: List_2D Cell -> Float -> Picture
) where

import Data.Cell (Cell)
import Data.List_2D (List_2D, map_List_2D_w_context_to_list)
import Drawing.Cell (draw_cell)
import Graphics.Gloss (Picture (Pictures))

draw_world :: List_2D Cell -> Float -> Picture
draw_world world cell_size = Pictures (map_List_2D_w_context_to_list draw_cell cell_size world)
