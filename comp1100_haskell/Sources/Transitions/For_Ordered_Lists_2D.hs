--
-- Uwe R. Zimmer
-- Australia 2012
--

module Transitions.For_Ordered_Lists_2D (
   transition_world -- :: Ordered_Lists_2D Cell -> Ordered_Lists_2D Cell
) where

import Data.Cell (Cell)
-- import Data.Cell (Cell (Head, Tail, Conductor, Empty))
-- import Data.Coordinates
import Data.Ordered_Lists_2D


-- Replace this function with something more meaningful:
      
transition_world :: Ordered_Lists_2D Cell -> Ordered_Lists_2D Cell
transition_world world = world                     