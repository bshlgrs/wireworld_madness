--
-- Uwe R. Zimmer
-- Australia 2012
--

module Transitions.For_Ordered_Lists_2D (
   transition_world -- :: Ordered_Lists_2D Cell -> Ordered_Lists_2D Cell
) where

import Data.Cell (Cell (Head, Tail, Conductor, Empty))
import Data.Coordinates
import Data.Ordered_Lists_2D

--
-- Matthew Alger (u5365162)
-- Australia 2013
--

get_next_cell :: Cell -> Coord -> Ordered_Lists_2D Cell -> Cell
get_next_cell cell coord world =
   case cell of
      Head -> Tail
      Tail -> Conductor
      Conductor
         | element_occurrence Head (local_elements coord world) `elem` [1, 2] -> Head
         | otherwise -> Conductor
      Empty -> Empty

step_through_cells :: Y_Coord -> Placed_Elements Cell -> Ordered_Lists_2D Cell -> Placed_Elements Cell
step_through_cells y cells world =
   case cells of
      (Placed_Element x cell) : t -> (Placed_Element x (get_next_cell cell (x, y) world)) : step_through_cells y t world
      [] -> []

step_through_lines :: Ordered_Lists_2D Cell -> Ordered_Lists_2D Cell -> Ordered_Lists_2D Cell
step_through_lines rows world =
   case rows of
      (Sparse_Line y cells) : t -> Sparse_Line y (step_through_cells y cells world) : step_through_lines t world
      [] -> []

transition_world :: Ordered_Lists_2D Cell -> Ordered_Lists_2D Cell
transition_world world =
   step_through_lines world world