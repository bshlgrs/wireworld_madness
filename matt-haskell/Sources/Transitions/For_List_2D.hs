--
-- Uwe R. Zimmer
-- Australia 2012
--

module Transitions.For_List_2D (
   transition_world -- :: List_2D Cell -> List_2D Cell
) where

import Data.Cell (Cell (Head, Tail, Conductor, Empty))
import Data.Coordinates
import Data.List_2D

--
-- Matthew Alger (u5365162)
-- Australia 2013
--

get_next_cell :: Cell -> Coord -> List_2D Cell -> Cell
get_next_cell cell coord world =
   case cell of
      Head -> Tail
      Tail -> Conductor
      Conductor
         | element_occurrence Head (local_elements coord world) `elem` [1, 2] -> Head
         | otherwise -> Conductor
      Empty -> Empty

step_through_cells :: List_2D Cell -> List_2D Cell -> List_2D Cell
step_through_cells cells world =
   case cells of
      (cell, coord) : t -> (get_next_cell cell coord world, coord) : step_through_cells t world
      [] -> []

transition_world :: List_2D Cell -> List_2D Cell
transition_world world =
   step_through_cells world world