--
-- Uwe R. Zimmer
-- Australia 2012
--

module Transitions.For_List_2D (
   transition_world -- :: List_2D Cell -> List_2D Cell
) where

import Data.Cell
-- import Data.Cell (Cell (Head, Tail, Conductor, Empty))
import Data.Coordinates
import Data.List_2D


-- Replace this function with something more meaningful:

transition_world :: List_2D Cell -> List_2D Cell
transition_world world = map (transitionCell (filter isHead world)) world
  where
    transitionCell :: List_2D Cell -> (Cell, Coord) -> (Cell, Coord)
    transitionCell heads (cellType, coord) = case cellType of
      Head -> (Tail, coord)
      Tail -> (Conductor, coord)
      Empty -> (Empty, coord)
      Conductor
        | length (local_elements_list coord heads) `elem` [1, 2] -> (Head, coord)
        | otherwise -> (Conductor, coord)
    
    isHead :: (Cell, Coord) -> Bool
    isHead (Head, _) = True
    isHead _ = False
