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

import qualified Data.Ordered_Lists_2D as OL2D
import qualified Transitions.For_Ordered_Lists_2D as FOL2D

get_y_values :: List_2D Cell -> [Y_Coord] -> [Y_Coord]
get_y_values list seen =
   case list of
      [] -> []
      (e, (x, y)) : t
         | y `elem` seen -> get_y_values t seen
         | otherwise -> y : get_y_values t (y:seen)

convert_List_2D_to_Placed_Elements :: List_2D Cell -> Y_Coord -> OL2D.Placed_Elements Cell
convert_List_2D_to_Placed_Elements world y =
   [(OL2D.Placed_Element x_e e) | (e, (x_e, y_e)) <- world, y_e == y]

convert_List_2D_to_Sparse_Line :: List_2D Cell -> Y_Coord -> OL2D.Sparse_Line Cell
convert_List_2D_to_Sparse_Line world y =
   OL2D.Sparse_Line y (convert_List_2D_to_Placed_Elements world y)

convert_List_2D_to_Ordered_Lists_2D :: List_2D Cell -> OL2D.Ordered_Lists_2D Cell
convert_List_2D_to_Ordered_Lists_2D world =
   [convert_List_2D_to_Sparse_Line world y | y <- get_y_values world []]

transition_world :: List_2D Cell -> List_2D Cell
transition_world world =
   OL2D.sparse_list_2D_to_list_2D (FOL2D.transition_world (convert_List_2D_to_Ordered_Lists_2D world))\