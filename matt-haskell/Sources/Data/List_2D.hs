--
-- Uwe R. Zimmer
-- Australia 2012
--

module Data.List_2D (

   List_2D,
      {- the central data structure of this module: 
      A single list containing elements with their associated coordinates.
      No order on the coordinates is assumed or preseverd -}

   singleton_world,               -- :: Element_w_Coord e -> List_2D e
   insert_element,                -- :: Element_w_Coord e -> List_2D e -> List_2D e
   combine_List_2D,               -- :: List_2D e -> List_2D e -> List_2D e
   read_element,                  -- :: Coord -> List_2D e -> Maybe e
   element_occurrence,            -- :: Eq e => e -> List_2D e -> Int   

   first_coord,                   -- ::          List_2D e -> Maybe Coord
   next_coord,                    -- :: Coord -> List_2D e -> Maybe Coord  

   remove_elements_less_than_x,   -- :: X_Coord -> List_2D e -> List_2D e
   remove_elements_less_than_y,   -- :: Y_Coord -> List_2D e -> List_2D e
   
   local_lines,                         -- :: Y_Coord -> List_2D e -> List_2D e 
      -- +/- 1 y coordinate lines neighbourhood (including the y line itself, if it exists)
   local_elements,                -- :: Coord -> List_2D e -> List_2D e 
   local_elements_list,           -- :: Coord -> List_2D e -> [e]
      -- +/- 1 (x, y) coordinates elements neighbourhood - including the element at (x, y) itself, if it exists

   map_list_2D,                   -- :: (Element_w_Coord e ->      b) ->      List_2D e -> List_2D b
   map_list_2D_to_list,           -- :: (Element_w_Coord e ->      b) ->      List_2D e -> [b]
   map_list_2D_w_context,         -- :: (Element_w_Coord e -> c -> b) -> c -> List_2D e -> List_2D b
   map_List_2D_w_context_to_list, -- :: (Element_w_Coord e -> c -> b) -> c -> List_2D e -> [b]
      -- apply a function with or without a context to all elements in a list_2D structure 
      -- and return the results in the same structure or in a flat list
   
   size,                          -- :: List_2D e -> Int
   
   list_2D_to_list,               -- :: List_2D e -> [e]
      -- roll out a list_2D structure to a flat list containing the elements only

) where

import Data.Coordinates (Distance, Coord, Element_w_Coord, X_Coord, Y_Coord)


type List_2D e = [Element_w_Coord e]


singleton_world :: Element_w_Coord e -> List_2D e
singleton_world element = [element]

insert_element :: Element_w_Coord e -> List_2D e -> List_2D e
insert_element element world = element: world

combine_List_2D :: List_2D e -> List_2D e -> List_2D e
combine_List_2D list world = list ++ world

read_element :: Coord -> List_2D e -> Maybe e
read_element (x, y) world = case world of
   (element, (x_e, y_e)): cs 
      |    x == x_e 
        && y == y_e -> Just element
      | otherwise   -> read_element (x, y) cs
   [] -> Nothing

element_occurrence :: Eq e => e -> List_2D e -> Int
element_occurrence element list = case list of
   (local_element, _): cs
      | local_element == element -> 1 + element_occurrence element cs
      | otherwise                ->     element_occurrence element cs
   [] -> 0

first_coord :: List_2D e -> Maybe Coord
first_coord world = scan_world_first world Nothing

   where
      scan_world_first :: List_2D e -> Maybe Coord -> Maybe Coord
      scan_world_first world candidate = case world of
         (_, (x_e, y_e)): cs -> case candidate of
            Just (x_c, y_c) 
               |     y_e <  y_c 
                 || (y_e == y_c && x_e < x_c) -> scan_world_first cs (Just (x_e, y_e))
               | otherwise                    -> scan_world_first cs candidate
            Nothing -> scan_world_first cs (Just (x_e, y_e))
         [] -> candidate

next_coord :: Coord -> List_2D e -> Maybe Coord
next_coord coord world = scan_world_next coord world Nothing

   where
      scan_world_next :: Coord -> List_2D e -> Maybe Coord -> Maybe Coord
      scan_world_next (x, y) world candidate = case world of
         (_, (x_e, y_e)): cs -> case candidate of
            Just (x_c, y_c)
               |    (y_e > y   || (y_e == y   && x_e > x  )) 
                 && (y_e < y_c || (y_e == y_c && x_e < x_c)) -> scan_world_next (x, y) cs (Just (x_e, y_e))
               | otherwise                                   -> scan_world_next (x, y) cs candidate
            Nothing
               | y_e > y || (y_e == y && x_e > x) -> scan_world_next (x, y) cs (Just (x_e, y_e))
               | otherwise                        -> scan_world_next (x, y) cs candidate
         [] -> candidate

remove_elements_less_than_x :: X_Coord -> List_2D e -> List_2D e
remove_elements_less_than_x x world = case world of 
   (element, (x_e, y_e)): cs
      | x_e < x   ->                        remove_elements_less_than_x x cs
      | otherwise -> (element, (x_e, y_e)): remove_elements_less_than_x x cs
   [] -> []

remove_elements_less_than_y :: Y_Coord -> List_2D e -> List_2D e
remove_elements_less_than_y y world = case world of 
   (element, (x_e, y_e)): cs
      | y_e < y   ->                        remove_elements_less_than_y y cs
      | otherwise -> (element, (x_e, y_e)): remove_elements_less_than_y y cs
   [] -> []

local_lines :: Y_Coord -> List_2D e -> List_2D e
local_lines y world = read_neighbouring_lines y 1 world
   
   where
      read_neighbouring_lines :: Y_Coord -> Distance -> List_2D e -> List_2D e
      read_neighbouring_lines y dist list = case list of
         (element, (x_e, y_e)): cs
            | abs (y_e - y) <= dist -> (element, (x_e, y_e)): read_neighbouring_lines y dist cs
            | otherwise             ->                        read_neighbouring_lines y dist cs
         [] -> []    

local_elements :: Coord -> List_2D e -> List_2D e
local_elements (x, y) list = read_neighbours (x, y) 1 list

   where
      read_neighbours :: Coord -> Distance -> List_2D e -> List_2D e
      read_neighbours (x, y) dist list = case list of
         (element, (x_e, y_e)): cs 
            |    abs (x_e - x) <= dist 
              && abs (y_e - y) <= dist -> (element, (x_e, y_e)): read_neighbours (x, y) dist cs
            | otherwise                ->                        read_neighbours (x, y) dist cs
         [] -> []
      
local_elements_list :: Coord -> List_2D e -> [e]
local_elements_list (x, y) list = read_neighbours_list (x, y) 1 list

   where
      read_neighbours_list :: Coord -> Distance -> List_2D e -> [e]
      read_neighbours_list (x, y) dist list = case list of
         (element, (x_e, y_e)): cs 
            |    abs (x_e - x) <= dist 
              && abs (y_e - y) <= dist -> element: read_neighbours_list (x, y) dist cs
            | otherwise                ->          read_neighbours_list (x, y) dist cs
         [] -> []

map_list_2D :: (Element_w_Coord e -> b) -> List_2D e -> List_2D b
map_list_2D f list = case list of
   (element, (x_e, y_e)): cs -> (f (element, (x_e, y_e)), (x_e, y_e)): map_list_2D f cs
   []                        -> []
   
map_list_2D_to_list :: (Element_w_Coord e -> b) -> List_2D e -> [b]
map_list_2D_to_list f list = map f list

map_list_2D_w_context :: (Element_w_Coord e -> c -> b) -> c -> List_2D e -> List_2D b
map_list_2D_w_context f context list = case list of
   (element, (x_e, y_e)): cs -> (f (element, (x_e, y_e)) context, (x_e, y_e)): map_list_2D_w_context f context cs
   []                        -> []
   
map_List_2D_w_context_to_list :: (Element_w_Coord e -> c -> b) -> c -> List_2D e -> [b]
map_List_2D_w_context_to_list f context list = case list of
   c: cs -> f c context: map_List_2D_w_context_to_list f context cs
   []    -> []
   
size :: List_2D e -> Int
size list = length list 

list_2D_to_list :: List_2D e -> [e]
list_2D_to_list list = case list of
   (element, _): cs -> element: list_2D_to_list cs
   []               -> []