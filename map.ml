open Yojson.Basic.Util

exception UnknownFood of string
exception UnknownWeapon of string

(** The abstract type of values representing weapons. *)
type weapon = {
  id : int;
  position: float * float;
}

(** The abstract type of values representing foods. *)
type food = {
  idn : int;
  location: float * float;
  strength: int;
  health: int
}

type t = {
  size: float * float;
  foods: food list;
  weapons: weapon list
}

(**[to_tuple a b j] converts a json representation [j] into a tuple of ints
   [(i,j)], where [i] is the value associated with key [a] 
   and [j] is the value associated with key [b]. If [a] is not a defined key
   in [j], [(Null, [j])] will be returned. 
   Requires: [j] is a valid json representation.*)
let to_tuple a b j = (j |> member a |> to_float, j |> member b |> to_float)

(**[weapon_of_json j] is the the weapon the [j] represents. 
   Requires: [j] is a valid json representation.*)
let weapon_of_json j = {
  id = j |> member "id" |> to_int;
  position = j |> member "location" |> to_tuple "rows" "cols"
}

(**[food_of_json j] is the the food the [j] represents. 
   Requires: [j] is a valid json representation.*)
let food_of_json j = {
  idn = j |> member "id" |> to_int;
  location = j |> member "location" |> to_tuple "rows" "cols";
  strength = j |> member "strength" |> to_int;
  health = j |> member "health" |> to_int;
}

let size m = m.size

(**[map_sort_uniq f lst] maps a list [lst] with a function [f] and 
   turns it into a sorted set-like list *)
let map_sort_uniq f lst = lst 
                          |> List.map f 
                          |> List.sort_uniq Stdlib.compare


let weapon_ids m = map_sort_uniq (fun w -> w.id) m.weapons

let food_ids m = map_sort_uniq (fun f -> f.idn) m.foods

let bound_check m r c = 
  let rows = fst m.size in 
  let cols = snd m.size in 
  r < 0.0 || r > rows || c < 0.0 || c > cols

