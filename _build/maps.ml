open Yojson.Basic.Util

module type F = sig 
  (** The abstract type of values representing foods. *)
  type food

  (** Constructor of a food *)
  val constructor : 
    row:int ->
    col:int ->
    health:int ->
    description:string -> 
    name:string -> 
    id:int -> 
    strength:int -> 
    food
end

module type W = sig 
  (** The abstract type of values representing weapons. *)
  type weapon

  (** Constructor of a weapon *)
  val constructor : 
    row:int -> 
    col:int -> 
    name:string -> 
    id:int -> 
    description:string -> 
    strength: int->
    weapon
end

module type MP = sig 
  (** The abstract type of values representing map params. *)
  type map_param 

  (** Constructor of a map param *)
  val single_map_element_constructor : 
    name:string -> 
    link: string ->
    map_param
end

module Food : F = struct
  type food = {
    name : string;
    description : string;
    id : int;
    location : int * int;
    health : int;
    strength : int;
  }

  let constructor 
      ~row ~col ~health ~description ~name ~id ~strength = {
    name = name;
    id = id;
    strength = strength;
    health = health;
    location = (col,row);
    description = description;
  }
end

module Weapon : W = struct 
  type weapon = {
    weapon_name : string;
    weapon_description : string;
    id : int;
    weapon_loc : int * int; (*col, row*)
    strength: int;
  }

  let constructor 
      ~row ~col ~name ~id ~description ~strength = {
    weapon_name = name;
    id = id;
    weapon_description = description;
    weapon_loc = (col,row);
    strength = strength;
  }
end

module MapParam : MP = struct 
  type map_param = {
    name: string; (*this represents a jpg name for this element*)
    link: string; (*link here represents another map file 
                    that current map element is linked to. If there is NO such
                    linked map, link will be empty string ""*)
  }
  let single_map_element_constructor ~name ~link = {
    link = link;
    name = name;
  }
end

exception UnknownFood of string
exception UnknownWeapon of string

(**[to_tuple a b j] converts a json representation [j] into a tuple of ints
   [(i,j)], where [i] is the value associated with key [a] 
   and [j] is the value associated with key [b]. If [a] is not a defined key
   in [j], [(Null, [j])] will be returned. 
   Requires: [j] is a valid json representation.*)

type t = {
  size : int * int; (*total col * total rows *)
  name : string; (*this map name*)
  (**[|((col, row), map_param)|]*)
  map_params: ((int * int) * MapParam.map_param) array; 
}

let map_constructor 
    ~size ~name ~all_map_param = {
  size = size; (*total col * total rows *)
  name = name; (*this map name *)
  map_params = all_map_param; 
}

let to_tuple a b j = (j |> member a |> to_int, j |> member b |> to_int)

let size m = m.size

let bound_check m r c = 
  let rows = fst m.size in 
  let cols = snd m.size in 
  r < 0 || r > rows || c < 0 || c > cols