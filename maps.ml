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
  val constructor : 
    row:int -> 
    col:int -> 
    name:string -> 
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
      ~row ~col ~health ~description ~name ~id ~strength = 
    {
      name = name;
      id = id;
      strength = strength;
      health = health;
      location = (row,col);
      description = description;
    }
end

module Weapon : W = struct 
  type weapon = {
    weapon_name : string;
    weapon_description : string;
    id : int;
    weapon_loc : int * int;
    strength: int;
  }

  let constructor 
      ~row ~col ~name ~id ~description ~strength = {
    weapon_name = name;
    id = id;
    weapon_description = description;
    weapon_loc = (row,col);
    strength = strength;
  }
end

module Map_Param : MP = struct 
  type map_param = {
    name : string;
    col : int; 
    row : int; 
  }
  let constructor 
      ~row ~col ~name = {
    name = name;
    row = row;
    col = col;
  }
end

type t = {
  size: int * int;
  weapons: Weapon.weapon list;
  foods: Food.food list
}

exception UnknownFood of string
exception UnknownWeapon of string

(**[to_tuple a b j] converts a json representation [j] into a tuple of ints
   [(i,j)], where [i] is the value associated with key [a] 
   and [j] is the value associated with key [b]. If [a] is not a defined key
   in [j], [(Null, [j])] will be returned. 
   Requires: [j] is a valid json representation.*)
let to_tuple a b j = (j |> member a |> to_int, j |> member b |> to_int)

let size m = m.size

let bound_check m r c = 
  let rows = fst m.size in 
  let cols = snd m.size in 
  r < 0 || r > rows || c < 0 || c > cols