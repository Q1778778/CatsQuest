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
  val get_name : food -> string

  val get_strength : food -> int

  val get_health : food -> int

  val get_loc : food -> int * int
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

  val get_name : weapon -> string

  val get_strength : weapon -> int

  val get_loc : weapon -> int * int

end

module type MP = sig 
  (** The abstract type of values representing map params. *)
  type map_param = {
    name: string; (*this represents a jpg name for this element*)
    link: string; (*link here represents another map file 
                    that current map element is linked to. If there is NO such
                    linked map, link will be empty string ""*)
  }

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

  let get_name f = f.name

  let get_strength f = f.strength

  let get_health f = f.health

  let get_loc f = f.location
end

module Weapon : W = struct 
  type weapon = {
    name : string;
    description : string;
    id : int;
    weapon_loc : int * int; (*col, row*)
    strength: int;
  }

  let constructor 
      ~row ~col ~name ~id ~description ~strength = {
    name = name;
    id = id;
    description = description;
    weapon_loc = (col,row);
    strength = strength;
  }

  let get_name w = w.name

  let get_strength w = w.strength

  let get_loc w = w.weapon_loc
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

let size m = m.size

let bound_check m c r = 
  let cols = fst m.size in 
  let rows = snd m.size in 
  c > 0 && c <= cols && r > 0 && r <= rows