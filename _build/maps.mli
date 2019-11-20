(*(**[size m] is the size of the map [m] in [(rows, columns)]. *)
  val size : t -> int * int

  (**[bound_check m r c] checks whether row [r] col [c] is within the 
  boundary of the map [m]. *)
  val bound_check : t -> int -> int -> bool*)

module type F = sig 
  (** The abstract type of values representing foods. *)
  type food = {
    name : string;
    description : string;
    id : int;
    location : int * int;
    health : int;
    strength : int;
  }

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
    type weapon = {
    weapon_name : string;
    weapon_description : string;
    id : int;
    weapon_loc : int * int; (*col, row*)
    strength: int;
  }

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
  type map_param = {
    name: string; (*this represents a jpg name for this element*)
    link: string; (*link here represents another map file 
                    that current map element is linked to. If there is NO such
                    linked map, link will be empty string ""*)
  }

  (** Constructor of a map param *)
  val single_map_element_constructor : 
    name: string -> 
    link: string ->
    map_param
end

module MapParam : MP 

(** The abstract type of values representing maps. *)
type t = {
  size : int * int; 
  name : string; 
  map_params: ((int * int) * MapParam.map_param) array;
}

val map_constructor: 
  size: int * int -> 
  name: string -> 
  all_map_param: ((int * int) * MapParam.map_param) array ->
  t

module Food: F 

module Weapon: W

val bound_check: t -> int -> int -> bool

