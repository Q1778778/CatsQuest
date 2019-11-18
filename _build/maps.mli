(** Raised when an unknown food is encountered. *)
exception UnknownFood of string

(** Raised when an unknown weapon is encountered. *)
exception UnknownWeapon of string

(*(**[size m] is the size of the map [m] in [(rows, columns)]. *)
  val size : t -> int * int

  (**[bound_check m r c] checks whether row [r] col [c] is within the 
  boundary of the map [m]. *)
  val bound_check : t -> int -> int -> bool*)

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
    name: string -> 
    link: string ->
    map_param
end


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

module MapParam: MP