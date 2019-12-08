open Player

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
    gainables: Player.skill list ->
    weapon

  (**[get_name w] is the name of the weapon [w]  *)
  val get_name : weapon -> string

  val get_description: weapon -> string

  (**[get_strength w] is the strength of the weapon [w]  *)
  val get_strength : weapon -> int

  (**[get_loc w] is the location of the weapon [w]  *)
  val get_loc : weapon -> int * int

  val set_loc : weapon -> int * int -> unit

  val get_gainables: weapon -> Player.skill list
end

module Weapon : W = struct 
  type weapon = {
    name : string;
    description : string;
    id : int;
    strength: int;
    gainables: Player.skill list;
    mutable weapon_loc : int * int; (*col, row*)
  }

  let constructor ~row ~col ~name ~id ~description ~strength ~gainables = {
    name = name;
    id = id;
    description = description;
    weapon_loc = (col,row);
    strength = strength;
    gainables = gainables;
  }

  let get_name w = w.name

  let get_description w = w.description

  let get_strength w = w.strength

  let get_loc w = w.weapon_loc

  let set_loc w loc = w.weapon_loc <- loc

  let get_gainables w = w.gainables
end
