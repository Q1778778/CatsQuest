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
