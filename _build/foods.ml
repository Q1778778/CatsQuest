open Player

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

  (**[get_name f] is the name of the food [f]  *)
  val get_name : food -> string

  (**[get_strength f] is the strength of the food [f]  *)
  val get_strength : food -> int

  (**[get_health f] is the health of the food [f]  *)
  val get_health : food -> int

  (**[get_loc f] is the location of the food [f]  *)
  val get_loc : food -> int * int

  val set_loc : food -> int * int -> unit
end


module Food : F = struct
  type food = {
    name : string;
    description : string;
    id : int;
    mutable location : int * int;
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

  let set_loc f loc = f.location <- loc
end

