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
    map: string ->
    food

  (**[get_name f] is the name of the food [f]  *)
  val get_name : food -> string

  (**[get_strength f] is the strength of the food [f]  *)
  val get_strength : food -> int

  (**[get_health f] is the health of the food [f]  *)
  val get_health : food -> int

  (**[get_loc f] is the location of the food [f]  *)
  val get_loc : food -> int * int

  (**[get_map f] is the map name of which the food [f] is currently in*)
  val get_map : food -> string
end


module Food : F = struct
  type food = {
    name : string;
    description : string;
    id : int;
    location : int * int;
    health : int;
    strength : int;
    map : string;
  }

  let constructor 
      ~row ~col ~health ~description ~name ~id ~strength ~map = {
    name = name;
    id = id;
    strength = strength;
    health = health;
    location = (col,row);
    description = description;
    map = map;
  }

  let get_name f = f.name

  let get_strength f = f.strength

  let get_health f = f.health

  let get_loc f = f.location

  let get_map f = f.map
end

