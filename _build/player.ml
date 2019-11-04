<<<<<<< HEAD
open Map
open Yojson.Basic.Util
module Player = struct
=======

open Yojson.Basic.Util

module type P = sig
  (** The abstract type of values representing the player's game state. *)
  type t

  (** The abstract type of values representing weapons. *)
  type weapon

  (** The abstract type of values representing foods. *)
  type food

  (** The type representing the result of an attempted movement. *)
  type result = Legal of t | Illegal of string

  (** [location p] is the current location of player [p]. *)
  val location : t -> int * int

  (** [health p] is the current health of player [p]. *)
  val health : t -> int

  (** [experience p] is the current experience value of player [p]. *)
  val experience : t -> int 

  (** [strength p] is the current strength of player [p]. *)
  val strength : t -> int

  (**[compare_weapons w1 w2] returns [0] if [w1]'s id equal to [w2]'s id,
     [-1] if less than, and [1] if greater than.  *)
  val compare_weapons : weapon -> weapon -> int 

  (**[compare_foods f1 f2] returns [0] if [f1]'s id equal to [f2]'s id,
     [-1] if less than, and [1] if greater than.  *)
  val compare_foods : food -> food -> int 

  (** [move_north p m] returns the new player state [r] after p moves north.
      [r] is [Legal p'] if player [p] is able to make the move, and [Illegal]
      otherwise. *)
  val move_north : t -> Maps.t -> result

  (** [move_south p m] returns the new player state [r] after p moves south.
      [r] is [Legal p'] if player [p] is able to make the move, and [Illegal]
      otherwise. *)
  val move_south : t -> Maps.t -> result

  (** [move_east p m] returns the new player state [r] after p moves east.
      [r] is [Legal p'] if player [p] is able to make the move, and [Illegal]
      otherwise. *)
  val move_east : t -> Maps.t -> result

  (** [move_west p m] returns the new player state [r] after p moves west.
      [r] is [Legal p'] if player [p] is able to make the move, and [Illegal]
      otherwise. *)
  val move_west : t -> Maps.t -> result

  (** [eat p] is [Legal p'] if there is food at the neighboring coordinates
       of player at state [p], and [Illegal] otherwise. [p'] is the updated
       state after the player eats the food. *) 
  val eat : t -> result 

  (** [retrieve_weapon p] is [Legal p'] if there is a weapon at the neighboring 
      coordinates of player at state [p], and [Illegal] otherwise. 
      [p'] is the updated state after the player retrieves the weapon. *) 
  val retrieve_weapon : t -> result

  (** [advance_level p] is [Legal p'] if the player [p] has reached the 
       experience value for that level alone, and [Illegal] otherwise.*)
  val advance_level : t -> result 


end 



module Player : P = struct

>>>>>>> 9c03733ef42629255138751123a48c90570a408b
  (** The abstract type of values representing keyboard keys. *)
  type key = Up | Down | Left | Right | W | A | S | D | Space | Null

  type weapon = {
    id : int;
    weapon_loc: int * int;
  }

  type food = {
    idn : int;
    food_loc: int * int;
    strength: int;
    health: int
  }

  type map_state = {
    food_list : food list;
    weapon_list : weapon list
  }

  type t = {
    location : int * int;
    strength : int;
    health : int;
    level : int;
    experience : int;
    weapons : weapon list;
    keys : string list;
    map_state : map_state
  }

  (**[to_tuple a b j] converts a json representation [j] into a tuple of ints
     [(i,j)], where [i] is the value associated with key [a] 
     and [j] is the value associated with key [b]. If [a] is not a defined key
     in [j], [(Null, [j])] will be returned. 
     Requires: [j] is a valid json representation.*)
  let to_tuple a b j = (j |> member a |> to_int, j |> member b |> to_int)

  (**[weapon_of_json j] is the the weapon the [j] represents. 
     Requires: [j] is a valid json representation.*)
  let weapon_of_json j = {
    id = j |> member "id" |> to_int;
    weapon_loc = j |> member "location" |> to_tuple "rows" "cols"
  }

  (**[food_of_json j] is the the food the [j] represents. 
     Requires: [j] is a valid json representation.*)
  let food_of_json j = {
    idn = j |> member "id" |> to_int;
    food_loc = j |> member "location" |> to_tuple "rows" "cols";
    strength = j |> member "strength" |> to_int;
    health = j |> member "health" |> to_int;
  }
  let location p = p.location

  let health p = p.health

  let experience p = p.experience

  let strength p = p.strength

  let max_strength = 100

  let max_health = 100

  let row p = fst p.location

  let col p = snd p.location

  (**[match_keys s] returns the equivalent enum value for the parsed key
     string [s] from the json file. *)
  let match_keys = function 
    | "right" -> Right
    | "left" -> Left
    | "up" -> Up
    | "down" -> Down
    | "w" -> W
    | "a" -> A
    | "s" -> S
    | "d" -> D
    | "space" -> Space
    | _ -> Null

  let map_state j = {
    food_list = j |> member "foods" |> to_list |> List.map food_of_json;
    weapon_list = j |> member "weapons" |> to_list |> List.map weapon_of_json
  }

  (**[player_of_json j] is the the player the [j] represents. 
     Requires: [j] is a valid json representation.*)
  let player_of_json j = {
    location =  j |> member "location" |> to_tuple "row" "col";
    strength = j |> member "strength" |> to_int;
    health = j |> member "health" |> to_int;
    level = j |> member "level" |> to_int;
    weapons = [];
    experience = 0;
    keys = j |> member "keys" |> to_list |> filter_string;
    map_state = map_state j
  }

  type result = Legal of t | Illegal of string

  (* [move p m r c] is a new state for which the player [p] moves north,
     south, east, or west in map [m]; i.e. moves north or south by 
     [row_diff], moves east or west by [col_diff] *)
  let move p m row_diff col_diff = 
    let row = row p in 
    let col = col p in
<<<<<<< HEAD
    if bound_check m row col then 
=======
    if Maps.bound_check m row col then 
>>>>>>> 9c03733ef42629255138751123a48c90570a408b
      Legal {
        p with 
        location = (row+row_diff, col+col_diff)
      }
    else Illegal "Cannot move out of the map!"

  let location p = p.location

  let move_north p m = move p m (-1) 0

  let move_south p m = move p m 1 0

  let move_east p m = move p m 0 1

  let move_west p m = move p m 0 (-1)

  let exists_food ms l = 
    List.exists (fun f -> l = f.food_loc) ms.food_list

  (**[get_food ms l] returns the food object within the [food] list in
     map state [ms] at location [l].
  *)
  let get_food ms l = 
    List.find (fun f -> f.food_loc = l) ms.food_list

  let rm_food ms l = 
    List.filter (fun f -> f.food_loc <> l) ms.food_list

  let exists_weapon ms l = 
    List.exists (fun w -> l = w.weapon_loc) ms.weapon_list

  (**[get_food ms l] returns the weapon object within the [weapon] list in
      map state [ms] at location [l]. *)
  let get_weapon ms l = 
    List.find (fun w -> w.weapon_loc = l) ms.weapon_list

  let compare_weapons a b = 
    Stdlib.compare a.id b.id 

  let compare_foods a b = 
    Stdlib.compare a.idn b.idn 

  (** [rm_weapon ms l] returns the updated [weapon] list at map state [ms], 
      with the [l] removed from the list. *)
  let rm_weapon ms l = 
    List.filter (fun w -> w.weapon_loc <> l) ms.weapon_list

  (**[neighbors p] is the list of all neighboring cells of [p]. 
     A neighboring coordinate of [(r,c)] is a coordinate
     [(r',c')] where [r'] and [r] differs by at most one and [c'] and [c] 
     differs by at most one, and [(r',c')] is not equal to [(r,c)]. *)
  let neighbors p = 
    let off = [(-1,-1); (-1,0); (-1,1); (0,-1); (0,1); (1,-1); (1,0); (1,1)] in 
    List.map (fun (i,j) -> (i+row p, j+col p)) off 

  let eat p  = 
    try 
      let food_loc = neighbors p |>
                     List.find (fun l -> exists_food p.map_state l) in 
      let food = get_food p.map_state food_loc in 
      Legal {
        p with
        map_state = {
          p.map_state with 
          food_list = rm_food p.map_state food_loc
        };
        strength = p.strength + food.strength;
        health = p.health + food.health;
      }
    with 
    | Not_found -> Illegal "cannot pick up food!"

  let retrieve_weapon p = 
    try 
      let weapon_loc = neighbors p |> 
                       List.find (fun l -> exists_weapon p.map_state l) in 
      let weapon = get_weapon p.map_state weapon_loc in 
      Legal {
        p with 
        map_state = {
          p.map_state with 
          weapon_list = rm_weapon p.map_state weapon_loc
        };
        weapons = weapon :: p.weapons
      }
    with 
      Not_found -> Illegal "cannot pick up a weapon!"

  let advance_level p = 
    let lev = p.level in 
    let experience_qual = 100 * lev in 
    if p.experience >= experience_qual then
      Legal {
        p with 
        level = lev + 1;
        experience = p.experience mod experience_qual;
      }
    else Illegal ("cannot advance level without experience " ^ 
                  (experience_qual |> string_of_int))

  (* [attack p e] returns a new player state [r] after which player [p] attacks
     an enemy [e]. [r] is [Legal p'] if the enemy successfully gets attacked, 
     and Illegal otherwise. *)
  (* let attack p e = 
     let enemy_loc = neighbors p |> 
     List.find (fun l -> exists_enemy p.map_state l) in 
     let enemy = get_enemy 
  *)
end