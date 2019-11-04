open Map
open Yojson.Basic.Util
module Player = struct
  (** The abstract type of values representing keyboard keys. *)
  type key = Up | Down | Left | Right | W | A | S | D | Space | Null


  type t = {
    location : int * int;
    strength : int;
    health : int;
    level : int;
    experience : int;
    keys : string list;
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

  (**[player_of_json j] is the the player the [j] represents. 
     Requires: [j] is a valid json representation.*)
  let player_of_json j = {
    location =  j |> member "location" |> to_tuple "row" "col";
    strength = j |> member "strength" |> to_int;
    health = j |> member "health" |> to_int;
    level = j |> member "level" |> to_int;
    experience = 0;
    keys = j |> member "keys" |> to_list |> filter_string;
  }

  type result = Legal of t | Illegal of string

  (* [move p m r c] is a new state for which the player [p] moves north,
     south, east, or west in map [m]; i.e. moves north or south by 
     [row_diff], moves east or west by [col_diff] *)
  let move p m row_diff col_diff = 
    let row = row p in 
    let col = col p in
    if bound_check m row col then 
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