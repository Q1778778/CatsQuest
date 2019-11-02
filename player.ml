open Map
open Yojson.Basic.Util

module Player = struct

  (** The abstract type of values representing keyboard keys. *)
  type key = Up | Down | Left | Right | W | A | S | D | Space | Null

  type t = {
    location : float * float;
    strength : int;
    health : int;
    level : int;
    experience : int;
    experience_qual : int;
    keys : string list;
  }

  let location p = p.location

  let health p = p.health

  let experience p = p.experience

  let strength p = p.strength

  let experience_qual p = p.experience_qual

  let row p = fst p.location

  let col p = snd p.location

  (**[to_tuple a b j] converts a json representation [j] into a tuple of ints
     [(i,j)], where [i] is the value associated with key [a] 
     and [j] is the value associated with key [b]. If [a] is not a defined key
     in [j], [(Null, [j])] will be returned. 
     Requires: [j] is a valid json representation.*)
  let to_tuple a b j = (j |> member a |> to_float, j |> member b |> to_float)

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
    experience_qual = j |> member "experience" |> to_int;
    keys = j |> member "keys" |> to_list |> filter_string;
  }

  type result = Legal of t | Illegal 

  (* [move p m r c] is a new state for which the player [p] moves north,
     south, east, or west in map [m]; i.e. moves north or south by 
     [row_diff], moves east or west by [col_diff] *)
  let move p m row_diff col_diff = 
    let row = row p in 
    let col = col p in
    if bound_check m row col then 
      Legal {
        p with 
        location = (row+.row_diff, col+.col_diff)
      }
    else Illegal

  let move_north p m = move p m (-1.0) 0.0

  let move_south p m = move p m 1.0 0.0

  let move_east p m = move p m 0.0 1.0

  let move_west p m = move p m 0.0 (-1.0)


end