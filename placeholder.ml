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
