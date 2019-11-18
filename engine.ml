open Enemy
open Player
open Map
open Maps
open Yojson.Basic.Util

(*!!!!!!!!!!!!!!!!!!!!!!!!!                                              *)
(*some functions below required an id, which is created by the functions *)
(*instead of contained in json                                           *)

type enemy = 
  | Enemy of Enemy.t
  | Deleted

type player = 
  | Player of Player.t 
  | Died

type item = 
  | Weapon of Maps.Weapon.weapon 
  | Food of Maps.Food.food
  | Eaten  (*Eaten means the food is eaten. We cannot destroy weapon in game *)

type one_map_param = Maps.Map_Param.map_param

type state = {
  mutable player: player;
  mutable map_size: int * int; (* (col, row) *)
  mutable current_map_name: string;
  mutable current_all_map_params: one_map_param array;
  mutable items: item array;
  mutable enemies: enemy array;
}

(**[probabilty s] produces a bool based on probabilty [s]

   Require:
   [s] mod 0.1. 0.1 <= [s] <= 1.0*)
let probabilty s = 
  (* 0 <= x <= 10 *)
  let x = Int.to_float (Random.int 11) in
  x <  (s *. 10.0)

(**[choose_skill_random t] is a (skill name, skill strength) for enemy [t]*)
let choose_skill_random s =
  let rec inner_chooser lst finished= 
    match lst with
    | [] -> finished
    | h::d -> let name, prob, strength = h in
      if probabilty prob && (strength > (finished |> snd))
      then (name, strength) |> inner_chooser d
      else inner_chooser d finished in
  inner_chooser 
    (s|>Enemy.get_all_skills_name_prob_and_strength_to_assoc_list) []

(**[contains s s1] is true if [s] contains substring [s1]. false otherwise*)
let contains s1 s2 =
  let rec counter count = 
    (*let re = Str.regexp_string s2
      in try ignore (Str.search_forward re s1 0); true
      with Not_found -> false*)
    if (String.length s1) - count < String.length s2 
    then false
    else if (String.sub s1 count (String.length s2)) = s2
    then true
    else counter (count + 1) in
  counter 0

(**[browse_dir_enemy h lst] is a list of enemy json files 
   extracted from the directory handler [h]

   Require:
   !!!!!!!!!!!!!!!!!!!!!!!!!!
   Valid enemy name format:
   "enemy-*.json", in which "*" represents different enemy names*)
let rec browse_dir_enemy (handler: Unix.dir_handle)(lst: string list)=
  match Unix.readdir handler with
  | exception _ -> 
    Unix.closedir handler; 
    lst |> List.rev
  | h -> let pos = String.length h in 
    if pos > 11
    && (String.sub h 0 6 = "enemy-") 
    && (String.sub h (pos-5) 5) = ".json" 
    then browse_dir_enemy handler (h::lst) 
    else browse_dir_enemy handler lst


let single_enemy_builder j id: enemy =
  Enemy (
    let name = j |> member "name" |> to_string in
    let id = (Int.to_string (id + 1)) in
    let descr = j |> member "description" |> to_string in
    let exp = j |> member "experience" |> to_int in
    let level = j |> member "level" |> to_int in
    (* random init pos *)
    let pos = ((Random.int 10)+1, (Random.int 10)+1) in
    let hp = j |> member "HP" |> to_int in
    let max_hp = hp in
    let lst = j |> member "skills" |> to_list in
    let skills = 
      List.map (fun x -> 
          let skill_name = x |> member "name" |> to_string in
          let skill_strength = x |> member "strength" |> to_int in
          let skill_probability = x |> member "probability" |> to_float in
          (Enemy.single_skill_constructor ~skill_name ~skill_strength
             ~skill_probability)) lst in
    Enemy.constructor ~pos ~level ~exp ~name
      ~hp ~id ~descr ~max_hp ~skills
  )

let browse_one_enemy_json j id: enemy = 
  if (contains j "witch" || contains j "minion" || contains j "goblin")
  then single_enemy_builder (Yojson.Basic.from_file j) id
  else failwith "something wrong with browse_dir_enemy. Check it"


(**[main_engine_enemy ()] read all enemy json files in current directory*)
let main_engine_enemy : unit -> enemy array =
  fun () -> (
      try
        let incr x = x := !x + 1 in
        let count = 
          let counter = ref 0 in fun () -> incr counter; !counter in
        Array.map  (fun x -> browse_one_enemy_json x (count ()))
          ([] |> browse_dir_enemy (Unix.opendir ".")) 
      with Unix.Unix_error(Unix.ENOENT, _ ,_ ) ->
        raise (Failure "NONE of 'enemy' json exists"))

let main_engine_player: unit -> player =
  let rec read_map handler =
    match Unix.readdir handler with
    | exception _ -> Unix.closedir handler; 
      failwith "player.json is not in current directory"
    | s -> if s = "player.json"
      then let player_json = s |> Yojson.Basic.from_file in
        let level = player_json |> member "level" |> to_int in
        let strength = player_json |> member "strength" |> to_int in
        let health = player_json |> member "health" |> to_int in
        let location = player_json |> member "location" in
        let row = location |> member "row" |> to_int in
        let col = location |> member "col" |> to_int in
        let experience = 0 in
        Player.constructor ~health ~level ~strength  ~row ~col ~experience 
      else read_map handler in
  fun () -> Player (read_map (Unix.opendir "."))

let food_array_builder jsons: item array = 
  Array.map (fun j -> let id = 
                       let count = 
                         let counter = ref 0 in fun () -> incr counter; 
                           !counter in count () in
             let health = j |> member "health" |> to_int in
             let strength = j |> member "strength" |> to_int in
             let name = j |> member "name" |> to_string in
             let description = j |> member "description" |> to_string in
             let location = j |> member "location" in
             let row = location |> member "row"|> to_int in
             let col = location |> member "col" |> to_int in
             Food (Maps.Food.constructor ~col ~row ~health 
                     ~description ~name ~id ~strength)) jsons

let weapon_array_builder jsons: item array = 
  Array.map (fun j -> let id = 
                       let count = 
                         let counter = ref 0 in fun () -> incr counter; 
                           !counter in count () in
             let name = j |> member "name" |> to_string in
             let description = j |> member "description" |> to_string in
             let location = j |> member "location" in
             let strength = j |> member "strength"|> to_int in
             let row = location |> member "row"|> to_int in
             let col = location |> member "col" |> to_int in
             Weapon (Maps.Weapon.constructor ~strength ~col ~row 
                       ~description ~name ~id)) jsons

(**[parse_dims s] parses [s] and returns [(col, row)]. 
   Requires: [s] is in the form ["# cols, # rows"]
*)
let parse_dims s = 
  let rows = List.nth (String.split_on_char ',' s) 0 in 
  let cols = List.nth (String.split_on_char ',' s) 1 in 
  (cols |> int_of_string, rows |> int_of_string)

let map_param_array_builder jsons : map_param array = 
  Array.map (fun j -> let id = 
                        let count = 
                          let counter = ref 0 in fun () -> incr counter; 
                            !counter in count () in 
              let name = j |> member "name" |> to_string in 
              let loc = j |> member "loc" |> to_string in
              let link = j |> member "link" |> to_string in 
              let col = parse_dims loc |> fst in 
              let row = parse_dims loc |> snd in 
      Map_Param (Maps.Map_Param.single_map_element_constructor ~row ~col ~name
              ~link)) jsons


let main_engine_map : unit -> item array = 
  let rec read_map handler =
    match Unix.readdir handler with
    | exception _ -> Unix.closedir handler; 
      failwith "map.json is not in current directory"
    | s -> if s = "map.json"
      then 
        let json = s |> Yojson.Basic.from_file in
        let weapon_array = json |> member "weapons" 
                         |> to_list |> weapon_array_builder in
        let food_array = json |> member "foods" 
                       |> to_list 
                       |> food_array_builder in
        (weapon_array @ food_array)
      else read_map handler in
  fun () -> (read_map (Unix.opendir "."))

let main_engine_map_param : unit -> string * int * int * map_param array = 
  let rec read_map handler = 
    match Unix.readdir handler with
    | exception _ -> Unix.closedir handler;
      failwith "map-param.json is not in current directory"
    | s -> if s = "map.json"
      then 
        let json = s |> Yojson.Basic.from_file in 
        let name = json |> member "name" |> to_string in 
        let size = json |> member "size" |> to_string |> parse_dims in 
        let rows = snd size in 
        let cols = fst size in 
        let params = json |> member "picture" |> to_list |> Array.of_list 
                     |>  map_param_array_builder in 
        (name, rows, cols, params)
      else read_map handler in 
  fun () -> (read_map (Unix.opendir "."))


let init (): state =
  let items = main_engine_map () in
  let name, col, row, map_param_array = main_engine_map_param () in
  {
    items = items;
    player = main_engine_player ();
    map_size = (col, row);
    current_map_name = name;
    current_all_map_params = map_param_array;
    enemies = main_engine_enemy ();
  }

let game_state= init ()

let change_item item (s:state) = s.items <- item

let change_player player s = s.player <- player

let change_enemies enemies s = s.enemies <- enemies

let get_items_array s = s.items

let get_player s = s.player

let get_enemies s = s.enemies

let get_current_map_name s = s.name

let get_all_map_params s = s.current_all_map_params

let get_current_map_size s = s.map_size