open Enemy
open Player
open Map
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

type map = int * int

type state = {
  mutable player: player;
  mutable map: map;
  mutable items: item list;
  mutable enemies: enemy list;
}

(**[probabilty s] produces a bool based on probabilty [s]

Require:
[s] mod 0.1. 0.1 <= [s] <= 1.0*)
let probabilty s = 
  (* 0 <= x <= 10 *)
   let x = Int.to_float (Random.int 11) in
   Float.to_int (s *. 10) > x

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


let enemy_builder j id: enemy =
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
          (Enemy.single_skill_constructor ~skill_name ~skill_strength)) lst in
    Enemy.constructor ~pos ~level ~exp ~name
      ~hp ~id ~descr ~max_hp ~skills
  )

let browse_one_enemy_json j id: enemy = 
  if (contains j "witch" || contains j "minion" || contains j "goblin")
  then enemy_builder (Yojson.Basic.from_file j) id
  else failwith "something wrong with browse_dir_enemy. Check it"


(**[main_engine_enemy ()] read all enemy json files in current directory*)
let main_engine_enemy : unit -> enemy list =
  fun () -> (
      try
        let incr x = x := !x + 1 in
        let count = 
          let counter = ref 0 in fun () -> incr counter; !counter in
        List.map  (fun x -> browse_one_enemy_json x (count ()))
          ([] |> browse_dir_enemy (Unix.opendir ".")) 
      with Unix.Unix_error(Unix.ENOENT, _ ,_ ) ->
        raise (Failure "NONE of 'enemy' json exists"))

let main_engine_player: unit -> (player) =
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

let food_list_builder jsons: item list = 
  List.map (fun j -> let id = 
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

let weapon_list_builder jsons: item list = 
  List.map (fun j -> let id = 
                       let count = 
                         let counter = ref 0 in fun () -> incr counter; 
                           !counter in count () in
             let name = j |> member "name" |> to_string in
             let description = j |> member "description" |> to_string in
             let location = j |> member "location" in
             let strength = j |> member "strength"|>to_int in
             let row = location |> member "row"|>to_int in
             let col = location |> member "col" |>to_int in
             Weapon (Maps.Weapon.constructor ~strength ~col ~row 
                       ~description ~name ~id)) jsons

let main_engine_map : unit -> (item list * (int * int)) = 
  let rec read_map handler =
    match Unix.readdir handler with
    | exception _ -> Unix.closedir handler; 
      failwith "map.json is not in current directory"
    | s -> if s = "map.json"
      then 
        let json = s |> Yojson.Basic.from_file in
        let rows = json |> member "size" |> member "rows" |> to_int in
        let cols = json |> member "size" |> member "cols" |> to_int in
        let weapon_lst = json |> member "weapons" 
                         |> to_list |> weapon_list_builder in
        let food_lst = json |> member "foods" 
                       |> to_list 
                       |> food_list_builder in
        (weapon_lst @ food_lst, (rows, cols))
      else read_map handler in
  fun () -> (read_map (Unix.opendir "."))

let init (): state =
  let items, loc = main_engine_map () in
  {
    items = items;
    player = main_engine_player ();
    map = loc;
    enemies = main_engine_enemy ();
  }

let game_state= init ()



let change_item item (s:state) = s.items <- item

let change_player player s = s.player <- player

let change_enemies enemies s = s.enemies <- enemies

let get_item_list s = s.items

let get_player s = s.player

let get_enemies s = s.enemies