open Enemy
open Player
open Map
open Yojson.Basic.Util
open Str


(*!!!!!!!!!!!!!!!!!!!!!!!!!                                              *)
(*some functions below required an id, which is created by the functions *)
(*instead of contained in json                                           *)

type enemy = 
  | Witch of Witch.s 
  | Goblin of Goblin.t 
  | Minion of Minion.t
  | Delete

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
  map: map;
  mutable items: item list;
  mutable enemies: enemy list;
}

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


let witch_builder j id: enemy =
  Witch (
    let name = "witch" in
    let id = (Int.to_string (id + 1)) in
    let descr = j |> member "description" |> to_string in
    let exp = j |> member "experience" |> to_int in
    let level = j |> member "level" |> to_int in
    (* random init pos *)
    let pos = ((Random.int 10)+1, (Random.int 10)+1) in
    let strength = j |> member "strength" |> to_int in
    let hp = j |> member "HP" |> to_int in
    let lst = j |> member "special skills" in
    let skills_descr = lst |> member "description" |> to_string in
    let skills_strength = lst |> member "strength" |> to_int in
    Witch.constructor ~pos ~level ~exp ~name
      ~skills_strength ~strength ~hp ~id ~descr ~skills_descr 
  )

let goblin_or_minion_builder j id: enemy =
  Minion (let name = j |> member "name"|> to_string in
          let id = (Int.to_string (id + 1)) in
          let descr = j |> member "description" |> to_string in
          let exp = j |> member "experience" |> to_int in
          let level = j |> member "level" |> to_int in
          let pos = ((Random.int 10)+1, (Random.int 10)+1) in 
          let strength = j |> member "strength" |> to_int in
          let hp = j |> member "HP" |> to_int in
          Minion.constructor ~name ~pos ~level ~exp ~strength ~hp ~id ~descr )


let browse_one_enemy_json j id: enemy = 
  if (contains j "witch") then witch_builder (Yojson.Basic.from_file j) id
  else if (contains j "minion" || contains j "goblin")
  then goblin_or_minion_builder (Yojson.Basic.from_file j) id
  else failwith "invalid input json name"


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
        let keys = player_json |> member "keys" 
                              |> to_list 
                              |> List.map (fun x -> Yojson.Basic.to_string x) in
        let experience = 0 in
        Player.constructor ~health ~level ~strength  ~row ~col ~experience ~keys
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
             let row = location |> member "row"|>to_int in
             let col = location |> member "col" |>to_int in
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
             let strength = j |> member "strength" in
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

let main (): state =
  let items, loc = main_engine_map () in
  {
    items = items;
    player = main_engine_player ();
    map = loc;
    enemies = main_engine_enemy ();
  }

let game_state= main()


let change_item item (s:state) = s.items <- item

let change_player player s = s.player <- player

let change_enemies enemies s = s.enemies <- enemies

let get_item_list s = s.items

let get_player s = s.player

let get_enemies s = s.enemies