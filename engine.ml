open Enemy
open Player
open Map
open Maps
open Yojson.Basic.Util

(*!!!!!!!!!!!!!!!!!!!!!!!!!                                                 *)
(*some constructors below required an id, which is created by the functions *)
(*instead of contained in json                                              *)

type enemy = 
  | Enemy of Enemy.t
  | Deleted

type player = 
  | Player of Player.t 
  | Died

type item = 
  | Weapon of Maps.Weapon.weapon 
  | Food of Maps.Food.food
  | Null (*once a weapon or food has been taken, this weapon becomes null *)

type map_param = Maps.MapParam.map_param

type current_map = Maps.t

exception UnknwonFood of string

exception UnknownWeapon of string

exception SuccessExit

type state = {
  mutable player: player;
  mutable food_inventory: item array;
  mutable weapon_inventory: item array;
   (* we can have several maps linked in one game, and [all_maps] store
   all maps in one game. *)
  
  mutable current_map: current_map;
  mutable all_maps: current_map array;
  (**[current_map_in_all_maps] counts which map the player is currently in.
  i.e. the index in [all_maps]*)
  mutable current_map_in_all_maps: int; 
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
  let rec inner_chooser lst finished = 
    match lst, finished with
    | [], _ -> finished
    | one::d, m -> 
      let name, prob, strength = one in
      let name1, strength1 = m in
      if probabilty prob && strength > strength1
      then (name, strength) |> inner_chooser d
      else inner_chooser d finished in
  let skills = s |> Enemy.get_all_skills_name_prob_and_strength_to_assoc_list in
  let first_name, _, first_strength = List.hd skills in
  inner_chooser skills ((first_name,first_strength))

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
  jsons |> List.map (fun j -> let id = 
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
                     ~description ~name ~id ~strength))
        |> Array.of_list

let weapon_array_builder jsons: item array = 
  jsons |> List.map (fun j -> let id = 
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
                       ~description ~name ~id))
        |> Array.of_list

(**[parse_dims s] parses [s] and returns [(col, row)]. 
   Requires: [s] is in the form ["# cols, # rows"]
*)
let parse_dims s = 
  let rows = List.nth (String.split_on_char ',' s) 0 in 
  let cols = List.nth (String.split_on_char ',' s) 1 in 
  (cols |> int_of_string, rows |> int_of_string)

let map_param_array_builder jsons : ((int * int) * map_param) array = 
  jsons |> List.map ( fun j ->
              let name = j |> member "name" |> to_string in 
              let loc = j |> member "loc" |> to_string in
              let link = j |> member "link" |> to_string in 
              let col = parse_dims loc |> fst in 
              let row = parse_dims loc |> snd in 
    ((col,row), (Maps.MapParam.single_map_element_constructor ~name ~link)))
        |> Array.of_list


let main_engine_item : unit -> item array = 
  let rec read_item handler =
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
        (Array.append weapon_array food_array)
      else read_item handler in
  fun () -> (read_item (Unix.opendir "."))

let main_engine_map : unit -> current_map = 
  let rec read_map handler = 
    match Unix.readdir handler with
    | exception _ -> Unix.closedir handler;
      failwith "no map-param.json is not in current directory"
    | s -> let pos = String.length s in 
              if String.length s > 13 
              && (String.sub s (pos-5) 5) = ".json" 
              && contains s "map-param"
      then 
        let json = s |> Yojson.Basic.from_file in 
        let name = json |> member "name" |> to_string in 
        let unparsed_size = json |> member "size" |> to_string |> parse_dims in 
        let rows = snd unparsed_size in 
        let cols = fst unparsed_size in 
        let size = (cols, rows) in
        let picture_lists = json |> member "picture" |> to_list in
        let all_map_param = map_param_array_builder picture_lists in 
        Maps.map_constructor ~size ~name ~all_map_param
      else read_map handler in 
  fun () -> (read_map (Unix.opendir "."))


let init (): state =
  let items = main_engine_item () in
  let map = main_engine_map () in {
    items = items;
    food_inventory = [||];
    weapon_inventory = [||];
    player = main_engine_player ();
    current_map = map;
    all_maps = [|map|];
    current_map_in_all_maps = 0;
    enemies = () |> main_engine_enemy |> Array.of_list;
  }

let game_state= init ()

let change_item item (s:state) = s.items <- item

let change_player player s = s.player <- player

let change_enemies enemies s = s.enemies <- enemies

let get_items_array s = s.items

let get_player s = s.player

let get_enemies s = s.enemies

let get_current_map_name s = s.current_map.name

let get_current_map_size s = s.current_map.size

(** [move_player_left] change the current pos (col', row') of player to 
  (col'-1, row'-1)*)
let move_player_left s = 
  try
    match s.player with
    | Died -> ()
    | Player t ->
      let () = Player.move_left t s.current_map in
        s.player <- Player t
  with Player.Illegal _ -> ()

let move_player_right s = 
  try
    match s.player with
    | Died -> ()
    | Player t ->
      let () = Player.move_right t s.current_map in
        s.player <- Player t
  with Player.Illegal _ -> ()

let move_player_up s = 
  try
    match s.player with
    | Died -> ()
    | Player t ->
      let () = Player.move_up t s.current_map in
        s.player <- Player t
  with Player.Illegal _ -> ()

let move_player_down s = 
  try
    match s.player with
    | Died -> ()
    | Player t ->
      let () = Player.move_down t s.current_map in
        s.player <- Player t
  with Player.Illegal _ -> ()

let delete_one_enemy_from_state s enemy =
  for i = 0 to (Array.length s.enemies) - 1 do 
    if s.enemies.(i) = enemy 
    then s.enemies.(i) <- Deleted 
    else ()
  done

(** raises: UnknownFood if [food_name] 
  is not a valid food name in player's inventory*)
let eat_one_food s food_name = 
  try
    (let food_array = s.food_inventory in
    for i = 0 to (Array.length food_array) - 1 do 
      match food_array.(i), s.player with
      | Food food, Player t -> 
        if food.name = food_name
        then 
          (let health = Maps.Food.get_health food
          and strength = Maps.Food.get_strength food in
          let () = Player.increase_health t health 
          and () = Player.increase_strength t strength;
          food_array.(i) <- Null;
          s.player <- Player t; 
          raise SuccessExit)
        else ()
      | _ -> ()
    done);
    raise (UnknownFood food_name)
  with SuccessExit ->
    ()
  

  let equip_one_weapon s weapon_name = 
  try
    (let weapon_array = s.items in
    for i = 0 to (Array.length weapon_array) - 1 do 
      begin
      match weapon_array.(i), s.player with
      | Weapon w, Player t -> 
          if (s.weapon_inventory 
              |> Array.to_list 
              |> List.for_all (fun w1 -> Maps.Weapon.get_name w1 <>
                                          Maps.Weapon.get_name w )
              && Player.location t = Maps.Weapon.get_loc w)
          then 
            weapon_array.(i) <- Null;
            s.weapon_inventory <- Array.append [|w|] s.weapon_inventory;
            let health = Maps.Weapon.get_strength w in
            let () = Player.increase_strength t strength;
            s.player <- Player t; 
            raise SuccessExit
        else ()
      | _ -> ()
      end
    done);
    raise (UnknownWeapon weapon_name)
  with SuccessExit ->
    ()