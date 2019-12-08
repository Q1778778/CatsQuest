open Enemy
open Player
open Maps
open Yojson.Basic.Util

(*some constructors below required an id, which is created by the functions *)
(*instead of contained in json                                              *)

(** The abstract type of values representing an enemy in the game engine *)
type enemy = 
  | Enemy of Enemy.t
  | Deleted

(** The abstract type of values representing a player in the game engine *)
type player = 
  | Player of Player.t 
  | Died

(** The abstract type of values representing a food item in the game engine *)
type food_item = 
  | Food of Foods.Food.food
  | Eaten (*once a weapon or food has been taken, this weapon becomes null *)

(** The abstract type of values representing a weapon item in the game engine *)
type weapon_item =
  | Weapon of  Weapons.Weapon.weapon
  | Empty

(** The abstract type of values representing a map param in the game engine *)
type map_param = Maps.MapParam.map_param

(** The abstract type of values representing the current map state *)
type current_map = Maps.t

(** The exception type of a successful food ate or weapon equipped.
   Used to notify that an operation is successful. *)
exception SuccessExit

exception PlayerDied

(** The abstract type of values representing the current game state *)
type state = {
  mutable player: player;
  mutable food_inventory: food_item array;
  mutable weapon_inventory: weapon_item array;
  mutable player_old_loc: (int * int);
  mutable current_map_in_all_maps: int;

  branched_map_info: ((int * int) * string) list; (* Array doesn't have a find function, so I use List instead *)
  mutable current_map: current_map;
  mutable all_enemies_in_current_map: enemy array;
  mutable all_foods_in_current_map: food_item array;
  mutable all_weapons_in_current_map: weapon_item array;

  all_maps: current_map list; (*persistent map *)
  mutable all_foods: food_item array array;
  mutable all_weapons: weapon_item array array;
  (* each element represents an array of enemies in ONE map *)
  mutable all_enemies: enemy array array; 
}



(*                           helper methods                           *)

(**[branch_map_store] keeps track of the list of item names and their 
   corresponding locations.  *)
let branch_map_store = ref []

(*look up the ref right above *)

(**[update_branch_map_store n l] updates [branch_map_store] by 
   appending [(n,l)] to the front of the list referenced 
   by [branch_map_store].  *)
let update_branch_map_store name loc =
  branch_map_store := (loc, name) :: !branch_map_store

(**[count ()] returns [c], the # of times this function has been called. 
   Each time this function gets called, [c] gets incremented. *)
let count = 
  let counter = ref 0 in fun () -> (incr counter; !counter)

(**[probabilty s] produces a bool based on probabilty [s]
   Requires:
   [s] mod 0.1. 0.1 <= [s] <= 1.0*)
let probabilty s = 
  (* 0 <= x <= 10 *)
  let x = Int.to_float (Random.int 11) in
  x <  (s *. 10.0)

(**[random_choice lst] is a random object chosen from list [lst]
   Requires:
   [lst] cannot be empty*)
let random_choice list = 
  List.nth list (Random.int (List.length list))

(**[random_list_with_fixed_length lst len] is a randomly chosen list
   with length [len] and its elements from [lst]*)
let random_list_with_fixed_length list len =
  List.map (fun _ -> random_choice list) 
    ((Array.make len 0) |> Array.to_list)

(**[choose_skill_random t] is a (skill name, skill strength) for enemy [t], 
   chosen randomly from all of enemy [t]'s skills *)
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

(**[contains s s1] is [true] if [s] contains substring [s1], [false] 
   otherwise *)
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

(**[random_int_array_for_enemies_and_items arr num] returns a 
   probability-driven random int array with the number [num] and the 
   location array [arr] *)
let random_int_array_for_enemies_and_items size_array number =
  let round f = truncate (f +. 0.5) in
  let raw_prob = size_array |> Array.to_list in
  let rec total_sum num = function
    | [] -> num
    | h::d -> total_sum (num + h) d in
  let sum = total_sum 0 raw_prob in
  let float_sum = float_of_int sum in
  let float_num = float_of_int number in
  let temp_random_number = (*the probability oper here is pretty messy *)
    List.map (fun s -> 
        ((float_of_int s) /. float_sum *. float_num) 
        |> round) raw_prob in 
  let tl =List.tl temp_random_number in
  (number - (total_sum 0 tl)::(tl))
  |> Array.of_list

(**[sorted_list col row n] is [List.rev [(col, row), (col-1, row), ... , 
   (1, row), (col, row-1), (col-1, row-1), ...]x] containing [n] elements. 
   Requires: 0 <= [n] <= [col * row] *)
let sorted_list col row length = 
  let rec inner_looper col' row' finished count = 
    if count = 0 then finished
    else if col' = 1 
    then 
      inner_looper col (row'-1) ((col', row')::finished) (count - 1)
    else 
      inner_looper (col'-1) (row') ((col', row')::finished) (count - 1) in
  inner_looper col row [] length

(**[unique_location_list col row n] returns [sorted_list col row n] if 
   [col < n/2] or [row < n/2], otherwise it returns a randomly constructed
   set-like list of [(col, row)] coordinates with length [n]. *)
let unique_location_list col row length =
  if (col < (length / 2)) || (row < (length / 2)) then
    (* small map. A sorted list is better for minimizing time complexity*)
    sorted_list col row length 
  else
    let rec constructor finished count =
      if count = 0 then finished
      else let r_col = 1 + Random.int col in
        let r_row = 1 + Random.int row in 
        if List.mem (r_col, r_row) finished
        then constructor finished count (* try again *)
        else constructor ((r_col, r_row)::finished) (count - 1) in
    constructor [] length


(**[parse_dims s] parses [s] and returns [(col, row)]. 
   Requires: [s] is in the form ["# cols, # rows"] *)
let parse_dims s = 
  let rows = List.nth (String.split_on_char ',' s) 0 in 
  let cols = List.nth (String.split_on_char ',' s) 1 in 
  (cols |> int_of_string, rows |> int_of_string)



(*                        models builder                         *)

let gainable_skill_constructor jsons_list = 
  if jsons_list = [] then []
  else
    List.map (fun skill -> 
        let description = skill |> member "description" |> to_string in
        let strength = skill |> member "strength" |> to_int in
        let name = skill |> member "name" |> to_string in
        let cd = skill |> member "cd" |> to_int in
        Player.skill_constructor name description strength cd) jsons_list


(**[browse_dir_enemy h lst] is a list of enemy json files 
   extracted from the directory handler [h]. 
   Requires: the files in the directory handler [h] must be in valid enemy 
   name format - ["enemy-*.json"], in which ["*"] represents different 
   enemy names*)
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

(**[single_enemy_builder j id col row] constructs a new enemy represented 
   by the json [j] at location [(col, row)] with id [id] *)
let single_enemy_builder j ~col ~row =
  Enemy (
    let name = j |> member "name" |> to_string in
    let id = count () |> string_of_int in
    let descr = j |> member "description" |> to_string in
    let exp = j |> member "experience" |> to_int in
    let level = j |> member "level" |> to_int in
    let pos = (col, row) in  (* random init pos *)
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
    let gainables =
      j |> member "gainable"|> to_list |> gainable_skill_constructor in 
    Enemy.constructor ~pos ~level ~exp
      ~hp ~id ~name ~descr ~max_hp ~skills ~gainables)

(** [browse_one_enemy_json j id col row] calls 
    [single_enemy_builder j id col row] if [j] is a valid enemy json
    representation. 
    Raises: [Failure "something wrong with browse_dir_enemy. Check it"] if 
    [j] is not a valid enemy json representation. *)
let browse_one_enemy_json j ~col ~row = 
  if (contains j "witch" || contains j "minion" || contains j "goblin")
  then single_enemy_builder (Yojson.Basic.from_file j) ~col ~row
  else failwith "something wrong with browse_dir_enemy. Check it"

(**[main_engine_ememy_for_single_map col row num] 
   Raises: [Failure "NONE of 'enemy' json exists"] if none of the json 
   file names are contain ["enemy"] *)
let main_engine_ememy_for_single_map ~col ~row ~(number:int) : enemy array = 
  try 
    let all_enemy_models = browse_dir_enemy (Unix.opendir ".") [] in
    let expected_enemy_models =
      random_list_with_fixed_length all_enemy_models number in
    (List.map2  (fun x (col, row)-> browse_one_enemy_json x ~col ~row)
       (expected_enemy_models)
       (unique_location_list col row number))|>Array.of_list
  with Unix.Unix_error(Unix.ENOENT, _ ,_ ) ->
    raise (Failure "NONE of 'enemy' json exists")


(**[main_engine_enemy loc_arr num_arr] reads all enemy json files in 
   current directory with the corresponding locations in [loc_arr] and 
   numbers in [num_arr], and returns a mapped 2d array with this information *)
let main_engine_enemy ~loc_array ~final_number_array : enemy array array =
  Array.map2 
    (fun number (col, row) -> main_engine_ememy_for_single_map col row number)
    (final_number_array) (loc_array)


(**[main_engine_player ()] is the main execution method for the 
   constructing the main player, which gets returned at the end of this
   function call. *)
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
        let experience = 0 in
        Player.constructor ~health ~level ~strength ~experience ()
      else read_map handler in
  fun () -> Player (read_map (Unix.opendir "."))


(**[food_array_builder cols rows j_arr] constructs a new food array 
   represented by the json array [j_arr] with the dimensions [cols] by [rows] 
*)
let food_array_builder cols rows jsons: food_item array = 
  jsons |> List.map2 
    (fun (col, row) j -> let id = count () in
      let health = j |> member "health" |> to_int in
      let strength = j |> member "strength" |> to_int in
      let name = j |> member "name" |> to_string in
      let description = j |> member "description" |> to_string 
      let gainables = j |> member "gainables" |> to_list 
                        |> gainable_skill_constructor in
      Food (Foods.Food.constructor ~col ~row ~health 
            ~description ~name ~id ~strength ~gainables))
    ((unique_location_list cols rows (List.length jsons))) 
  |> Array.of_list

(**[main_engine_food_for_single_map col row num] reads the file ["foods.json"]
   from the current directory, and returns the food array parsed from that 
   file *)
let main_engine_food_for_single_map ~col ~row ~number= 
  let rec read_food handler = 
    match Unix.readdir handler  with 
    | exception _ -> Unix.closedir handler; 
      failwith "foods.json is not in current directory"
    | json ->  let pos = String.length json in 
      if String.length json >= 10
      && (String.sub json (pos-5) 5) = ".json" 
      && contains json "foods"
      then json 
           |> Yojson.Basic.from_file 
           |> to_list 
           |> food_array_builder col row
      else read_food handler in
  (read_food (Unix.opendir "."))

(**[main_engine_food locs nums] calls [main_engine_food_for_single_map] for
   each location in [locs] and final number in [nums], and returns the
   mapped 2d array with this information.  *)
let main_engine_food ~(loc_array: (int * int) array) 
    ~(final_number_array: int array) : food_item array array =
  Array.map2 
    (fun number (col, row) -> (main_engine_food_for_single_map col row number))
    final_number_array loc_array

(**[weapon_array_builder cols rows j_arr] constructs a new weapon array 
   represented by the json array [j_arr] with the dimensions [cols] by [rows] 
*)
let weapon_array_builder cols rows jsons: weapon_item array = 
  jsons 
  |> List.map2 
    (fun (col, row) j -> let id = count () in
      let name = j |> member "name" |> to_string in
      let description = j |> member "description" |> to_string in
      let strength = j |> member "strength"|> to_int in
      let gainables = j |> member "gainables" |> to_list 
                        |> gainable_skill_constructor in
      Weapon (Weapons.Weapon.constructor ~strength ~col ~row 
                ~description ~name ~id ~gainables))
    (unique_location_list cols rows (List.length jsons))
  |> Array.of_list

(**[main_engine_weapon_for_single_map c r n] reads the file ["weapons.json"] 
   in the current directory and returns the parsed weapon item array. 
   Raises: [Failure "weapons.json is not in current directory"] if the 
   the current directory does not contain the file ["weapons.json"] *)
let main_engine_weapon_for_single_map ~(col: int) ~(row: int) ~number =
  let rec read_weapon handler = 
    match Unix.readdir handler with 
    | exception _ -> Unix.closedir handler; 
      failwith "weapons.json is not in current directory"
    | json -> let pos = String.length json in 
      if String.length json >= 12
      && (String.sub json (pos-5) 5) = ".json" 
      && contains json "weapons"
      then json |> Yojson.Basic.from_file 
           |> to_list 
           |> weapon_array_builder col row
      else read_weapon handler in
  (read_weapon (Unix.opendir "."))

(**[main_engine_weapon locs nums] calls [main_engine_weapon_for_single_map] for
   each location in [locs] and final number in [nums], and returns the
   mapped 2d array with this information.  *)
let main_engine_weapon ~loc_array ~final_number_array = 
  Array.map2 
    (fun number (col, row) -> 
       main_engine_weapon_for_single_map col row number)
    final_number_array loc_array


(**[map_param_array_builder j_arr] constructs a new map param array 
   represented by the json array [j_arr]. *)
let map_param_array_builder jsons : ((int * int) * map_param) list = 
  jsons |> List.map ( fun j ->
      let name = j |> member "name" |> to_string in 
      let loc = j |> member "loc" |> to_string in
      let link = j |> member "link" |> to_string in 
      (*updating branched loc*)
      let col = parse_dims loc |> fst in 
      let row = parse_dims loc |> snd in 
      let _ = 
        (* i don't think the later check is necessary *)
        if link <> "" && name = "main" 
        then update_branch_map_store link (col, row) 
        else () in
      ((col,row), (Maps.MapParam.single_map_element_constructor ~name ~link)))

(**[build_one_map s] returns the constructed map from the json file name [s].
   Requires: [s] must be in the form ["map-param*.json"] where ["*"] denotes
   any string.  *)
let build_one_map s = 
  let json = s |> Yojson.Basic.from_file in 
  let name = json |> member "name" |> to_string in 
  let size = json |> member "size" |> to_string |> parse_dims in 
  let picture_lists = json |> member "picture" |> to_list in
  let all_map_param = map_param_array_builder picture_lists in
  (Maps.map_constructor ~size ~name ~all_map_param), size

(**[reformat_output_map comb_list] is the array representation of 
   [comb_list]. 
   Raises: [Failure "no main map in this directory"] if the main map does not 
   exist in this directory. 
*)
let reformat_output_map comb_list: current_map array * (int * int) array =
  let rec looper finished_map finished_loc = function
    | [] -> failwith "no main map in this directory"
    | (map, loc)::d ->
      if map.name = "main" then
        let remains_map, remains_loc = List.split d in
        (map::(finished_map @ remains_map)) |> Array.of_list, 
        (loc::(finished_loc @ remains_loc)) |> Array.of_list
      else looper (map::finished_map) (loc::finished_loc) d in
  looper [] [] comb_list

(**[main_engine_map_param ()] reads all files in the current directory 
   with the format ["map-param*.json"] where ["*"] can be any string, 
   and returns the parsed map representation. 
   Raises: [Failure "no map-param.json is in current directory"] if there
   is no file with the format ["map-param*.json"] *)
let main_engine_map_param () : (current_map array) * (int * int) array = 
  let rec read_map handler list = 
    match Unix.readdir handler with
    | exception _ -> Unix.closedir handler;
      if list = [] then failwith "no map-param.json is in current directory"
      (*the first map of the output MUST be main map*)
      else reformat_output_map list 
    | s -> let pos = String.length s in 
      if String.length s > 13 
      && (String.sub s (pos-5) 5) = ".json" 
      && contains s "map-param"
      then 
        read_map handler ((build_one_map s)::list)
      else read_map handler list in 
  (read_map (Unix.opendir ".") [])

let main_map_size_array map_array : int array = 
  Array.map (fun map -> let x, y = Maps.size map in x * y) map_array

(*                       initiate the game                           *)

(** [init ()] is the init state of the entire game. 
    Invariant: the first map of all maps must be main map !!! *)
let init (): state =
  let map_array, loc_array = main_engine_map_param () in
  let number = 15 (*this number can be either artificially set or stored in json.*) in
  let map_size_array = main_map_size_array map_array in
  let final_number_array = random_int_array_for_enemies_and_items map_size_array number in
  let all_enemies = (main_engine_enemy ~loc_array ~final_number_array)  in
  let all_foods = (main_engine_food ~loc_array ~final_number_array)  in
  let all_weapons = (main_engine_weapon ~loc_array ~final_number_array) in {
    player = main_engine_player ();
    food_inventory = [|Eaten; Eaten; Eaten|];
    weapon_inventory = [|Empty; Empty; Empty|]; (*the length of inventory shouldn't be changed *)
    current_map_in_all_maps = 0; (* this shouldn't be changed *)
    current_map = map_array.(0);

    player_old_loc = (0,0);
    branched_map_info = !branch_map_store;
    all_enemies_in_current_map = all_enemies.(0); (* deep copy. Update will change both*)
    all_foods_in_current_map = all_foods.(0); 
    all_weapons_in_current_map = all_weapons.(0);

    all_maps = map_array |> Array.to_list;
    all_foods = all_foods;
    all_weapons = all_weapons;
    all_enemies = all_enemies;
  }




(*                        basic getters                            *)

(**[game_state] is the initial game state *)
let game_state = init ()

(**[change_player p s] changes the player in game state [s] to [p] *)
let change_player player s = s.player <- player

(**[get_player s] returns the player if the player in game state [s]
   is alive. 

   Raises: [PlayerDied] if the player has already died.  *)
let get_player s = 
  match s.player with
  | Player p -> p
  | Died -> raise PlayerDied

(**[get_enemies s] is all the enemies in game state [s] *)
let get_enemies s = s.all_enemies_in_current_map

let check_enemy s store =
  let loc = s |> get_player |> Player.location in
  for i = 0 to (Array.length s.all_enemies_in_current_map) - 1 do
    match s.all_enemies_in_current_map.(i) with
    | Enemy e when Enemy.get_pos e = loc ->
      (store.(0) <- Enemy.get_id e;
       raise SuccessExit)
    | _ -> ()
  done

let check_enemy_in_current_loc s =
  let store = [|""|] in
  try
    check_enemy s store;
    false, ""
  with SuccessExit ->
    true, store.(0)

(**[get_map s] is the current map in game state [s] *)
let get_map s = s.current_map

(**[find_one_map_by_name lst name] is the map with its name as [name] from
   a list of map [lst]
   Requires: map with name [map_name] must be inside [lst] *)
let find_one_map_by_name map_array map_name =
  List.find (fun map -> map.name = map_name) (map_array)

(**[get_current_map_name s] is the name of the current map in game state [s] *)
let get_current_map_name s = s.current_map.name

(**[get_current_map_size s] is the size of the current map in game state [s] *)
let get_current_map_size s = s.current_map.size


let reduce_player_health s hp = 
  match s.player with
  | Player t -> 
    if Player.health t <= hp
    then s.player <- Died
    else Player.reduce_health t hp
  | _ -> ()
(*map-param related methods *)



(*                          move player                           *)

(** [move_player_left s] change the current pos (col', row') of player 
    in state [s] to (col'-1, row') within the map boundaries.*)
let move_player_left s = 
  match s.player with
  | Died -> ()
  | Player t -> Player.move_left t s.current_map

(** [move_player_right s] change the current pos (col', row') of player 
    in state [s] to (col'+1, row') within the map boundaries.*)
let move_player_right s = 
  match s.player with
  | Died -> ()
  | Player t -> Player.move_right t s.current_map 

(** [move_player_up s] change the current pos (col', row') of player 
    in state [s] to (col', row'+1) within the map boundaries.*)
let move_player_up s = 
  match s.player with
  | Died -> ()
  | Player t -> Player.move_up t s.current_map

(** [move_player_down s] change the current pos (col', row') of player 
    in state [s] to (col', row'-1) within the map boundaries.*)
let move_player_down s = 
  match s.player with
  | Died -> ()
  | Player t -> Player.move_down t s.current_map



(*                       change game state                        *)

(**[delete_one_enemy_from_state s] deletes the enemy with player's current
   location at state [s] *)
let delete_one_enemy_from_state s =
  let player = s |> get_player in
  let loc = player |> Player.location in
  for i = 0 to (Array.length s.all_enemies_in_current_map) - 1 do 
    match s.all_enemies_in_current_map.(i) with
    | Enemy t when Enemy.get_pos t = loc ->
      s.all_enemies_in_current_map.(i) <- Deleted;
      Player.increase_experience player (Enemy.get_experience t);
      Player.update_skill player (Enemy.get_gainable_skill t)
    | _ -> ()
  done

(**[take_one_food s] takes food from the player's current location at state
   [s], if any, and adds it to the first empty slot in the player's food 
   inventory. 
   Raises: [SuccessExit] if the food from the player's location was 
   successfully eaten. *)
let take_one_food s =
  let update_food_inventory f t =
    (for j = 0 to (Array.length s.food_inventory) - 1 do
       match s.food_inventory.(j) with
       | Eaten -> 
         (s.food_inventory.(j) <- Food f; 
          raise SuccessExit)
       | _ -> ()
     done) in
  let player = s |> get_player in
  let loc = player |> Player.location in
  for i = 0 to (Array.length s.all_foods_in_current_map) - 1 do
    match s.all_foods_in_current_map.(i) with
    | Food f when Foods.Food.get_loc f = loc ->
      s.all_foods_in_current_map.(i) <- Eaten;
      update_food_inventory f player
    | _ -> ()
  done

(**[take_one_food s] takes food on player's current location at state [s], 
   if any, to the first empty slot in player's food inventory. *)
let take_one_food_in_current_location s = 
  try
    take_one_food s
  with SuccessExit ->
    ()

(**[drop_one_food s pos] drops the food at index [pos] of the player's 
   food inventory to the player's current location at state [s]. 
   Raises: [SuccessExit] if the food at index [pos] is successfully dropped. *)
let drop_one_food s pos = 
  let search_food_array s f loc = 
    Foods.Food.set_loc f loc;
    let food = Food f in
    (for j = 0 to (Array.length s.all_foods_in_current_map) - 1 do
       match s.all_foods_in_current_map.(j) with
       | Eaten -> (s.all_foods_in_current_map.(j) <- food;
                   raise SuccessExit)
       | _ -> ()
     done);
    (* no empty slot *)
    s.all_foods_in_current_map <- Array.append 
        [| food |] s.all_foods_in_current_map; in
  match s.food_inventory.(pos) with
  | Food f -> 
    s.food_inventory.(pos) <- Eaten;
    search_food_array s f (s |> get_player |> Player.location);
  | Eaten -> ()

(**[drop_one_food_to_current_location s pos] drops the food at index [pos] 
   of the player's food inventory to the player's current location at state 
   [s]. *)
let drop_one_food_to_current_location s pos =
  try
    drop_one_food s pos
  with SuccessExit ->
    ()

(**[eat_one_food_in_inventory s pos] eats the food from the player's current
   location, if any, with the player increasing health and strength and the 
   food at index [pos] removed from the player's inventory at state [s].  *)
let eat_one_food_in_inventory s pos = 
  let eat_food food t = 
    let health = Foods.Food.get_health food
    and strength = Foods.Food.get_strength food in
    Player.increase_health t health;
    Player.increase_strength t strength;
    Player.update_skill t (Foods.Food.get_gainables food) in
  let player = s |> get_player in
  match s.food_inventory.(pos) with
  | Food f -> 
    eat_food f player;
    s.food_inventory.(pos) <- Eaten;
  | _ -> ()



(**[equip_one_weapon s] will check player's inventory and equip player with
   weapon of player's current location if possible. If the weapon inventory is
   already full, the weapon will not be equipped (the game state wouldn't 
   change).
   Raises: [SuccessExit] if the weapon is successfully equipped in the 
   player's inventory.  *)
let equip_one_weapon s =
  let update_weapon_inventory w t =
    (for j = 0 to (Array.length s.weapon_inventory) - 1 do
       match s.weapon_inventory.(j) with
       | Empty -> 
         (s.weapon_inventory.(j) <- Weapon w;
          Player.increase_strength t (Weapons.Weapon.get_strength w);
          Player.update_skill t (Weapons.Weapon.get_gainables w);
          raise SuccessExit)
       | _ -> ()
     done) in
  let player = s |> get_player in
  let loc = player |> Player.location in
  for i = 0 to (Array.length s.all_weapons_in_current_map) - 1 do
    match s.all_weapons_in_current_map.(i) with
    | Weapon w when Weapons.Weapon.get_loc w = loc ->
      s.all_weapons_in_current_map.(i) <- Empty;
      update_weapon_inventory w player
    | _ -> ()
  done

(**[equip_one_weapon s] will update the weapon inventory of game state [s]
   if there is any empty slot and weapon in player's current location will be 
   equipped in that slot.  *)
let equip_weapon_in_current_loc s = 
  try
    equip_one_weapon s
  with SuccessExit ->
    ()

(**[drop_one_weapon s pos] drops the weapon at index [pos] of the player's 
   weapon inventory to the player's current location at state [s]. 
   Example: if the weapon inventory is [|Null; Null; dagger|]
   [drop_one_weapon s 2] drops [dagger], and the weapon inventory 
   becomes [|Null; Null; Empty|].
   Raises: [SuccessExit] if the weapon at index [pos] is successfully removed
   from the weapon inventory. *)
let drop_one_weapon s pos = 
  let search_weapon_array s w loc = 
    Weapons.Weapon.set_loc w loc;
    let weapon = Weapon w in
    (for j = 0 to (Array.length s.all_weapons_in_current_map) - 1 do
       match s.all_weapons_in_current_map.(j) with
       | Empty -> 
         (s.all_weapons_in_current_map.(j) <- weapon;
          raise SuccessExit)
       | _ -> ()
     done);
    (* no empty slot *)
    s.all_weapons_in_current_map <- Array.append 
        [| weapon |] s.all_weapons_in_current_map; in
  match s.weapon_inventory.(pos) with
  | Weapon w -> 
    let player = s |> get_player in
    Player.reduce_strength player (Weapons.Weapon.get_strength w);
    s.weapon_inventory.(pos) <- Empty;
    search_weapon_array s w (player |> Player.location);
  | _ -> ()

(**[drop_one_weapon_to_current_location s pos] drops the weapon at index 
   [pos] of the player's weapon inventory to the player's current location 
   at state [s]. 
   Example: if the weapon inventory is [|Null; Null; dagger|]
   [drop_one_weapon_to_current_location s 2] drops [dagger], and the 
   weapon inventory becomes [|Null; Null; Empty|]. *)
let drop_one_weapon_to_current_location s pos =
  try
    drop_one_weapon s pos
  with SuccessExit ->
    ()


(**[check_food_on_loc_and_return_name_list s loc] returns a list of food names
   (possibly empty) are at the location [loc] 
   in the current map in state [s] *)
let check_food_on_loc_and_return_name_list s loc =
  let store = [|[]|] in
  (for i = 0 to (Array.length s.all_foods_in_current_map) - 1 do
     match s.all_foods_in_current_map.(i) with
     | Food f when Foods.Food.get_loc f = loc ->
       store.(0) <- (Foods.Food.get_name f)::store.(0)
     | _ -> ()
   done);
  store.(0)

(**[check_weapon_on_loc_and_return_name_list s loc] returns a list of 
   weapon names (possibly empty) that are at the location [loc] 
   in the current map in state [s] *)
let check_weapon_on_loc_and_return_name_list s loc =
  let store = [|[]|] in
  (for i = 0 to (Array.length s.all_weapons_in_current_map) - 1 do
     match s.all_weapons_in_current_map.(i) with
     | Weapon w when Weapons.Weapon.get_loc w = loc ->
       store.(0) <- ((Weapons.Weapon.get_name w)::store.(0))
     | _ -> ()
   done);
  store.(0)


(**[check_item_on_player_ground s] is a tuple of 
   (food_name list,  weapon_name list) at player's current position
   at state [s] 
   Requires: Player MUST BE Alive *)
let check_item_on_player_ground s =
  let loc = s |> get_player |> Player.location in
  (
    check_food_on_loc_and_return_name_list s loc,
    check_weapon_on_loc_and_return_name_list s loc
  )




(**[check_current_linked_map s] returns [(false, "")] if the current map in 
   state [s] is not ["main"] or if the player at state [s] is not found in the 
   branced map info. Otherwise, [(true, info)] is returned where [info] is 
   the information in the branched map *)
let check_current_linked_map s =
  if get_current_map_name s <> "main" then false, ""
  else 
    try
      let loc = s |> get_player |> Player.location in
      true, (List.assoc loc s.branched_map_info)
    with Not_found ->
      false, ""

(**[get_map_index_by_name s name] returns the index of the map in game state
   [s] with the corresponding name [name]. *)
let get_map_index_by_name s name = 
  let rec search acc = function 
    | [] -> failwith "invalid map name"
    | h::d -> if h.name = name then acc else
        search 0 d in 
  search 0 s.all_maps


(**[transfer_player_to_branch_map s] transfers the player at state [s] to the
   branch map. *)
let transfer_player_to_branch_map s = 
  let status, name =  check_current_linked_map s in
  if status = false then ()
  else 
    let map = find_one_map_by_name s.all_maps name in
    let map_index = get_map_index_by_name s map.name in
    s.player_old_loc <- s |> get_player |> Player.location;
    s.current_map <- map;
    s.all_enemies_in_current_map <- s.all_enemies.(map_index);
    s.all_foods_in_current_map <- s.all_foods.(map_index);
    s.all_weapons_in_current_map <- s.all_weapons.(map_index);
    s.current_map_in_all_maps <- map_index;
    (* the init pos of player in branched map is (1,1) *)
    Player.switch_loc (get_player s) (1,1)

(**[check_branch_map_status s] returns whether the player at state [s] has 
   finished his current branched map. *)
let check_branch_map_status s = 
  get_current_map_name s <> "main" 
  && Array.for_all (fun enemy -> enemy = Deleted) s.all_enemies_in_current_map

(**[transfer_player_to_main_map s] transfers the player at state [s] to the 
   main map and changes to the corresponding configurations in the map. *)
let transfer_player_to_main_map s =
  if check_branch_map_status s
  then 
    (s.current_map <- List.hd s.all_maps;
     Player.switch_loc (get_player s) s.player_old_loc;
     s.all_enemies_in_current_map <- s.all_enemies.(0);
     s.all_foods_in_current_map <- s.all_foods.(0);
     s.all_weapons_in_current_map <- s.all_weapons.(0);
     s.current_map_in_all_maps <- 0)
  else
    ()

(**[list_of_entrance_loc_to_branch_map s] is the list of entrance locations
   to the branch map named [s]. *)
let list_of_entrance_loc_to_branch_map s =
  if get_current_map_name s <> "main"
  then []
  else
    s.branched_map_info |> List.split|> fst