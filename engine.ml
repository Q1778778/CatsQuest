open Enemy
open Player
open Maps
open Yojson.Basic.Util

(*some constructors below required an id, which is created by the functions *)
(*instead of contained in json                                              *)

type enemy = 
  | Enemy of Enemy.t
  | Deleted

type player = 
  | Player of Player.t 
  | Died

type food_item = 
  | Food of Foods.Food.food
  | Null (*once a weapon or food has been taken, this weapon becomes null *)

type weapon_item =
  | Weapon of  Weapons.Weapon.weapon
  | Null

type map_param = Maps.MapParam.map_param

type current_map = Maps.t

exception UnknownFood of string

exception UnknownWeapon of string

exception SuccessExit

type state = {
  mutable player: player;
  mutable food_inventory: food_item array;
  mutable weapon_inventory: weapon_item array;
  mutable player_old_loc: (int * int);
  mutable current_map_in_all_maps: int;

  branched_map_info: ((int * int) * string) list;
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

let branch_map_store = ref []

(*look up the ref right above *)
let update_branch_map_store name loc =
  branch_map_store := ((loc, name)::(!branch_map_store))

let count = 
  let counter = ref 0 in fun () -> (incr counter; !counter)

(**[probabilty s] produces a bool based on probabilty [s]

   Require:
   [s] mod 0.1. 0.1 <= [s] <= 1.0*)
let probabilty s = 
  (* 0 <= x <= 10 *)
  let x = Int.to_float (Random.int 11) in
  x <  (s *. 10.0)

(**[random_choice lst] is a random object chosen from list [lst]

   Require:
   [lst] cannot be empty*)
let random_choice list = 
  List.nth list (Random.int (List.length list))

(**[random_list_with_fixed_length lst len] is a randomly chosen list
   with length as [len] and its elements from [lst]*)
let random_list_with_fixed_length list len =
  List.map (fun _ -> random_choice list) 
    ((Array.make len 0) |> Array.to_list)

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

let random_int_array_for_enemies_and_items loc_array number =
  let round f = truncate (f +. 0.5) in
  let raw_prob = List.map (fun (x, y) -> x * y) (loc_array |> Array.to_list) in
  let rec total_num num = function
    | [] -> num
    | h::d -> total_num (num + h) d in
  let sum = total_num 0 raw_prob in
  let temp_random_number = (*the probability oper here is pretty messy *)
    List.map 
    (fun s -> 
      ((float_of_int s) /. (float_of_int sum) *. (float_of_int number)) 
          |> round) raw_prob in 
  (((List.hd random_number) 
      + sum - (total_sum 0 random_number))::(List.tl temp_random_number))
          |> Array.of_list
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

(* change it later *)
let single_enemy_builder j ~id ~col ~row =
  Enemy (
    let name = j |> member "name" |> to_string in
    let id = (Int.to_string (id + 1)) in
    let descr = j |> member "description" |> to_string in
    let exp = j |> member "experience" |> to_int in
    let level = j |> member "level" |> to_int in
    let pos = ((Random.int col)+1, (Random.int row)+1) in  (* random init pos *)
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
    let map = "main" in
    Enemy.constructor ~pos ~level ~exp ~name
      ~hp ~id ~descr ~max_hp ~skills ~map )

let browse_one_enemy_json j ~id ~col ~row = 
  if (contains j "witch" || contains j "minion" || contains j "goblin")
  then single_enemy_builder (Yojson.Basic.from_file j) ~id ~col ~row
  else failwith "something wrong with browse_dir_enemy. Check it"

let main_engine_ememy_for_single_map ~col ~row ~(number:int) : enemy array = 
  try 
    let all_enemy_models = browse_dir_enemy (Unix.opendir ".") [] in
    let expected_enemy_models =
      random_list_with_fixed_length all_enemy_models number in
    let id = count () in 
    (List.map  (fun x -> browse_one_enemy_json x ~id ~col ~row)
      expected_enemy_models) |> Array.of_list
  with Unix.Unix_error(Unix.ENOENT, _ ,_ ) ->
    raise (Failure "NONE of 'enemy' json exists")

(**[main_engine_enemy ()] read all enemy json files in current directory*) (* add forbidden *)
let main_engine_enemy ~loc_array ~final_number_array =
  (Array.map2 
    (fun number (col, row) -> main_engine_ememy_for_single_map col row number)
      (Array.of_list final_number_array) (Array.of_list loc_array))
  
  
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
        Player.constructor ~health ~level ~strength  ~row ~col ~experience ()
      else read_map handler in
  fun () -> Player (read_map (Unix.opendir "."))


let food_array_builder cols rows jsons: food_item array = 
  jsons |> 
    Array.map 
      (fun j -> let id = count () in
        let health = j |> member "health" |> to_int in
        let strength = j |> member "strength" |> to_int in
        let name = j |> member "name" |> to_string in
        let description = j |> member "description" |> to_string in
        let row = 1 + Random.int rows in
        let col = 1 + Random.int cols in
        let map = "main" in
        Food (Foods.Food.constructor ~col ~row ~health 
                ~description ~name ~id ~strength ~map))



let weapon_array_builder cols rows jsons: weapon_item array = 
  jsons 
  |> Array.map 
    (fun j -> let id = count () in
      let name = j |> member "name" |> to_string in
      let description = j |> member "description" |> to_string in
      let strength = j |> member "strength"|> to_int in
      let row = 1 + Random.int rows in
      let col = 1 + Random.int cols in
      let map = "main" in
      Weapon (Weapons.Weapon.constructor ~strength ~col ~row 
                ~description ~name ~id ~map))


(**[parse_dims s] parses [s] and returns [(col, row)]. 
   Requires: [s] is in the form ["# cols, # rows"]
*)
let parse_dims s = 
  let rows = List.nth (String.split_on_char ',' s) 0 in 
  let cols = List.nth (String.split_on_char ',' s) 1 in 
  (cols |> int_of_string, rows |> int_of_string)


let map_param_array_builder jsons : ((int * int) * map_param) list = 
  jsons |> List.map ( fun j ->
      let name = j |> member "name" |> to_string in 
      let loc = j |> member "loc" |> to_string in
      let link = j |> member "link" |> to_string in 
      (*updating branched loc*)
      let col = parse_dims loc |> fst in 
      let row = parse_dims loc |> snd in 
      let _ = 
        if link <> "" && name = "main" (* i don't think the later check is necessary *)
        then update_branch_map_store link (col, row) 
        else () in
      ((col,row), (Maps.MapParam.single_map_element_constructor ~name ~link)))


let main_engine_food_for_single_map ~col ~row = 
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
          |> Array.of_list
          |> food_array_builder col row
      else read_food handler in
  fun () -> (read_food (Unix.opendir "."))


let main_engine_food ~(loc_array: (int * int) array) 
    ~(final_number_array: int array) =
  Array.map2 
    (fun number (col, row) -> main_engine_food_for_single_map col row number ())
      final_random_array loc_array


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
                |> Array.of_list 
                |> weapon_array_builder col row
      else read_weapon handler in
  fun () -> (read_weapon (Unix.opendir "."))


let main_engine_weapon ~loc_array ~final_number_array = 
  Array.map2 
    (fun number (col, row) -> 
      main_engine_weapon_for_single_map col row number ())
      final_random_array loc_array


let build_one_map s = (*s is the map-param * .json*)
  let json = s |> Yojson.Basic.from_file in 
  let name = json |> member "name" |> to_string in 
  let size = json |> member "size" |> to_string |> parse_dims in 
  let picture_lists = json |> member "picture" |> to_list in
  let all_map_param = map_param_array_builder picture_lists in
  (Maps.map_constructor ~size ~name ~all_map_param), size


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


let main_engine_map_param (): -> (current_map array) * (int * int) array = 
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
  fun () -> (read_map (Unix.opendir ".") [])


(**[find_one_map_by_name lst name] is the map with its name as [name] from
   a list of map [lst]

   Require:
   map with name [map_name] must be inside [lst] *)
let find_one_map_by_name map_array map_name =
  List.find (fun map -> map.name = map_name) map_array


(* TODO: Unfinished *)
(* Invariant: the first map of all maps must be main map !!!*)
let init (): state =
  let map_array, loc_array = main_engine_map_param () in
  let number = 7 (*this number can be either artificially set or stored in json.*) in
  let final_number_array = random_int_array_for_enemies_and_items loc_array number in
  let all_enemies = (main_engine_enemy ~loc_array ~final_number_array)  in
  let all_foods = (main_engine_food ~loc_array ~final_number_array)  in
  let all_weapons = (main_engine_weapon ~loc_array ~final_number_array)  in {
    player = main_engine_player ();
    food_inventory = [|Null; Null; Null|];
    weapon_inventory = [|Null; Null; Null|]; (*the length of inventory shouldn't be changed *)
    current_map_in_all_maps = 0; (* this shouldn't be changed *)
    current_map = find_one_map_by_name map_array "main";

    player_old_loc = (0,0);
    branched_map_info = !branch_map_store;
    all_enemies_in_current_map = all_enemies.(0); (* deep copy. Update will change both*)
    all_foods_in_current_map = all_foods.(0); 
    all_weapons_in_current_map = all_weapons.(0);

    all_maps = map_array;
    all_foods = all_foods;
    all_weapons = all_weapons;
    all_enemies = all_enemies;
  }

let game_state = init ()

let change_player player s = s.player <- player

let change_enemies enemies s = s.enemies <- enemies

let get_player s = s.player

let get_enemies s = s.enemies

let get_map s = s.current_map

let get_current_map_name s = s.current_map.name

let get_current_map_size s = s.current_map.size

(*let get_all_weapons_on_current_map s = 
  let name = s.current_map.name
*)
(** [move_player_left] change the current pos (col', row') of player to 
    (col'-1, row')*)
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

(**[eat_one_food s food_name] makes the following updates: 
   1. [food_name] is removed from the the player's food array in [s].
   2. the player in [s] increases health and strength by its corresponding food
   health and strength. 
   Returns [()] if the following updates are successful (i.e. when [food_name]
   is a valid food name in the player's inventory.)
   Raises: [UnknownFood food_name] if [food_name] is not a valid food name in 
    player's inventory*)
let eat_one_food s food_name = 
  let eat_food food_array i= 
    match (food_array.(i) : food_item), s.player with
    | Food food, Player t 
      when Foods.Food.get_name food = food_name ->   
      (let health = Foods.Food.get_health food
       and strength = Foods.Food.get_strength food in
       let _ = Player.increase_health t health in
       let _ = Player.increase_strength t strength in
       food_array.(i) <- Null;
       s.player <- Player t; 
       raise SuccessExit)
    | _ -> () in
  try
    (let food_array = (s.food_inventory : food_item array) in
     for i = 0 to (Array.length food_array) - 1 do 
       eat_food food_array i
     done);
    raise (UnknownFood food_name)
  with SuccessExit -> ()

let get_weapon_name_list_of_player_inventory s =
  let array = [|[]|] in
  (for i = 0 to (Array.length s.weapon_inventory) do
     match s.weapon_inventory.(i) with
     | Null -> ()
     | Weapon w -> 
       array.(0) <- (Weapons.Weapon.get_name w) :: array.(0) 
   done); 
  array.(0)

(**[match_weapons s weapon_array i] is a helper function that, if  
   the [weapon_array.(i)] and [s.player] is a valid and defined weapon 
   and player, [s] includes the weapon [weapon_array.(i)] in its inventory 
   and increases the player's health. *)
let equip_weapon_helper s weapon_array i = 
  let for_each_weapon w t j = 
    if s.weapon_inventory.(j) = Null 
    then (s.weapon_inventory.(j) <- Weapon w;
          Player.increase_strength t (Weapons.Weapon.get_strength w);
          s.player <- Player t; 
          raise SuccessExit)
    else () in 
  let execute_valid_weapon_player w t = 
    if List.for_all (fun w1 -> w1 <> Weapons.Weapon.get_name w) 
        (get_weapon_name_list_of_player_inventory s)
    && Player.location t = Weapons.Weapon.get_loc w
    then 
      (weapon_array.(i) <- Null;    
       for j = 0 to (Array.length s.weapon_inventory) - 1 do 
         for_each_weapon w t j 
       done)
    else () in 
  match weapon_array.(i), s.player with
  | Weapon w, Player t -> 
    execute_valid_weapon_player w t
  | _ -> () 

(**[equip_one_weapon s weapon_name] calls [match_weapons] for every single 
   possible weapon in [s], and returns [()] if the [weapon_name] is known in 
   [s]. 
   Raises [UnknownWeapon weapon_name] if the weapon [weapon_name] does not 
   exist in [s].  *)
let equip_one_weapon s weapon_name = 
  try
    (let weapon_array = s.all_weapons in
     for i = 0 to (Array.length weapon_array) - 1 do 
       equip_weapon_helper s weapon_array i  
     done);
    raise (UnknownWeapon weapon_name)
  with SuccessExit ->
    ()


(*map-param related methods *)
let get_player s = 
  match s.player with
  | Player p -> p
  | Died -> failwith "player is dead"


let check_current_linked_map s =
  if get_current_map_name s <> "main" then false, ""
  else 
    try
      let loc = s |> get_player |> Player.location in
      true, (List.assoc loc s.branched_map_info)
    with Not_found ->
      false, ""


let transfer_player_to_branch_map s = 
  let status, name =  check_current_linked_map s in
  if status = false then ()
  else 
    let map = find_one_map_by_name s.all_maps name in
    s.player_old_loc <- s |> get_player |> Player.location;
    s.current_map <- map;
    s.all_enemies_in_current_map <- [||]; (*TODO *)
    s.all_foods_in_current_map <- [||];
    s.all_weapons_in_current_map <- [||];
    Player.switch_loc (get_player s) (1,1)

let check_branch_map_status s = (*true indicates player finished this branched map *)
  get_current_map_name s <> "main" 
  && Array.for_all (fun enemy -> enemy = Null) s.all_enemies_in_current_map 

let transfer_player_to_main_map s =
  if check_branch_map_status s
  then 
    s.current_map <- List.hd s.all_maps;
    Player.switch_loc (get_player s) s.player_old_loc;
    s.all_enemies_in_current_map <- s.all_enemies.(0);
    s.all_foods_in_current_map <- s.all_foods.(0);
    s.all_weapons_in_current_map <- s.all_weapons.(0);
  else
    ()