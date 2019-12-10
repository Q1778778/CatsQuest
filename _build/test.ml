open OUnit2
open Player
open Enemy
open Engine
open Foods
open Maps
open Weapons

(**[get_player s] returns [t] if the player at state [s] yields [Player t].
   Otherwise, fails with "died" *)
let get_player s = 
  match s.player with
  | Player t -> t
  | Died -> failwith "died"

(**[get_player_prop s f] is the property obtained by calling the function [f] 
   on the player at state [s]. 
   Requires: [f] is a defined function inside the [Player] module *)
let get_player_prop s f = s |> get_player |> f

(**[get_pos s] is the player's location at state [s]  *)
let get_pos s = get_player_prop s Player.location 

(**[get_health s] is the player's health at state [s]  *)
let get_health s = get_player_prop s Player.health

(**[get_max_health s] is the player's maximum health at state [s] *)
let get_max_health s = get_player_prop s Player.max_health

(**[get_experience s] is the player's experience at state [s]  *)
let get_experience s = get_player_prop s Player.experience

(**[get_level s] is the player's level at state [s]  *)
let get_level s = get_player_prop s Player.level

(**[map_cols s] is the # of cols of the player's map at state [s]  *)
let map_cols s = s.current_map.size |> fst

(**[map_rows s] is the # of rows of the player's map at state [s]  *)
let map_rows s = s.current_map.size |> snd

(**[delete_all_enemies_in_current_map s] removes all enemies in the 
   current map in state [s].   *)
let delete_all_enemies_in_current_map s = 
  for i = 0 to Array.length s.all_enemies_in_current_map - 1 do
    s.all_enemies_in_current_map.(i) <- Deleted
  done

(**[array_loc e arr] is the index of [e] in array [arr]

   Raises:
   Failure if [e] is not in [arr]*)
let array_loc element arr = 
  let store' = ref 0 in 
  let inner_searcher store = 
    for i = 0 to Array.length arr - 1 do
      if arr.(i) = element 
      then (store := i; raise SuccessExit )
      else ()
    done in
  try
    inner_searcher store';
    raise (Failure "not in this array")
  with SuccessExit ->
    !store'

(**[switch_to_different_map s pos] switches to the map indexed at 
   [pos]. *)
let switch_to_different_map s pos = 
  s.current_map <- List.nth s.all_maps pos;
  s.all_enemies_in_current_map <- s.all_enemies.(pos);
  s.all_foods_in_current_map <- s.all_foods.(pos);
  s.all_weapons_in_current_map <- s.all_weapons.(pos);
  s.current_map_in_all_maps <- pos


(** Player state update tests *)

(**[get_enemy t] returns the enemy [e] if [t] is a valid enemy type, 
   and raises [Failure "invalid enemy"] otherwise. *)
let get_enemy = function
  | Deleted -> failwith "invalid enemy"
  | Enemy e -> e

(**[get_food t] returns the food [f] if [t] is a valid food type, 
   and raises [Failure "invalid food"] otherwise. *)
let get_food = function
  | Eaten -> failwith "invalid food"
  | Food f -> f

(**[get_weapon t] returns the weapon [w] if [t] is a valid weapon type, 
   and raises [Failure "invalid weapon"] otherwise. *)
let get_weapon = function
  | Empty -> failwith "invalid weapon"
  | Weapon w -> w

(**[enemy_is_alive t] returns [true] if [t] is a valid enemy and [false]
   otherwise.  *)
let enemy_is_alive = function
  | Deleted -> false
  | Enemy e -> true

(**[get_one_enemy_pos s] returns the location of the first enemy 
   in the current map at state [s].  *)
let get_one_enemy_pos s = 
  s.all_enemies_in_current_map.(0) |> get_enemy |> Enemy.get_pos

(**[get_first_alive_enemy_at_index s pos] returns the first alive enemy of
   all the enemies indexed map [pos] at state [s]. 
   Raises: [Failure "all enemies are dead"] if all enemies are dead.  *)
let get_first_alive_enemy_at_index s pos =
  let store = ref s.all_enemies_in_current_map.(0) in
  let rec inner_searcher enemies_array = 
    for i = 0 to Array.length enemies_array - 1 do
      match enemies_array.(i) with
      | Enemy e -> store := Enemy e;
        raise SuccessExit
      | _ -> ()
    done in
  try
    inner_searcher s.all_enemies.(pos);
    failwith "all enemies are dead"
  with SuccessExit ->
    !store |> get_enemy

(**[get_first_available_food_at_index s pos] returns the first available food 
   of all the foods indexed map [pos] at state [s].
   Raises: [Failure "no foods"] if no food is available.  *)
let get_first_available_food_at_index s pos =
  let store = ref s.all_foods_in_current_map.(0) in
  let rec inner_searcher food_array = 
    for i = 0 to Array.length food_array - 1 do
      match food_array.(i) with
      | Food f -> 
        store := Food f;
        raise SuccessExit
      | _ -> ()
    done in
  try
    inner_searcher s.all_foods.(pos);
    failwith "no foods"
  with SuccessExit ->
    !store |> get_food

(**[get_first_available_weapon_at_index s pos] returns the first available 
   weapon of all the weapons indexed map [pos] at state [s]. 
   Raises: [Failure "no weapons"] if no food is available.  *)
let get_first_available_weapon_at_index s pos =
  let store = ref s.all_weapons_in_current_map.(0) in
  let rec inner_searcher weapon_array = 
    for i = 0 to Array.length weapon_array - 1 do
      match weapon_array.(i) with
      | Weapon w -> 
        store := Weapon w;
        raise SuccessExit
      | _ -> ()
    done in
  try
    inner_searcher s.all_weapons.(pos);
    failwith "no weapons"
  with SuccessExit ->
    !store |> get_weapon



(*                initiate testing param                    *)


(* initial pos -> 1,1 
   initial map -> 0 *)
let state = Engine.init () 
let init_health = get_health state
let init_experience = get_experience state
let init_level = get_level state
let init_pos = get_pos state

(* move right -> 2,1 *)
let _ = Engine.move_player_right state
let state1_loc = get_pos state

(* move left -> 1,1 *)
let _  = Engine.move_player_left state 
let state2_loc = get_pos state

(* move left -> 1,1 *)
let _  = Engine.move_player_left state 
let state3_loc = get_pos state

(* move down -> 1,1 *)
let _  = Engine.move_player_down state 
let state4_loc = get_pos state

(* move up -> 1,2 *)
let _  = Engine.move_player_up state
let state5_loc = get_pos state

(* move up -> 1,3 *)
let _  = Engine.move_player_up state 
let state6_loc = get_pos state

(* move down -> 1,2 *)
let _  = Engine.move_player_down state 
let state7_loc = get_pos state


(* increase experience -> init_experience+10 *)
let _ = Player.increase_experience (get_player state) 10
let state11_experience = get_experience state
let state11_level = get_level state


let _ = Player.advance_level (get_player state)
let state11'_experience = get_experience state
let state11'_level = get_level state
let state11'_health = get_health state

(* increase experience enough to advance *)
let _ = Player.increase_experience (get_player state) 
    (state |> get_player |> Player.level_up_expereince)
let state12_experience = get_experience state 
let state12_level = get_level state 
let state12_health = get_health state

(* move right towards rightmost column, upmost row *)
let _ = 
  for i = 1 to (map_cols state) + 15 do 
    Engine.move_player_up state;
    Engine.move_player_right state;
  done
let state15_loc = get_pos state

(* TODO tests: *)
(* get_skill_by_skill_name *)
(* skill_name *)

(* reduce health by 12 *)
let _ = Player.reduce_health (get_player state) 12 
let state_f3_health = get_health state

(* increase health to max *)
let _ = Player.increase_health (get_player state) (get_max_health state)
let state_f2_health = get_health state

(* reduce health -> 0, DIED *)
let _ = Player.reduce_health (get_player state) (get_max_health state)
let state_f_health = get_health state
let player_f = state.player


(** Enemy state update tests *)
let new_s = init ()

let one_e = get_first_alive_enemy_at_index new_s 0

let init_1_hp = Enemy.get_hp one_e
let init_1_level = Enemy.get_level one_e
let skill_list_1 = 
  Enemy.get_all_skills_name_prob_and_strength_to_assoc_list one_e <> []

let _ = Enemy.reduce_hp one_e 10

let init_2_hp = Enemy.get_hp one_e
let init_2_level = Enemy.get_level one_e
let skill_list_2 = 
  Enemy.get_all_skills_name_prob_and_strength_to_assoc_list one_e <> []

(** Food state update tests *)

let new_s1 = init ()

let one_f = get_first_available_food_at_index new_s1 0

let init_1f_strength = Food.get_strength one_f
let init_1f_health = Food.get_health one_f

let _ = Food.set_loc one_f (2,3)

let init_2f_strength = Food.get_strength one_f
let init_2f_health = Food.get_health one_f
let f_new_loc = Food.get_loc one_f


(** Weapon state update tests *)
let new_s2 = init ()

let one_w = get_first_available_weapon_at_index new_s2 0
let init_1w_name = Weapon.get_name one_w
let init_1w_strength = Weapon.get_strength one_w


let _ = Weapon.set_loc one_w (2,3)
let init_2w_name = Weapon.get_name one_w
let init_2w_strength = Weapon.get_strength one_w
let w_new_loc = Weapon.get_loc one_w

(** Branched map state update tests *)


(** Engine update tests *)
let new_e = init ()
let new_e_fir_e = get_first_alive_enemy_at_index new_e 0
let new_e_fir_level = new_e_fir_e |> Enemy.get_level
let new_e_fir_health = new_e_fir_e |> Enemy.get_hp
let new_e_fir_pos = new_e_fir_e |> Enemy.get_pos

let _ = strengthen_whole_map new_e

let new_e_sec_e = get_first_alive_enemy_at_index new_e 0

let new_e_sec_level = new_e_sec_e |> Enemy.get_level
let new_e_sec_health = new_e_sec_e |> Enemy.get_hp
let new_e_sec_pos = new_e_sec_e |> Enemy.get_pos

let branch_1_loc = new_e |> list_of_entrance_loc_to_branch_map |> List.hd
let _ = Player.switch_loc (get_player new_e) branch_1_loc

let new_e_branch_1 = Engine.transfer_player_to_branch_map new_e

let new_e_index = new_e.current_map_in_all_maps (* != 0 *)
let new_e_name = new_e.current_map.name 
let new_e_food = array_loc new_e.all_foods_in_current_map new_e.all_foods
let new_e_enemy = array_loc new_e.all_enemies_in_current_map 
    new_e.all_enemies
let new_e_weapon = array_loc new_e.all_weapons_in_current_map
    new_e.all_weapons



(**[make_test n i o] constructs a test [n] to check whether [i] is equal 
   to [o]. *)
let make_test n i o = 
  n >:: (fun _ ->  assert_equal i o)

(**[make_exc_test n f e] constructs a test [n] where applying function [f]
   must produce an exception [e]. *)
let make_exc_test n f e = 
  let func = fun () -> f in 
  n >:: (fun _ -> assert_raises e func)


(** Test suite for player states  *)
let player_state_tests = [
  make_test "init" init_pos (1,1);

  make_test "R" state1_loc (2,1);

  make_test "RL" state2_loc (1,1);

  make_test "RLL = RL" state3_loc (1,1);

  make_test "RLLD = RL" state4_loc (1,1);

  make_test "RLU" state5_loc (1,2);

  make_test "RLUU" state6_loc (1,3);

  make_test "RLUUD" state7_loc (1,2);

  make_test "advance level failed" state11_experience state11'_experience;

  make_test "advance level failed" state11_level state11'_level;

  make_test "increase exp" state11_experience 
    (init_experience + 10);

  make_test "increase exp to advance 1" state12_experience 
    (init_experience + 10);

  make_test "increase exp to advance 2" state12_level (init_level+1);

  make_test "upper right bound" state15_loc 
    (map_cols state, map_rows state);

  make_test "health before advanced level" state11'_health init_health;

  make_test "health after advanced level" state12_health (init_health+20);

  make_test "reduce health by 12" state_f3_health (init_health+8);

  make_test "increase all health" state_f2_health (get_max_health state);

  make_test "reduce all health" state_f_health 0;
]

(** Test suite for enemy states  *)
let enemy_state_tests = [
  make_test "reduce enemy hp" (init_2_hp - init_1_hp) 10;

  make_test "always ensure the skills output of enemies are valid"
    (skill_list_2) true;

  make_test "always ensure the skills output of enemies are valid"
    (skill_list_1) true;

  make_test "enemy level is a static field and it shouldn't be changed"
    (init_1_level = init_2_level) true;
]

(** Test suite for food states  *)
let food_state_tests = [
  make_test "food's gainable strength should never be changed when we move food"
    (init_1f_strength = init_2f_strength) true;

  make_test "food's gainable health should never be changed when we move food"
    (init_1f_health = init_2f_health) true;

  make_test "change food's location" f_new_loc (2,3);
]

(** Test suite for weapon states  *)
let weapon_state_tests = [
  make_test "weapon's name should be consistent throughout moving"
    (init_1w_name = init_2w_name) true;

  make_test "weapon's strength should be consistent throughout moving"
    (init_1w_strength = init_2w_strength) true;

  make_test "weapon's new location should be correct after moving"
    w_new_loc (2, 3);
]

(** Test suite for branched map states  *)
let branched_map_tests = []

(** Test suites for engine states *)
let engine_state_tests = [
  make_test "successfully strengthen enemy's health after play"
    new_e_sec_health (new_e_fir_health+10);

  make_test "successfully strengthen enemy's level after play"
    new_e_sec_level (new_e_fir_level+1);

  make_test "enemy's pos shouldn't be changed after strengthening"
    new_e_fir_pos new_e_sec_pos;

  make_test "sucessfully transfer to branch map"
    (new_e_index <> 0) true;

  make_test "sucessfully transfer to branch map"
    (new_e_name <> "main") true;

  make_test "sucessfully transfer to branch map"
    (new_e_weapon <> 0) true;

  make_test "sucessfully transfer to branch map"
    (new_e_food) new_e_weapon;

  make_test "sucessfully transfer to branch map"
    new_e_weapon new_e_enemy;
]

(** All the test suites  *)
let suite =
  "test suite for A2" >::: 
  List.flatten [
    player_state_tests;
    enemy_state_tests;
    food_state_tests;
    weapon_state_tests;
    branched_map_tests;
    engine_state_tests;
  ]

(** Run all the test suites  *)
let _ = run_test_tt_main suite