open OUnit2
open Player
open Enemy
open Engine

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

let delete_all_enemies_in_current_map s = 
  for i = 0 to Array.length s.all_enemies_in_current_map - 1 do
    s.all_enemies_in_current_map.(i) <- Deleted
  done

let switch_to_different_map s pos = 
  s.current_map <- List.nth s.all_maps pos;
  s.all_enemies_in_current_map <- s.all_enemies.(pos);
  s.all_foods_in_current_map <- s.all_foods.(pos);
  s.all_weapons_in_current_map <- s.all_weapons.(pos);
  s.current_map_in_all_maps <- pos
(** Player state update tests *)

let get_enemy = function
  | Deleted -> failwith "invalid"
  | Enemy e -> e

let get_one_enemy_pos s = 
  s.all_enemies_in_current_map.(0) |> get_enemy |> Enemy.get_pos

let state = Engine.init () (* initial pos -> 1,1 *)
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
    (70 + 30 * init_level)
let state12_experience = get_experience state 
let state12_level = get_level state 
let state12_health = get_health state

(* move right towards rightmost column, upmost row *)
let _ = 
  for i = 1 to (map_cols state)+12 do 
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
  (* make_test "died" player_f Died; *)
]

(** Test suite for enemy states  *)
let enemy_state_tests = [

]

let suite =
  "test suite for A2" >::: 
  List.flatten [
    player_state_tests;
    enemy_state_tests;
  ]


let _ = run_test_tt_main suite