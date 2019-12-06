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

(**[get_strength s] is the player's strength at state [s]  *)
let get_strength s = get_player_prop s Player.strength

(**[get_experience s] is the player's experience at state [s]  *)
let get_experience s = get_player_prop s Player.experience

(**[get_level s] is the player's level at state [s]  *)
let get_level s = get_player_prop s Player.level

(**[get_player_map s] is the player's map at state [s]  *)
let get_player_map s = get_player_prop s Player.map

(**[map_cols s] is the # of cols of the player's map at state [s]  *)
let map_cols s = s.current_map.size |> fst

(**[map_rows s] is the # of rows of the player's map at state [s]  *)
let map_rows s = s.current_map.size |> snd


(** Player state update tests *)

let init_state = Engine.init () (* initial pos -> 1,1 *)
let init_health = get_health init_state
let init_strength = get_strength init_state
let init_experience = get_experience init_state
let init_level = get_level init_state
let init_pos = get_pos init_state

(* move right -> 2,1 *)
let _ = Engine.move_player_right init_state
let state1_loc = get_pos init_state

(* move left -> 1,1 *)
let _  = Engine.move_player_left init_state 
let state2_loc = get_pos init_state

(* move left -> 1,1 *)
let _  = Engine.move_player_left init_state 
let state3_loc = get_pos init_state

(* move down -> 1,1 *)
let _  = Engine.move_player_down init_state 
let state4_loc = get_pos init_state

(* move up -> 1,2 *)
let _  = Engine.move_player_up init_state
let state5_loc = get_pos init_state

(* move up -> 1,3 *)
let _  = Engine.move_player_up init_state 
let state6_loc = get_pos init_state

(* move down -> 1,2 *)
let _  = Engine.move_player_down init_state 
let state7_loc = get_pos init_state

(* reduce strength -> init_strength-12 *)
let _ = Player.reduce_strength (get_player init_state) 12 
let state8_strength = get_strength init_state

(* reduce strength -> 0 *)
let _ = Player.reduce_strength (get_player init_state) (init_health+12)
let state9_strength = get_strength init_state

(* increase strength -> 13 *)
let _ = Player.increase_strength (get_player init_state) 13
let state10_strength = get_strength init_state

(* increase experience -> init_experience+10 *)
let _ = Player.increase_experience (get_player init_state) 10
let state11_experience = get_experience init_state

(* increase experience -> init_experience + (level * 100) *)
let _ = Player.increase_experience (get_player init_state) 
    (init_experience + init_level * 100)
let state12_experience = get_experience init_state (* 1010 *)
let state12_level = get_level init_state (* 10 *)

(* advance level -> exp=10, lev=2*)
let _ = Player.advance_level (get_player init_state)
let state13_experience = get_experience init_state (* 10 *)
let state13_level = get_level init_state (* 11 *)

(* change_map to "modified"*)
let _ = Player.change_map (get_player init_state) "modified"
let state14_map = get_player_map init_state 

(* move right towards rightmost column, upmost row *)
let _ = 
  for i = 1 to (map_cols init_state)+12 do 
    Engine.move_player_up init_state;
    Engine.move_player_right init_state;
  done
let state15_loc = get_pos init_state

(* TODO tests: *)
(* get_skill_by_skill_name *)
(* skill_name *)

(* reduce health -> 88 *)
let _ = Player.reduce_health (get_player init_state) 12 
let state_f3_health = get_health init_state

(* increase health -> 100 *)
let _ = Player.increase_health (get_player init_state) 12
let state_f2_health = get_health init_state

(* reduce health -> 0, DIED *)
let _ = Player.reduce_health (get_player init_state) (init_health+20)
let state_f_health = get_health init_state
let player_f = init_state.player


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
  make_test "reduce strength by 12" state8_strength (init_health-12);
  make_test "reduce all strength" state9_strength 0;
  make_test "increase strength by 13" state10_strength 13;
  make_exc_test "advance level attempt" 
    (init_state |> get_player |> Player.advance_level)
    (let exp = init_state |> get_experience |> string_of_int in 
     let s = "cannot advance level without experience " ^ exp in 
     Player.Illegal s);
  make_test "increase exp" state11_experience 
    (init_experience + 10);
  make_test "increase exp to advance" state12_experience 
    (init_experience + 10 + 100 * init_level);
  make_test "increase exp to advance" state12_level 10;
  make_test "advance level" state13_experience (init_experience + 10);
  make_test "advance level" state13_level 11;
  make_test "change map" state14_map "modified";
  make_test "upper right bound" state15_loc 
    (map_cols init_state, map_rows init_state);

  make_test "reduce health by 12" state_f3_health (init_health-12);
  make_test "increase all health" state_f2_health init_health;
  make_test "reduce all health" state_f_health 0;
  make_test "died" player_f Died;
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