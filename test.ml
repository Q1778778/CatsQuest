open OUnit2
open Player
open Enemy
open Engine

let get_player s = 
  match s.player with
  | Player t -> t
  | Died -> failwith ""

let get_pos s = s |> get_player |> Player.location 
let get_health s = s |> get_player |> Player.health
let get_strength s = s |> get_player |> Player.strength
let get_experience s = s |> get_player |> Player.experience
let get_level s = s |> get_player |> Player.level
let get_player_map s = s |> get_player |> Player.map

(** Player state update tests *)

(* initial pos -> 1,1 *)
let init_state = Engine.init ()
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

(* move left -> 1,1 *)
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

(* reduce strength -> 88 *)
let _ = Player.reduce_strength (get_player init_state) 12 
let state6_strength = get_strength init_state

(* reduce strength -> 0 *)
let _ = Player.reduce_strength (get_player init_state) 120
let state7_strength = get_strength init_state

(* increase strength -> 13 *)
let _ = Player.increase_strength (get_player init_state) 13
let state8_strength = get_strength init_state

(* increase experience -> 10 *)
let _ = Player.increase_experience (get_player init_state) 10
let state9_experience = get_experience init_state

(* increase experience -> 110 *)
let _ = Player.increase_experience (get_player init_state) 100
let state10_experience = get_experience init_state (* 110 *)
let state10_level = get_level init_state (* 1 *)

(* advance level -> exp=10, lev=2*)
let _ = Player.advance_level (get_player init_state)
let state11_experience = get_experience init_state (* 10 *)
let state11_level = get_level init_state (* 2 *)

(* TODO tests: *)
(* get_skill_by_skill_name *)
(* skill_name *)

(* change_map to "modified"*)
let _ = Player.change_map (get_player init_state) "modified"
let state12_map = get_player_map init_state 

(* reduce health -> 88 *)
let _ = Player.reduce_health (get_player init_state) 12 
let state_f3_health = get_health init_state

(* increase health -> 100 *)
let _ = Player.increase_health (get_player init_state) 12
let state_f2_health = get_health init_state

(* reduce health -> 0, DIED *)
let _ = Player.reduce_health (get_player init_state) 120
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

let player_state_tests = [
  make_test "init" init_pos (1,1);
  make_test "R" state1_loc (2,1);
  make_test "RL" state2_loc (1,1);
  make_test "RLU" state3_loc (1,2);
  make_test "RLUU" state4_loc (1,3);
  make_test "RLUUD" state5_loc (1,2);
  make_test "reduce strength by 12" state6_strength 88;
  make_test "reduce all strength" state7_strength 0;
  make_test "increase strength by 13" state8_strength 13;
  (* make_test "increase exp by 10" state9_experience 10; *)
  make_exc_test "advance level attempt" 
    (Player.advance_level (get_player init_state)) 
    (Player.Illegal ("cannot advance level without experience " ^ 
                     (init_state |> get_experience |> string_of_int)));
  make_test "increase exp to advance" state10_experience 110;
  make_test "increase exp to advance" state11_level 1;
  make_test "advance level" state11_experience 10;
  make_test "advance level" state11_level 10;
  make_test "change map" state12_map "modified";

  make_test "reduce health by 12" state_f3_health 88;
  make_test "increase all health" state_f2_health 100;
  make_test "reduce all health" state_f_health 0;
  make_test "died" player_f Died;
]

let enemy_state_tests = [

]

let suite =
  "test suite for A2" >::: 
  List.flatten [
    player_state_tests;
    enemy_state_tests;
  ]


let _ = run_test_tt_main suite