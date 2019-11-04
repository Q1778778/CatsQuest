open Enemy
open Player
open Map
open Yojson.Basic.Util


(*!!!!!!!!!!!!!!!!!!!!!!!!!                                              *)
(*some functions below required an id, which is created by the functions *)
(*instead of contained in json                                           *)

type enemy = 
  | Witch of Witch.s 
  | Goblin of Goblin.t 
  | Minion of Minion.t
  | Delete

type player = 
  | Player of P.t 
  | Died

type item = 
  | Weapon of P.weapon 
  | Food of P.food 
  | Eaten  (*Eaten means the food is eaten. We cannot destroy weapon in game *)

type map = {
  player: int * int;
  enemy: (enemy * (int * int)) list;
}
type state = {
  player: player;
  map: map;
  items: item list;
  enemies: enemy list;
}

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
    let id = ((id |> int_of_string) + 1) |> Int.to_string in
    let descr = j |> member "description" |> to_string in
    let exp = j |> member "experience" |> to_int in
    let level = j |> member "level" |> to_int in
    (* random init pos *)
    let pos = ((Random.int 10)+1, (Random.int 10)+1) in
    let strength = j |> member "strength" |> to_int in
    let hp = j |> member "HP" |> to_int in
    let lst = j |> member "special skills" in
    let skills_descr = lst |> member "description" |> to_string in
    let skill_strength = lst |> member "strength" |> to_int in
    Witch.constructor ~pos ~level ~exp 
      ~skill_strength ~strength ~hp ~id ~descr ~skills_descr 
  )

let goblin_or_minion_builder j id: enemy = 
  Minion ( let id = ((id |> int_of_string) + 1) |> Int.to_string in
           let descr = j |> member "description" |> to_string in
           let exp = j |> member "experience" |> to_int in
           let level = j |> member "level" |> to_int in
           let pos = ((Random.int 10)+1, (Random.int 10)+1) in 
           let strength = j |> member "strength" |> to_int in
           let hp = j |> member "HP" |> to_int in
           Minion.constructor ~pos ~level ~exp ~strength ~hp ~id ~descr )


let browse_one_enemy_json j id: enemy = 
  match j with
  | "enemy-witch.json" -> witch_builder (from_file j) id
  | "enemy-goblin.json"
  | "enemy-minion.json" -> goblin_or_minion_builder (from_file j) id
  | _ -> failwith "invalid input json name"


(**[main_engine_enemy ()] read all enemy json files in current directory*)
let main_engine_enemy : enemy list =
  try
    List.map [] |> browse_dir_enemy (Unix.opendir ".") 
  with Unix.Unix_error(Unix.ENOENT, _ ,_ ) ->
    raise Failure "directory 'enemy' did not exist"




let main () = 
  match Yojson.Basic.from_file "map.json" |> from_json with
  | exception _ -> ANSITerminal.(print_string [red]
                                   "no map.json file found. check your directory"
                                | s -> 
