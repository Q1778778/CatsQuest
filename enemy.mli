module type EnemySig = sig
  type t
  type skills
  (* Static fields                  *)
  (* Onle getters exist             *)
  val get_id: t -> string
  val get_name: t -> string
  val get_description: t -> string
  val get_experience: t -> int
  val get_skills_description: t -> string
  val get_skills_strength: t -> int
  (*if the difficulty of the game can be changed, then the strength can be
    changed   *)
  val get_normal_strength: t -> int 
  val get_level: t -> int

  (* Dynamic fields.                *)
  (* Both setters and getters exist *)

  (* Setters                        *)
  val set_move: t -> int * int -> t
  val reduce_hp: t -> int -> t

  (* Getters                        *)
  val get_hp: t -> int
  val get_pos: t -> int * int  

  val skill_constructor: 
    skill_descr: string -> 
    skill_strength: int -> 
    skills

  val constructor:
    pos:int * int ->
    level:int ->
    exp:int ->
    strength:int ->
    hp:int ->
    id:string -> 
    name:string -> 
    descr:string -> 
    ?skills: skills option ->unit->
    t
end


module Witch : EnemySig
module Minion :EnemySig
module Goblin : EnemySig