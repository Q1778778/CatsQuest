module type EnemySig = sig
  type t
  type skills

  exception UnknownSkill of string
  (* Static fields                  *)
  (* Onle getters exist             *)
  val get_id: t -> string
  val get_name: t -> string
  val get_description: t -> string
  val get_experience: t -> int
  val get_all_skills_name: t -> string list
  val get_one_skill_strength_by_name: t -> string -> int
  (*if the difficulty of the game can be changed, then the strength can be
    changed   *)
  val get_level: t -> int

  (* Dynamic fields.                *)
  (* Both setters and getters exist *)

  (* Setters                        *)
  val set_move: t -> int * int -> unit
  val reduce_hp: t -> int -> unit

  (* Getters                        *)
  val get_hp: t -> int
  val get_pos: t -> int * int  
  val get_max_hp: t-> int

  val get_all_skills_name_prob_and_strength_to_assoc_list: 
    t -> (string * float * int) list

  val single_skill_constructor: 
    skill_name: string -> 
    skill_strength: int ->
    skill_probability: float ->
    skills

  val constructor:
    pos:int * int ->
    level:int ->
    exp:int ->
    hp:int ->
    id:string -> 
    name:string -> 
    descr:string -> 
    max_hp:int->
    skills: skills list ->
    t
end



module Enemy : EnemySig