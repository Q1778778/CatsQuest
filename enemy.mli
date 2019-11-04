module type EnemySig = sig
  type t
  (* Static fields                  *)
  (* Onle getters exist             *)
  val get_id: t -> string
  val get_name: t -> string
  val get_description: t -> string
  val get_experience: t -> int
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
  val constructor:
    ?pos:int * int ->
    ?level:int ->
    ?exp:int ->
    ?strength:int ->
    ?hp:int ->
    id:string -> name:string -> descr:string -> t
end

module type EnemyAugmentedSig = sig
  type skills 
  type s 

  val get_skills_description: s -> string
  val get_skills_strength: s -> int
  include EnemySig with type t := s
  val constructor :
    ?pos:int * int ->
    ?level:int ->
    ?exp:int ->
    ?skills_strength:int ->
    ?strength:int ->
    ?hp:int ->
    id:string -> name:string -> descr:string -> skills_descr: string -> s
end
module Witch : EnemyAugmentedSig
module Minion :EnemySig
module Goblin : EnemySig