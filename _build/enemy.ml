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

module Goblin: EnemySig = struct
  type skills = {
    descr: string;
    strength: int
  }

  type t = {
    (*static fields *)
    id: string;
    name: string;
    descr: string;
    exp: int;
    strength: int;
    level: int;
    (*dynamic fields *)
    pos: int * int;
    hp: int;
    skills: skills option;
  }

  let get_id s = s.id

  let get_name s = s.name 

  let get_description s = s.descr

  let get_experience s = s.exp

  let get_level s = s.level

  let get_hp s = s.hp

  let get_pos s = s.pos

  let get_normal_strength s = s.strength

  let set_move s d = {s with pos = d}

  let reduce_hp s d = {s with hp = s.hp - d}

  (* new methods *)
  let get_skills_description s = (Option.get (s.skills)).descr

  let get_skills_strength s = (Option.get (s.skills)).strength

  let skill_constructor ~skill_descr ~skill_strength = 
    {
      descr = skill_descr;
      strength = skill_strength;
    }

  let constructor ~pos ~level 
      ~exp ~strength ~hp 
      ~id ~name  ~descr ?skills:(skills = None) ()=
    {
      id = id;
      name = name;
      descr = descr;
      exp = exp;
      level = level;
      pos = pos;
      strength = strength;
      hp = hp;
      skills = skills
    }
end

module Minion: EnemySig = struct
  include Goblin
end

module Witch : EnemySig = struct
  include Goblin
end
