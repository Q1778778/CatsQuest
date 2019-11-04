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

module Goblin: EnemySig = struct
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

  let constructor 
      ?pos:(pos = 1,1) ?level:(level=5) 
      ?exp:(exp=5) ?strength:(strength=20) ?hp:(hp=70) 
      ~id ~name  ~descr : t =
    {
      id = id;
      name = name;
      descr = descr;
      exp = exp;
      level = level;
      pos = pos;
      strength = strength;
      hp = hp;
    }
end



module Minion: EnemySig = struct
  include Goblin
end

module Witch : EnemyAugmentedSig = struct
  type skills = {
    descr: string;
    strength: int
  }

  type s = {
    (*static fields *)
    id: string;
    name: string;
    descr: string;
    exp: int;
    strength: int;
    level: int;
    (*new field in augmented enemy *)
    skills: skills;
    (*dynamic fields *)
    pos: int * int;
    hp: int;
  }

  (* the same as goblin *)
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
  let get_skills_description s = s.skills.descr

  let get_skills_strength s = s.skills.strength

  let constructor 
      ?pos:(pos = 1,1) ?level:(level=5) 
      ?exp:(exp=5) ?skills_strength:(skills_strength=50)
      ?strength:(strength=20) ?hp:(hp=70) 
      ~id:(id:string)  
      ~name:(name:string)
      ~descr:(descr:string)  
      ~skills_descr : s =
    {
      id = id;
      name = name;
      descr = descr;
      exp = exp;
      level = level;
      pos = pos;
      strength = strength;
      hp = hp;
      skills = { descr = skills_descr;
                 strength = skills_strength;}
    }

end
