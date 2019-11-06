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

  val single_skill_constructor: 
    skill_name: string -> 
    skill_strength: int -> 
    skills

  val constructor:
    pos:int * int ->
    level:int ->
    exp:int ->
    hp:int ->
    id:string -> 
    name:string -> 
    descr:string -> 
    skills: skills list ->
    t
end

module Enemy: EnemySig = struct
  exception UnknownSkill of string

  type skills = {
    descr: string;
    strength: int
  }

(*i set these fields as mutable because there is a chance that we will modify
it in MS2 *)
  type t = {
    (*static fields *)
    id: string;
    name: string;
    descr: string;
    mutable exp: int;
    mutable level: int;
    (*dynamic fields *)
    mutable pos: int * int;
    mutable hp: int;
    mutable skills: skills list;
  }

  let get_id s = s.id

  let get_name s = s.name 

  let get_description s = s.descr

  let get_experience s = s.exp

  let get_level s = s.level

  let get_hp s = s.hp

  let get_pos s = s.pos

  let set_move s d = s.pos <- d

  let reduce_hp s d = s.hp <- (s.hp - d)

  (* new methods *)
  let get_all_skills_name s = 
    List.map (fun x -> x.name) s.skills

  let get_one_skill_strength_by_name s name = 
    match List.find (fun x -> x.name = name) s.skills with
    | Not_found -> raise (UnknownSkill name)
    | a -> a.strength

  let single_skill_constructor ~skills_name ~skills_strength = 
    {
      name = skills_name;
      strength = skills_strength;
    }

  let constructor ~pos ~level 
      ~exp ~strength ~hp 
      ~id ~name  ~descr ~skills =
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
