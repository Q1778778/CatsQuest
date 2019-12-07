open Player

module type EnemySig = sig

  (** The abstract type of values representing an enemy. *)
  type t

  (** The abstract type of values representing an enemy's skill. *)
  type skills

  (** The exception type of an unknown skill. *)
  exception UnknownSkill of string

  (* Static fields                  *)
  (* Onle getters exist             *)

  (**[get_id e] returns the id # of the enemy [e]. *)
  val get_id: t -> string

  (**[get_name e] returns the name of the enemy [e]. *)
  val get_name: t -> string

  (**[get_description e] returns the description of the enemy [e]. *)
  val get_description: t -> string

  (**[get_experience e] returns the experience amount of the enemy [e]. *)
  val get_experience: t -> int

  (**[get_all_skills_name e] returns a list of all the skill names of 
     the enemy [e].  *)
  val get_all_skills_name: t -> string list

  (**[get_one_skill_strength_by_name e n] returns the skill [s] if the enemy
     possesses the skill named [n]; otherwise, raises [UnknownSkill n]. *)
  val get_one_skill_strength_by_name: t -> string -> int

  (*if the difficulty of the game can be changed, then the strength can be
    changed   *)

  (**[get_all_skills_name_prob_and_strength_to_assoc_list e] returns 
     [[(n1,p1,s1); (n2,p2,s2); ... ]]
     where [ni] is the name of an enemy [e]'s skill at index [i], 
     [pi] is the skill probability of enemy [e]'s skill at index [i], 
     and [si] is the strength of the enemy [e]'s particular skill at index [i], 
     for all [i] between 1 and [List.length s.skills], inclusive. *)
  val get_all_skills_name_prob_and_strength_to_assoc_list: 
    t -> (string * float * int) list

  (**[get_level e] returns the current level that the enemy [e] is on. *)
  val get_level: t -> int

  (* Dynamic fields.                *)
  (* Both setters and getters exist *)

  (* Setters                        *)
  (**[reduce_hp e d] reduces the enemy's hp amount by [d]. *)
  val reduce_hp: t -> int -> unit

  (* Getters                        *)
  (**[get_hp e] returns the current hp amount of the enemy [e] *)
  val get_hp: t -> int

  (**[get_pos e] returns the current position of the enemy [e] *)
  val get_pos: t -> int * int  

  (**[get_max_hp e] returns the maximum hp of the enemy [e] *)
  val get_max_hp: t-> int

  (**[single_skill_constructor n s p] constructs a new skill with the name 
     [n], strength [s] and skill probability [p]. *)
  val single_skill_constructor: 
    skill_name: string -> 
    skill_strength: int -> 
    skill_probability: float ->
    skills

  (**[constructor p l exp h id n d max_hp s] constructs a new enemy module 
     located at position [p] with experience [exp], hp [hp], id # [id], 
     name [n], maximum hp [max_hp] and a list of skills [s] at level [l]. *)
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
    gainables: Player.skill list ->
    t
end

module Enemy: EnemySig = struct
  exception UnknownSkill of string

  type skills = {
    skill_name: string;
    skill_strength: int;
    skill_probability: float;
  }

  (*i set these fields as mutable because there is a chance that we will modify
    it in MS2 *)
  type t = {
    (*static fields *)
    id: string;
    name: string;
    descr: string;
    max_hp: int;
    gainables: Player.skill list;
    mutable exp: int;
    mutable level: int;
    (*dynamic fields *)
    mutable pos: int * int;
    mutable hp: int;
    mutable skills: skills list;
  }

  (* getters are defined here *)
  let get_id s = s.id

  let get_name s = s.name 

  let get_description s = s.descr

  let get_experience s = s.exp

  let get_level s = s.level

  let get_hp s = s.hp

  let get_pos s = s.pos

  let get_max_hp s=s.max_hp

  (* setters are defined here *)
  let reduce_hp s d = s.hp <- (s.hp - d)

  let get_all_skills_name s = 
    List.map (fun x -> x.skill_name) s.skills

  let get_all_skills_name_prob_and_strength_to_assoc_list s = 
    List.map (fun x -> (x.skill_name, x.skill_probability, x.skill_strength)) 
      s.skills

  let get_one_skill_strength_by_name s name = 
    try (match List.find (fun x -> x.skill_name = name) s.skills with 
        | a -> a.skill_strength) 
    with Not_found -> raise (UnknownSkill name)

  let single_skill_constructor ~skill_name ~skill_strength ~skill_probability 
    :skills = 
    {
      skill_name = skill_name;
      skill_strength = skill_strength;
      skill_probability = skill_probability;
    }

  let constructor ~pos ~level ~exp 
      ~hp ~id ~name  
      ~descr ~max_hp ~skills ~gainables =
    {
      id = id;
      name = name;
      descr = descr;
      exp = exp;
      level = level;
      pos = pos;
      hp = hp;
      max_hp = max_hp;
      skills = skills;
      gainables = gainables;
    }
end