module type P = sig

  (** The abstract type of values representing a player's skill. *)
  type skill

  (** The abstract type of values representing a player. *)
  type t

  (** [constructor s h l e ()] constructs a new player module located at
      row 1, col 1, with strength [s], health [h], experience [e], 
      at level [l]. *)
  val constructor: 
    ?strength:int ->
    ?health:int -> 
    ?level:int -> 
    ?experience:int -> 
    unit -> t

  (**[level p] is the current level of player [p] *)
  val level : t -> int

  (** [location p] is the current location of player [p]. *)
  val location : t -> int * int

  (** [row p] is the current row coordinate of player [p]. *)
  val row : t -> int 

  (** [col p] is the current col coordinate of player [p]. *)
  val col : t -> int 

  (** [health p] is the current health of player [p]. *)
  val health : t -> int

  (** [max_health p] is the maximum health of a player [p]*)
  val max_health: t -> int

  (** [experience p] is the current experience value of player [p]. *)
  val experience : t -> int 

  (** [strength p] is the current strength of player [p]. *)
  val strength : t -> int

  (**[move_up p m] moves player [p] to [(col, row+1)] within the bounds
     of map [m]. The player [p] stays in its place when such a move causes
     him to move out of bounds. *)
  val move_up : t -> Maps.t -> unit

  (**[move_down p m] moves player [p] to [(col, row-1)] within the bounds
     of map [m]. The player [p] stays in its place when such a move causes
     him to move out of bounds. *)
  val move_down : t -> Maps.t -> unit

  (**[move_right p m] moves player [p] to [(col+1, row)] within the bounds
     of map [m]. The player [p] stays in its place when such a move causes
     him to move out of bounds. *)
  val move_right : t -> Maps.t -> unit

  (**[move_left p m] moves player [p] to [(col-1, row)] within the bounds
     of map [m]. The player [p] stays in its place when such a move causes
     him to move out of bounds. *)
  val move_left : t -> Maps.t -> unit

  (** [reduce_health p h] reduces the health of player [p] by [h].
      Requires: [h] >= 0 *)
  val reduce_health : t -> int -> unit

  (** [reduce_strength p s] reduces the strength of player [p] by [s].
      Requires: [s] >= 0 *)
  val reduce_strength : t -> int -> unit

  (** [increase_experience p e] increases the experience of player [p] by [e].
      Requires: [e] >= 0 *)
  val increase_experience : t -> int -> unit 

  (**[increase_health p h] increases player [p]'s health by [h].*)
  val increase_health: t -> int -> unit

  (**[increase_strength p s] increases player [p]'s strength by [s].*)
  val increase_strength: t -> int -> unit

  (**[switch_loc p loc] changes the location of player [p] to [loc].  *)
  val switch_loc: t -> int * int -> unit (*this method is pretty dangerous !!*)

  val update_skill: t -> skill list -> unit

  (**[advance_level p] advances player [p] to the next level and updates
     player [p]'s experience as well. If [p] does not have enough experience
     to advance, no change will occur. *)
  val advance_level: t -> unit

  (*  skill-related methods *)

  (** [skill_constructor p n d s] adds a skill to player [p] with 
      name [n], description [d] and strength [s]. *)
  val skill_constructor: 
    name:string ->
    description:string -> 
    strength:int -> 
    cd:int->
    skill

  (**[get_skill_by_skill_name p n] returns skill [s] with 
     the name [n] of player [p]. 
     Raises [UnknownSkill ("skill name " ^ [n] ^ "does not exist")] 
     if the player [p] does not have the skill named [n]. *)
  val get_skill_by_skill_name: t -> string -> skill

  (**[extract_skill_strength_single_skill s] is the strength amount of the 
     skill [s]. *)
  val extract_skill_strength_single_skill: skill -> int

  (**[extract_skill_description_single_skill s] is the description of the 
     skill [s]  *)
  val extract_skill_description_single_skill: skill -> string

  (**[skills_list p] is [p.skills], the skills that the player [p] posesses. *)
  val available_skills_list: t-> skill list

  (**[skill_name s] is the name of the skill [s]. *)
  val skill_name: skill -> string

  val skill_strength: skill -> int

  val skill_description: skill -> string
end

module Player : P = struct

  type skill = {
    name: string;
    description: string;
    mutable strength: int;
    mutable cd: int;
  }

  type t = {
    mutable location : int * int;
    mutable strength : int;
    mutable health : int;
    mutable level : int;
    mutable experience : int;
    mutable skills: skill list;
  }

  (** The exception type of an unknown skill. *)
  exception Unknownskill of string

  (** [skill_constructor n d s] constructs a new skill of 
      strength [s], name [n], description [d].  *)
  let skill_constructor ~name ~description ~strength ~cd = {
    description = description;
    strength = strength;
    name = name;
    cd = cd;
  }

  let constructor 
      ?strength:(strength=10) ?health:(health=100) 
      ?level:(level=1) ?experience:(experience=0) () = 
    {
      location = (1,1);
      strength = strength;
      health = health;
      level = level;
      experience = experience;
      skills = [{
          (* basic skill *)
          name = "punch";
          description = "Basic attacks. 
                        Player uses fists to challenge the evils!";
          strength = strength;
          cd = 0;
        }];
    }

  let location p = p.location

  let health p = p.health

  let max_health p = 70 + 30 * p.level (* this could be mutable *)

  let experience p = p.experience

  let strength p = p.strength

  let level p= p.level

  let col p = fst p.location

  let row p = snd p.location

  (** [move p m c r] changes the player state for which the player [p] 
      moves in map [m];  *)
  let move p m col_diff row_diff = 
    let col' = col_diff + (col p) in
    let row' = row_diff + (row p) in 
    if Maps.bound_check m col' row' then 
      p.location <- (col', row')
    else 
      ()

  let move_left p m = move p m (-1) 0

  let move_right p m = move p m 1 0

  let move_up p m = move p m 0 1

  let move_down p m = move p m 0 (-1)

  let increase_health t hp = 
    let new_health = t.health + hp in
    let max = max_health t in
    if new_health >= max_health t 
    then t.health <- max
    else t.health <- t.health + hp

  let increase_strength t st =
    let incr = st / (List.length t.skills) in
    List.iter (fun s -> s.strength <- s.strength + incr) t.skills

  let reduce_health p h = 
    let new_health = 
      if p.health - h >= 0 then p.health - h else 0 
    in p.health <- new_health

  let reduce_strength p s = 
    let new_strength = 
      if p.strength - s >= 0 then p.strength - s else 0 
    in p.strength <- new_strength

  let advance_level p = 
    let experience_qual = 70 + 30 * p.level in 
    if p.experience >= experience_qual 
    then
      (p.level <- p.level + 1;
       increase_health p 20;
       p.experience <- p.experience mod experience_qual;
       increase_strength p 20)
    else 
      ()

  let increase_experience p e = 
    p.experience <- p.experience + e;
    advance_level p

  let extract_skill_strength_single_skill (skill:skill) = skill.strength

  let extract_skill_description_single_skill skill = skill.description

  let get_skill_by_skill_name t name = 
    match List.filter (fun x -> x.name = name ) t.skills with
    | [] -> raise (Unknownskill 
                     (Printf.sprintf "skill name %s does not exist" name))
    | h::_ -> h

  let available_skills_list t =
    t.skill <- Array.map (fun skill -> 
      if skill.cd <> 0 
      then skill.cd <- skill.cd - 1
      else ()) t.skill;
    List.filter (fun skill -> skill.cd = 0) t.skill

  let skill_name skill = skill.name

  let skill_strength skill =  skill.strength
  
  let skill_description skill = skill.description

  let update_skill t skill_lst = t.skill <- t.skill @ skill_lst
  
  let switch_loc t loc = t.location <- loc
  
end

type skill = Player.skill