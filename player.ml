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

  (**[map p] is the current map name of which player is currently in*)
  val map : t -> string

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

  (** *)
  val move_up : t -> Maps.t -> unit

  (** *)
  val move_down : t -> Maps.t -> unit

  (** *)
  val move_right : t -> Maps.t -> unit

  (** *)
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

  (** [skill_constructor p n d s] adds a skill to player [p] with 
      name [n], description [d] and strength [s]. *)
  val skill_constructor: 
    player:t ->
    name:string ->
    description:string -> 
    strength:int -> 
    unit

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
  val skills_list: t-> skill list

  (**[skill_name s] is the name of the skill [s]. *)
  val skill_name: skill->string

  (**[increase_health p h] increases player [p]'s health by [h].*)
  val increase_health: t -> int -> unit

  (**[increase_strength p s] increases player [p]'s strength by [s].*)
  val increase_strength: t -> int -> unit

  (**[change_map p map] updates the map name that player [p]'s currently in.*)
  val change_map: t -> string -> unit

  (**[switch_loc p loc] changes the location of player [p] to [loc].  *)
  val switch_loc: t -> int * int -> unit (*this method is pretty dangerous !!*)
end 

module Player : P = struct

  type skill = {
    name: string;
    description: string;
    strength: int;
  }

  type t = {
    mutable location : int * int;
    mutable strength : int;
    mutable health : int;
    mutable level : int;
    mutable experience : int;
    mutable skills: skill list;
    mutable map: string;
  }

  exception Unknownskill of string

  let skill_constructor ~player ~name ~description ~strength = 
    player.skills <- ({
        description = description;
        strength = strength;
        name = name;
      }::player.skills)

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
          name = "punch";
          description = "Basic attacks.
        Player uses fists to challenge the evils!";
          strength = strength;
        }];
      map = "main" (* The initial map should be the main map 
                        so I hard-coded this *)
    }

  let map p = p.map

  let location p = p.location

  let health p = p.health

  let max_health p = 100 + 30 * p.level (* this could be mutable *)

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
    let new_health = t.health + tp in
    let max = max_health t in
    if new_health >= max_health t 
    then t.health <- max;
    else t.health <- t.health + hp
  
  let increase_strength t st =
    t.strength <- t.strength + st

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

  let skills_list t =
    t.skills

  let skill_name skill =
    skill.name

  let change_map t map = 
    t.map <- map

  let switch_loc t loc =
    t.location <- loc
end