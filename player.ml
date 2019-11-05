
open Yojson.Basic.Util

module type P = sig
  (** The abstract type of values representing the player's game state. *)
  type t

  (** The exception representing the result of an illegal movement.*)
  exception Illegal of string

  (** [location p] is the current location of player [p]. *)
  val location : t -> int * int

  (** [row p] is the current row coordinate of player [p]. *)
  val row : t -> int 

  (** [col p] is the current col coordinate of player [p]. *)
  val col : t -> int 

  (** [health p] is the current health of player [p]. *)
  val health : t -> int

  (** [experience p] is the current experience value of player [p]. *)
  val experience : t -> int 

  (** [strength p] is the current strength of player [p]. *)
  val strength : t -> int

  (** [move_north p m] updates the player state after [p] moves north,
      returning [unit].
      Raises: [Illegal] if the move results in out of bounds from the map*)
  val move_north : t -> Maps.t -> unit

  (** [move_south p m] updates the player state after [p] moves south,
      returning [unit].
      Raises: [Illegal] if the move results in out of bounds from the map*)
  val move_south : t -> Maps.t -> unit

  (** [move_east p m] updates the player state after [p] moves east,
      returning [unit].
      Raises: [Illegal] if the move results in out of bounds from the map*)
  val move_east : t -> Maps.t -> unit

  (** [move_west p m] updates the player state after [p] moves west,
      returning [unit].
      Raises: [Illegal] if the move results in out of bounds from the map*)
  val move_west : t -> Maps.t -> unit

  (** [reduce_health p h] reduces the health of player [p] by [h].
      Requires: [h] >= 0 *)
  val reduce_health : t -> int -> unit

  (** [reduce_strength p s] reduces the strength of player [p] by [s].
      Requires: [s] >= 0 *)
  val reduce_strength : t -> int -> unit

  (** [increase_experience p e] increases the experience of player [p] by [e].
      Requires: [e] >= 0 *)
  val increase_experience : t -> int -> unit 

  (** [advance_level p] is [Legal p'] if the player [p] has reached the 
       experience value for that level alone, and [Illegal] otherwise.*)
  val advance_level : t -> unit 

  val max_health : int

end 

module Player : P = struct

  (** The abstract type of values representing keyboard keys. *)
  type key = Up | Down | Left | Right | W | A | S | D | Space | Null

  type t = {
    mutable location : int * int;
    mutable strength : int;
    mutable health : int;
    mutable level : int;
    mutable experience : int;
    keys : string list;
  }

  let constructor 
      ~row ~col ?strength:(strength=10) ?health:(health=100) 
      ?level:(level=1) ?experience:(experience=0) ~keys = 
    {
      location = (row,col);
      strength = strength;
      health = health;
      level = level;
      experience = experience;
      keys = keys
    }

  let location p = p.location

  let health p = p.health

  let experience p = p.experience

  let strength p = p.strength

  let max_strength = 100

  let max_health = 100

  let row p = fst p.location

  let col p = snd p.location

  (**[match_keys s] returns the equivalent enum value for the parsed key
     string [s] from the json file. *)
  let match_keys = function 
    | "right" -> Right
    | "left" -> Left
    | "up" -> Up
    | "down" -> Down
    | "w" -> W
    | "a" -> A
    | "s" -> S
    | "d" -> D
    | "space" -> Space
    | _ -> Null

  (* type result = Legal of t | Illegal of string *)
  exception Illegal of string 

  (* [move p m r c] changes the player state for which the player [p] 
     moves north, south, east, or west in map [m]; i.e. moves north or south by 
     [row_diff], moves east or west by [col_diff] *)
  let move p m row_diff col_diff = 
    let row = row p in 
    let col = col p in
    if Maps.bound_check m row col then 
      p.location <- (row+row_diff, col+col_diff)
    else raise (Illegal "Cannot move out of the map!")

  let move_north p m = move p m (-1) 0

  let move_south p m = move p m 1 0

  let move_east p m = move p m 0 1

  let move_west p m = move p m 0 (-1)

  let reduce_health p h = 
    assert(h>=0);
    let new_health = 
      if p.health - h >= 0 then p.health - h else 0 
    in p.health <- new_health

  let reduce_strength p s = 
    assert(s>=0);
    let new_strength = 
      if p.strength - s >= 0 then p.strength - s else 0 
    in p.strength <- new_strength

  let increase_experience p e = 
    assert (e>=0);
    p.experience <- p.experience + e

  let advance_level p =  begin
    let lev = p.level in 
    let experience_qual = 100 * lev in 
    if p.experience >= experience_qual then
      begin
        p.level <- lev + 1;
        p.experience <- p.experience mod experience_qual;
      end
    else 
      let error_msg = ("cannot advance level without experience " 
                       ^ string_of_int experience_qual) in 
      raise (Illegal error_msg)
  end
end