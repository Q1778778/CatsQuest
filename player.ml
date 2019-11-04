
open Yojson.Basic.Util

module type P = sig
  (** The abstract type of values representing the player's game state. *)
  type t

  (** The type representing the result of an attempted movement. *)
  type result = Legal of t | Illegal 

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

  (** [move_north p m] returns the new player state [r] after p moves north.
      [r] is [Legal p'] if player [p] is able to make the move, and [Illegal]
      otherwise. *)
  val move_north : t -> Maps.t -> result

  (** [move_south p m] returns the new player state [r] after p moves south.
      [r] is [Legal p'] if player [p] is able to make the move, and [Illegal]
      otherwise. *)
  val move_south : t -> Maps.t -> result

  (** [move_east p m] returns the new player state [r] after p moves east.
      [r] is [Legal p'] if player [p] is able to make the move, and [Illegal]
      otherwise. *)
  val move_east : t -> Maps.t -> result

  (** [move_west p m] returns the new player state [r] after p moves west.
      [r] is [Legal p'] if player [p] is able to make the move, and [Illegal]
      otherwise. *)
  val move_west : t -> Maps.t -> result

  (** [advance_level p] is [Legal p'] if the player [p] has reached the 
       experience value for that level alone, and [Illegal] otherwise.*)
  val advance_level : t -> result 

end 

module Player = struct

  (** The abstract type of values representing keyboard keys. *)
  type key = Up | Down | Left | Right | W | A | S | D | Space | Null

  type t = {
    location : int * int;
    strength : int;
    health : int;
    level : int;
    experience : int;
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

  type result = Legal of t | Illegal of string

  (* [move p m r c] is a new state for which the player [p] moves north,
     south, east, or west in map [m]; i.e. moves north or south by 
     [row_diff], moves east or west by [col_diff] *)
  let move p m row_diff col_diff = 
    let row = row p in 
    let col = col p in
    if Maps.bound_check m row col then 
      Legal {
        p with 
        location = (row+row_diff, col+col_diff)
      }
    else Illegal "Cannot move out of the map!"

  let move_north p m = move p m (-1) 0

  let move_south p m = move p m 1 0

  let move_east p m = move p m 0 1

  let move_west p m = move p m 0 (-1)

  let advance_level p = 
    let lev = p.level in 
    let experience_qual = 100 * lev in 
    if p.experience >= experience_qual then
      Legal {
        p with 
        level = lev + 1;
        experience = p.experience mod experience_qual;
      }
    else Illegal ("cannot advance level without experience " ^ 
                  (experience_qual |> string_of_int))

end