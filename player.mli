module type P = sig
  (** The abstract type of values representing the player's game state. *)
  type t 

  (** The type representing the result of an attempted movement. *)
  type result = Legal of t | Illegal 

  (** [location p] is the current location of player [p]. *)
  val location : t -> int * int

  (** [health p] is the current health of player [p]. *)
  val health : t -> int

  (** [experience p] is the current experience value of player [p]. *)
  val experience : t -> int 

  (** [strength p] is the current strength of player [p]. *)
  val strength : t -> int

  (** [experience_qual p] is the minimum amount of experience for a level 
      required for a player [p] to advance to the next level. *)
  val experience_qual : t -> int

  (** [move_north p m] returns the new player state [r] after p moves north.
      [r] is [Legal p'] if player [p] is able to make the move, and [Illegal]
      otherwise. *)
  val move_north : t -> Map.t -> result

  (** [move_south p m] returns the new player state [r] after p moves south.
      [r] is [Legal p'] if player [p] is able to make the move, and [Illegal]
      otherwise. *)
  val move_south : t -> Map.t -> result

  (** [move_east p m] returns the new player state [r] after p moves east.
      [r] is [Legal p'] if player [p] is able to make the move, and [Illegal]
      otherwise. *)
  val move_east : t -> Map.t -> result

  (** [move_west p m] returns the new player state [r] after p moves west.
      [r] is [Legal p'] if player [p] is able to make the move, and [Illegal]
      otherwise. *)
  val move_west : t -> Map.t -> result

end 
