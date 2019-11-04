open Map
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
