open Map
module type P = sig
  (** The abstract type of values representing the player's game state. *)
  type t

  (** The abstract type of values representing weapons. *)
  type weapon

  (** The abstract type of values representing foods. *)
  type food

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

  (**[compare_weapons w1 w2] returns [0] if [w1]'s id equal to [w2]'s id,
     [-1] if less than, and [1] if greater than.  *)
  val compare_weapons : weapon -> weapon -> int 

  (**[compare_foods f1 f2] returns [0] if [f1]'s id equal to [f2]'s id,
     [-1] if less than, and [1] if greater than.  *)
  val compare_foods : food -> food -> int 

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

  (** [eat p] is [Legal p'] if there is food at the neighboring coordinates
       of player at state [p], and [Illegal] otherwise. [p'] is the updated
       state after the player eats the food. *) 
  val eat : t -> result 

  (** [retrieve_weapon p] is [Legal p'] if there is a weapon at the neighboring 
      coordinates of player at state [p], and [Illegal] otherwise. 
      [p'] is the updated state after the player retrieves the weapon. *) 
  val retrieve_weapon : t -> result

  (** [advance_level p] is [Legal p'] if the player [p] has reached the 
       experience value for that level alone, and [Illegal] otherwise.*)
  val advance_level : t -> result 

end 
