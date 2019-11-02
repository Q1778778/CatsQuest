(** The abstract type of values representing maps. *)
type t 

(** Raised when an unknown food is encountered. *)
exception UnknownFood of string

(** Raised when an unknown weapon is encountered. *)
exception UnknownWeapon of string

(**[size m] is the size of the map [m]. *)
val size : t -> float * float

(**[weapon_ids m] is the list of weapon id #s in the map [m]. *)
val weapon_ids : t -> int list

(**[food_ids m] is the list of food id #s in the map [m]. *)
val food_ids : t -> int list

(**[bound_check m r c] checks whether row [r] col [c] is within the 
   boundary of the map [m]. *)
val bound_check : t -> int -> int -> bool