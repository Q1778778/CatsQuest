(** The abstract type of values representing maps. *)
type t 

(** Raised when an unknown food is encountered. *)
exception UnknownFood of string

(** Raised when an unknown weapon is encountered. *)
exception UnknownWeapon of string

(**[size m] is the size of the map [m]. *)
val size : t -> int * int

(**[bound_check m r c] checks whether row [r] col [c] is within the 
   boundary of the map [m]. *)
val bound_check : t -> int -> int -> bool