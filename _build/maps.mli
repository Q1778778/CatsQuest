(*(**[size m] is the size of the map [m] in [(rows, columns)]. *)
  val size : t -> int * int

  (**[bound_check m r c] checks whether row [r] col [c] is within the 
  boundary of the map [m]. *)
  val bound_check : t -> int -> int -> bool*)

module type MP = sig 
  (** The abstract type of values representing map params. *)
  type map_param = {
    name: string; (*this represents a jpg name for this element*)
    link: string; (*link here represents another map file 
                    that current map element is linked to. If there is NO such
                    linked map, link will be empty string ""*)
  }

  (** Constructor of a map param *)
  val single_map_element_constructor : 
    name: string -> 
    link: string ->
    map_param
end

module MapParam : MP 

(** The abstract type of values representing maps. *)
type t = {
  size : int * int; 
  name : string; 
  map_params: ((int * int) * MapParam.map_param) array;
}

val map_constructor: 
  size: int * int -> 
  name: string -> 
  all_map_param: ((int * int) * MapParam.map_param) array ->
  t


val bound_check: t -> int -> int -> bool

