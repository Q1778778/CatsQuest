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

  val get_name: map_param -> string

  val get_link: map_param -> string

  (* the name and link of a map param should be IMMUTABLE. 
  There is NO setter method for this module *)
end


module MapParam : MP = struct 
  type map_param = {
    name: string; (*this represents a jpg name for this element*)
    link: string; (*link here represents another map file 
                    that current map element is linked to. If there is NO such
                    linked map, link will be empty string ""*)
  }

  let single_map_element_constructor ~name ~link = {
    link = link;
    name = name;
  }

  let get_link param = param.link

  let get_name param = param.name

end

type t = {
  size : int * int; (*total col * total rows *)
  name : string; (*this map name*)
  (**[|((col, row), map_param)|]*)
  map_params: ((int * int) * MapParam.map_param) array; 
}

let map_constructor 
    ~size ~name ~all_map_param = {
  size = size; (*total col * total rows *)
  name = name; (*this map name *)
  map_params = all_map_param; 
}

let size m = m.size

let bound_check m c r = 
  let cols = fst m.size in 
  let rows = snd m.size in 
  c > 0 && c <= cols && r > 0 && r <= rows