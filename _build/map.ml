open Yojson.Basic.Util

exception UnknownFood of string
exception UnknownWeapon of string

type t = {
  size: int * int;
}

let size m = m.size

let bound_check m r c = 
  let rows = fst m.size in 
  let cols = snd m.size in 
  r < 0 || r > rows || c < 0 || c > cols
