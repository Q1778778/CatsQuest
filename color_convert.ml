open Graphics
open Yojson.Basic.Util

(** The abstract type of values representing an image with image json data*)
type oimage={
  name: string;
  matrix: int list list list;
}

(** The abstract type of values representing multiple images *)
type images ={
  one_image: oimage list;
}

(** The abstract type of values representing an image with RGB data*)
type eimage={
  name_data:string;
  image_data: Graphics.color array array;
}

(**[list_color l acc i] recursively converts each element of [l] into 
   an rgb matrix starting with an accumulator [acc] and index [i]. *)
let rec list_color (lst:int list list list) acc int2=
  match lst with 
  |h::t->
    let r_size=List.length h in
    let init=(match h with
        |[r;g;b]::t->Array.make r_size (Graphics.rgb r g b)
        |h::t->failwith"not a three items list"
        |[]->failwith"should not reach the end")in
    let rec row_color (h:int list list) arr flag int: color array=
      match h with
      |[r;g;b]::t->if flag then 
          let _=(arr.(int)<-(Graphics.rgb r g b)) in
          (row_color t arr true (int+1)) else 
          row_color t arr true (int+1)
      |h::t->failwith"broken code"
      |[]->arr in
    acc.(int2)<-(row_color h init false 0);
    (list_color t acc (int2+1))
  |[]->acc

(**[array_color l] converts the image json data [l] into an rgb matrix [m], 
   and returns [m].  *)
let array_color lst=
  let c_size=List.length lst in
  let array= Array.make c_size [|16777215;|] in
  list_color lst array 0

(**[string_tolll m] is [m] in a 3d list representation.  *)
let string_tolll matrix=
  matrix|>to_list|>List.map to_list
  |>List.map (List.map to_list)|>List.map (List.map (List.map to_int))

(**[image_of_images i] is the image that [i] represents
   Requires: [i] is a valid JSON image representation. *)
let image_of_images i={
  name=i|>member"name"|>to_string;
  matrix=i|>member"matrix"|>string_tolll;
}

(**[images_of_json j] is the image list that [j] represents
   Requires: [j] is a valid JSON image list representation. *)
let images_of_json json=
  json|> member"images"|>to_list|>List.map image_of_images

(**[enemy_stab] is the image data extracted from ["punch_enemy.json"]  *)
let enemy_stab=images_of_json (Yojson.Basic.from_file "punch_enemy.json")

(**[enemy_s] is the image data extracted from ["scar.json"]  *)
let enemy_s=images_of_json (Yojson.Basic.from_file "scar.json")

(**[cat] is the image data extracted from ["cute cat.json"]  *)
let cat=
  images_of_json (Yojson.Basic.from_file "cute cat.json")

(**[player] is the image data extracted from ["player_image.json"]  *)
let player=images_of_json (Yojson.Basic.from_file "player_image.json")

(**[minion] is the image data extracted from ["minion-image.json"]  *)
let minion=images_of_json (Yojson.Basic.from_file "minion-image.json")

(**[player_combat] is the image data extracted from ["player_combat.json"]  *)
let player_combat=images_of_json (Yojson.Basic.from_file "player_combat.json")

(**[stab] is the image data extracted from ["stab.json"]  *)
let stab=images_of_json (Yojson.Basic.from_file "stab.json")

(**[sand5] is the image data extracted from ["sand-120.json"]  *)
let sand5=images_of_json (Yojson.Basic.from_file "sand-120.json")

(**[grass5] is the image data extracted from ["grass-120.json"]  *)
let grass5=images_of_json (Yojson.Basic.from_file "grass-120.json")

(**[stone5] is the image data extracted from ["stone-120.json"]  *)
let stone5=images_of_json (Yojson.Basic.from_file "stone-120.json")

(**[snow5] is the image data extracted from ["snow-120.json"]  *)
let snow5=images_of_json (Yojson.Basic.from_file "snow-120.json")

(**[water5] is the image data extracted from ["water-120.json"]  *)
let water5=images_of_json (Yojson.Basic.from_file "water-120.json")

(**[dirt5] is the image data extracted from ["dirt-120.json"]  *)
let dirt5=images_of_json (Yojson.Basic.from_file "dirt-120.json")

(**[enemy_punch] is the RGB color 2d matrix represented by the image 
   ["stab"] *)
let enemy_punch=(array_color (List.find (fun x->x.name="stab") enemy_stab)
                             .matrix)

(**[enemy_scar] is the RGB color 2d matrix represented by the image 
   ["scar-200"] *)
let enemy_scar=(array_color (List.find (fun x->x.name="scar-200") enemy_s)
                            .matrix)

(**[cute_cat] is the RGB color 2d matrix represented by the image 
   ["cute cat"] *)
let cute_cat= 
  (array_color (List.find (fun x->x.name="cute cat") cat).matrix)

(**[the_player] is the RGB color 2d matrix represented by the image 
   ["player"] *)
let the_player= 
  (array_color (List.find (fun x->x.name="player") player).matrix)

(**[the_minion] is the RGB color 2d matrix represented by the image 
   ["minion"] *)
let the_minion=(array_color (List.find (fun x->x.name="minion") minion)
                            .matrix)

(**[player_in_combat] is the RGB color 2d matrix represented by the image 
   ["player_combat"] *)
let player_in_combat=
  (array_color (List.find (fun x->x.name="player_combat") player_combat)
               .matrix)

(**[the_stab] is the RGB color 2d matrix represented by the image 
   ["stab"] *)
let the_stab=
  (array_color (List.find (fun x->x.name="stab") stab).matrix)

(**[sand_120] is the RGB color 2d matrix represented by the image 
   ["sand-120"] *)
let sand_120=
  (array_color (List.find (fun x->x.name="sand-120") sand5).matrix)

(**[grass_120] is the RGB color 2d matrix represented by the image 
   ["grass-120"] *)
let grass_120=(array_color (List.find (fun x->x.name="grass-120") grass5)
                           .matrix)

(**[stone_120] is the RGB color 2d matrix represented by the image 
   ["stone-120"] *)
let stone_120=(array_color (List.find (fun x->x.name="stone-120") stone5)
                           .matrix)

(**[snow_120] is the RGB color 2d matrix represented by the image 
   ["snow-120"] *)
let snow_120=(array_color (List.find (fun x->x.name="snow-120") snow5)
                          .matrix)

(**[water_120] is the RGB color 2d matrix represented by the image 
   ["water-120"] *)
let water_120=(array_color (List.find (fun x->x.name="water-120") water5)
                           .matrix)

(**[dirt_120] is the RGB color 2d matrix represented by the image 
   ["dirt-120"] *)
let dirt_120=(array_color (List.find (fun x->x.name="dirt-120") dirt5).matrix)

(**[enemy_data] is the list that contains a single [eimage] with the minion 
   image.  *)
let enemy_data=[{name_data="minion";image_data=the_minion}]
