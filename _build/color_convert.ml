open Graphics
open Yojson.Basic.Util

type oimage={
  name: string;
  matrix: int list list list;
}

type images ={
  one_image: oimage list;
}

type eimage={
  name_data:string;
  image_data: Graphics.color array array;
}

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

let array_color lst=
  let c_size=List.length lst in
  let array= Array.make c_size [|16777215;|] in
  list_color lst array 0

let string_tolll matrix=
  matrix|>to_list|>List.map to_list
  |>List.map (List.map to_list)|>List.map (List.map (List.map to_int))

let image_of_images i={
  name=i|>member"name"|>to_string;
  matrix=i|>member"matrix"|>string_tolll;
}

let images_of_json json=
  json|> member"images"|>to_list|>List.map image_of_images


let cat=
  images_of_json (Yojson.Basic.from_file "cute cat.json")

let player=images_of_json (Yojson.Basic.from_file "player_image.json")

let minion=images_of_json (Yojson.Basic.from_file "minion-image.json")

let player_combat=images_of_json (Yojson.Basic.from_file "player_combat.json")

let cute_cat= 
  (array_color (List.find (fun x->x.name="cute cat") cat).matrix)

let the_player= 
  (array_color (List.find (fun x->x.name="player") player).matrix)

let the_minion=(array_color (List.find (fun x->x.name="minion") minion).matrix)

let player_in_combat=
  (array_color (List.find (fun x->x.name="player_combat") player_combat).matrix)

let enemy_data=[{name_data="minion";image_data=the_minion}]
