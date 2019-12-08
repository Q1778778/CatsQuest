let purple_red=Graphics.rgb 140 67 86

let field=ref None

let beach=ref None

let mountain=ref None

let lake=ref None

let snow=ref None

let grass=ref None

let text_init()=
  field:=Some (Graphics.make_image Color_convert.dirt_120);
  beach:=Some (Graphics.make_image Color_convert.sand_120);
  mountain:=Some (Graphics.make_image Color_convert.stone_120);
  lake:=Some (Graphics.make_image Color_convert.water_120);
  snow:=Some(Graphics.make_image Color_convert.snow_120);
  grass:=Some (Graphics.make_image Color_convert.grass_120)

(**[map_size_cal t] scales the size of the map model object [t] 
   to the size of the GUI screen and returns the dimensions of the 
   scaled map w.r.t the GUI. 
*)
let map_size_cal (t:Maps.t)=
  let (col,row)=t.size in 
  match col,row with
  |5,5->((300,200),120)
  |3,3->((375,200),150)
  |5,10->((0,200),120)
  |_->failwith"wrong size of map"

(**[picture_getter s] returns the image RGB matrix given by the 
   picture name [s]. 
   Fails with ["wrong name of picture, broken json or code"] if [s] is not 
   a valid picture name. *)
let picture_getter s size=
  match s,size with
  |"field",size->Option.get !field
  |"beach",size->Option.get !beach
  |"mountain",size->Option.get !mountain
  |"lake",size->Option.get !lake
  |"snow",size->Option.get !snow
  |"grass",size->Option.get !grass
  |_->failwith "wrong name of picture, broken json or code"

(**[map_text_build ()] draws the map on the GUI based on the map params 
   and returns [()] *)
let map_text_build ()=
  let t=Engine.get_map Engine.game_state in
  let ((rr,cr),interval)=map_size_cal t in 
  let photo_data= t.map_params in
  let rec draw_pic (data:((int * int) * Maps.MapParam.map_param) list) 
      rs cs inter=
    match data with 
    |((r,c),p)::t-> (let pic=picture_getter p.name p in 
                     Graphics.draw_image pic (rs+(c-1)*inter) 
                       (cs+(r-1)*inter);
                     draw_pic t rs cs inter )
    |[]->() in 
  draw_pic photo_data rr cr interval

(**[draw_player ()] draws the player on the GUI and returns [()]. *)
let draw_player () : unit =
  let t=Engine.get_map Engine.game_state in
  let ((rr,cr),interval)=map_size_cal t in 
  let player=Engine.get_player Engine.game_state in 
  let (row,col)=Player.Player.location player in 
  Graphics.set_color Graphics.red;
  Graphics.fill_circle (rr+(col-1)*interval+interval/2) 
    (cr+(row-1)*interval+(interval/2)) (interval/4)

let draw_items ()=
  let t=Engine.get_map Engine.game_state in
  let ((rr,cr),interval)=map_size_cal t in 
  let s=Engine.game_state in 
  let weapons=Array.to_list s.all_weapons_in_current_map in 
  let foods=Array.to_list s.all_foods_in_current_map in
  let enemy=Array.to_list s.all_enemies_in_current_map in
  let draw_witem w=
    match w with 
    |Engine.Weapon w->
      let (r,c)= Weapons.Weapon.get_loc w in
      Graphics.set_color Graphics.yellow;
      Graphics.fill_circle (rr+(c-1)*interval+interval/2) 
        (cr+(r-1)*interval+(interval/2)) (interval/8);
      Graphics.set_color Graphics.black;
      let name="Weapon" in
      let pixel=(String.length name)-1 in
      Graphics.moveto ((rr+(c-1)*interval+interval/2)-pixel*3)
        ((cr+(r-1)*interval+(interval/2))-r/2);
      Graphics.draw_string name
    |Engine.Empty->() in 
  let draw_fitem =function
    |Engine.Food f->
      let (r,c)= Foods.Food.get_loc f in
      Graphics.set_color Graphics.green;
      Graphics.fill_circle (rr+(c-1)*interval+interval/2) 
        (cr+(r-1)*interval+(interval/2)) (interval/8);
      Graphics.set_color Graphics.black;
      let name="Food" in
      let pixel=(String.length name)-1 in
      Graphics.moveto ((rr+(c-1)*interval+interval/2)-pixel*3)
        ((cr+(r-1)*interval+(interval/2))-r/2);
      Graphics.draw_string name
    |Engine.Eaten->() in
  let draw_enemy =function
    |Engine.Enemy e-> (let (r,c)=Enemy.Enemy.get_pos e in 
                       Graphics.set_color purple_red;
                       Graphics.fill_circle (rr+(c-1)*interval+interval/2) 
                         (cr+(r-1)*interval+(interval/2)) (interval/6);
                       Graphics.set_color Graphics.black;
                       let name=Enemy.Enemy.get_name e in
                       let pixel=(String.length name)-1 in
                       Graphics.moveto ((rr+(c-1)*interval+interval/2)-pixel*3)
                         ((cr+(r-1)*interval+(interval/2))-r/2);
                       Graphics.draw_string name )
    |Engine.Deleted ->() in 
  List.iter draw_enemy enemy;
  List.iter draw_witem weapons;
  List.iter draw_fitem foods




