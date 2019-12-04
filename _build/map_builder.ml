(**[map_size_cal t] scales the size of the map model object [t] 
   to the size of the GUI screen and returns the dimensions of the 
   scaled map w.r.t the GUI. 
*)
let map_size_cal (t:Maps.t)=
  let (col,row)=t.size in 
  match col,row with
  |5,5->((300,200),120)
  |3,3->((375,200),150)
  |_->failwith"wrong size of map"

(**[picture_getter s] returns the image RGB matrix given by the 
   picture name [s]. 
   Fails with ["wrong name of picture, broken json or code"] if [s] is not 
   a valid picture name. *)
let picture_getter s size=
  match s,size with
  |"field",size->Color_convert.dirt_120
  |"beach",size->Color_convert.sand_120
  |"mountain",size->Color_convert.stone_120
  |"lake",size->Color_convert.water_120
  |"snow",size->Color_convert.snow_120
  |"grass",size->Color_convert.grass_120
  |_->failwith "wrong name of picture, broken json or code"

(**[map_text_build ()] draws the map on the GUI based on the map params 
   and returns [()] *)
let map_text_build ()=
  let t=Engine.get_map Engine.game_state in
  let ((rr,cr),interval)=map_size_cal t in 
  let photo_data=Array.to_list t.map_params in
  let rec draw_pic (data:((int * int) * Maps.MapParam.map_param) list) 
      rs cs inter=
    match data with 
    |((c,r),p)::t-> (let pic=picture_getter p.name p in 
                     let the_image=Graphics.make_image pic in
                     Graphics.draw_image the_image (rs+(c-1)*inter) 
                       (cs+(r-1)*inter);
                     draw_pic t rs cs inter )
    |[]->() in 
  draw_pic photo_data rr cr interval

(**[draw_player ()] draws the player on the GUI and returns [()]. *)
let draw_player () : unit =
  let t=Engine.get_map Engine.game_state in
  let ((rr,cr),interval)=map_size_cal t in 
  let player=Engine.get_player Engine.game_state in 
  match player with 
  |Player z-> let (col,row)=Player.Player.location z in 
    Graphics.set_color Graphics.red;
    Graphics.fill_circle (rr+(col-1)*interval+interval/2) 
      (cr+(row-1)*interval+(interval/2)) (interval/4)
  |Died->()

