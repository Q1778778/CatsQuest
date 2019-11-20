let map_size_cal (t:Maps.t)=
  let (col,row)=t.size in 
  match col,row with
  |5,5->((300,200),120)
  |_->failwith"wrong size of map"

let picture_getter s=
  match s with
  |"field"->Color_convert.dirt_120
  |"beach"->Color_convert.sand_120
  |"mountain"->Color_convert.stone_120
  |"lake"->Color_convert.water_120
  |"snow"->Color_convert.snow_120
  |"grass"->Color_convert.grass_120
  |_->failwith "wrong name of picture, broken json or code"

let map_text_build ()=
  let t=Engine.get_map Engine.game_state in
  let ((rr,cr),interval)=map_size_cal t in 
  let photo_data=Array.to_list t.map_params in
  let rec draw_pic (data:((int * int) * Maps.MapParam.map_param) list) rs cs inter=
    match data with 
    |((c,r),p)::t-> (let pic=picture_getter p.name in 
                     let the_image=Graphics.make_image pic in
                     Graphics.draw_image the_image (rs+(c-1)*inter) (cs+(r-1)*inter);
                     draw_pic t rs cs inter )
    |[]->() in 
  draw_pic photo_data rr cr interval

let draw_player () : unit =
  let t=Engine.get_map Engine.game_state in
  let ((rr,cr),interval)=map_size_cal t in 
  let player=Engine.get_player Engine.game_state in 
  match player with 
  |Player z-> let (col,row)=Player.Player.location z in 
    Graphics.set_color Graphics.red;
    Graphics.fill_circle (rr+(col-1)*interval+interval/2) (cr+(row-1)*interval+(interval/2)) (interval/4)
  |Died->()

