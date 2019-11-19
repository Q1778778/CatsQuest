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
  let t=Engine.main_engine_map() in
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
