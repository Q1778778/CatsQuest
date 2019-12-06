open Graphics
open Enemy
open Player

type stage=
  |Combat
  |Normal

type box=
  | Action_button of (int*int*int*int)*string
  | Action_circle of (int*int*int)*string
  | Dialog_sense of string
  | Enemy of (string*int*int*int)
  | Bnone

type clist=
  { mutable fbutton : box list;
    mutable ecircle : box list;
    mutable dialog: box;
    mutable irefresh: bool;
    mutable difficulty: string;
    mutable enemy_to_combat:string;
    mutable dialog_in_progress:bool}

type state={
  mutable skill: string list;
}

exception Not_such_enemy of string

let cplace= {fbutton=[];ecircle=[];dialog=Bnone; irefresh=false;
             difficulty="empty";enemy_to_combat="none";dialog_in_progress=false}

let cstate={skill=["stab"]}

let lblue=Graphics.rgb 82 219 255

let grey=Graphics.rgb 192 192 192

let whitebox_draw a b c d width=
  Graphics.set_line_width width;
  Graphics.moveto a b;
  Graphics.lineto c b;
  Graphics.lineto c d;
  Graphics.lineto a d;
  Graphics.lineto a b

let dialog text npc name=
  Graphics.set_color white;
  Graphics.fill_rect 150 100 900 200;
  Graphics.set_color black;
  whitebox_draw 150 100 1050 300 4;
  let pnpc=Graphics.make_image npc in
  Graphics.draw_image pnpc 150 304;
  Graphics.moveto 175 275;
  Graphics.draw_string (name^":");
  Graphics.moveto 200 260;
  Graphics.draw_string text;
  Graphics.moveto 920 120;
  Graphics.draw_string "Click to continue #";
  cplace.dialog<-Dialog_sense name

let get_player_health ()=
  let s= Engine.get_player(Engine.game_state) in
  (Player.max_health, Player.health s)


let get_player_level ()=
  let s= Engine.get_player(Engine.game_state) in
  (Player.level s)


let get_player_expeience ()=
  let s= Engine.get_player(Engine.game_state) in
  (Player.experience s)


let player_reduce_health int=
  let s= Engine.get_player(Engine.game_state) in
  (Player.reduce_health s int)

let experience_bar ()=
  Graphics.set_color black;
  whitebox_draw 130 20 150 190 5;
  Graphics.moveto 115 6;
  Graphics.draw_string "Experience";
  let upper_bound=(get_player_level()*100)in
  Graphics.set_color green;
  Graphics.fill_rect 130 20 20 (170*(get_player_expeience())/upper_bound);
  ()

let health_bar ()=
  whitebox_draw 100 730 300 750 5;
  Graphics.set_color white;
  Graphics.fill_rect 100 730 200 20;
  let (m,h)=get_player_health() in
  Graphics.set_color red;
  let hp=if h*200/m>=0 then h*200/m else 0 in
  Graphics.fill_rect 100 730 (hp) 20;
  Graphics.set_color black;
  Graphics.moveto 180 735;
  Graphics.draw_string ((string_of_int h)^"/"^string_of_int m);
  Graphics.moveto 40 733;
  Graphics.draw_string"Health:"

let enemy_health_bar enemy=
  let health=Enemy.get_hp enemy in
  let max_hp= Enemy.get_max_hp enemy in
  whitebox_draw 850 530 1050 550 5;
  Graphics.set_color red;
  let hp=if health*200/max_hp>=0 then health*200/max_hp else 0 in
  Graphics.fill_rect 850 530 hp 20;
  Graphics.set_color white;
  Graphics.moveto 930 535;
  Graphics.set_color black;
  Graphics.draw_string ((string_of_int health)^"/"^(string_of_int max_hp));
  Graphics.set_color black;
  Graphics.moveto 850 515;
  Graphics.draw_string (Enemy.get_name enemy^" Health:")

let status_bar ()=
  Graphics.set_color black;
  Graphics.set_line_width 5;
  Graphics.moveto 0 200;
  Graphics.lineto 1200 200;
  let player=Graphics.make_image Color_convert.the_player in 
  Graphics.draw_image player 10 0;
  Graphics.moveto 10 175;
  Graphics.draw_string ("The Hero Level: "^string_of_int (get_player_level()))



let draw_a_image image x y=
  let p=Graphics.make_image image in
  Graphics.draw_image p x y

(** rquire: [text] is a non-empty string*)
let create_button text color tcolor x y w h trigger=
  Graphics.set_color color;
  Graphics.fill_rect x y w h;
  let pixel=(String.length text)-1 in
  (Graphics.moveto (x+w/2-pixel*6) (y+h/2);
   Graphics.set_color tcolor;
   Graphics.draw_string text);
  Action_button ((x,y,w,h),trigger)

let normal_four_botton c=
  let first=create_button  "cat" red black 920 105 130 85 ("first") in
  let second=create_button "second" 
      magenta black 1060 105 130 85 ("second")in
  let third=create_button "third" blue black 920 10 130 85("second") in
  let fourth=create_button "fourth" 
      green black 1060 10 130 85 ("second")in
  c.fbutton<-(first::second::third::[fourth])

let combat_four_botton c =
  let fskill=(List.nth_opt (cstate.skill) 0) in
  let sskill=(List.nth_opt (cstate.skill) 1) in
  let tskill=(List.nth_opt (cstate.skill) 2) in
  let forskill=(List.nth_opt (cstate.skill) 3) in
  if Option.is_some forskill then
    (let first=create_button (Option.get fskill)
         lblue black 920 105 130 85 ((Option.get fskill)) in
     let second=create_button (Option.get fskill)
         lblue black 1060 105 130 85 ((Option.get fskill))in
     let third=create_button (Option.get fskill) 
         lblue black 920 10 130 85((Option.get fskill)) in
     let forth=create_button (Option.get fskill) 
         lblue black 1060 10 130 85 ((Option.get fskill))in
     c.fbutton<-(first::second::third::[forth])) else if 
    Option.is_some tskill then 
    (let first=create_button (Option.get fskill)
         lblue black 920 105 130 85 ((Option.get fskill)) in
     let second=create_button (Option.get fskill)
         lblue black 1060 105 130 85 ((Option.get fskill))in
     let third=create_button (Option.get fskill) 
         lblue black 920 10 130 85((Option.get fskill)) in
     let _=create_button "None" 
         grey black 1060 10 130 85 "None"in
     c.fbutton<-(first::second::[third])) else if  Option.is_some sskill then
    (let first=create_button (Option.get fskill)
         lblue black 920 105 130 85 ((Option.get fskill)) in
     let second=create_button (Option.get fskill)
         lblue black 1060 105 130 85 ((Option.get fskill))in
     let _=create_button "None"
         grey black 920 10 130 85("None") in
     let _=create_button "None" 
         grey black 1060 10 130 85 "None"in
     c.fbutton<-(first::[second])) else
    (let first=create_button (Option.get fskill)
         lblue black 920 105 130 85 ((Option.get fskill)) in
     let _=create_button "None"
         grey black 1060 105 130 85 "None"in
     let _=create_button "None"
         grey black 920 10 130 85("None") in
     let _=create_button "None" 
         grey black 1060 10 130 85 "None"in
     c.fbutton<-([first]))

let rec find_enemy_image_data name (lst:Color_convert.eimage list)=
  match lst with
  |h::t when h.name_data=name->h.image_data
  |h::t->find_enemy_image_data name t 
  |[]->failwith "can not find the image"

let draw_enemy name x y r color=
  Graphics.set_color color;
  Graphics.fill_circle x y r;
  let pixel=(String.length name)-1 in
  Graphics.moveto (x-pixel*3) (y-r/2);
  Graphics.set_color black;
  Graphics.draw_string name;
  cplace.ecircle<-Enemy (name,x,y,r)::(cplace.ecircle)


let radius_circle x y r rx ry=
  let dx=x-rx in 
  let dy=y-ry in 
  let distance=sqrt(float_of_int(dx*dx+dy*dy))in 
  float_of_int r>=distance

let enemy_list()=Array.to_list(Engine.get_enemies Engine.game_state)

let rec get_one_enemy name lst=
  match lst with
  |Engine.Enemy s::t when ((Enemy.get_name s)=name)->s
  |h::t -> get_one_enemy name t
  |[]->raise (Not_such_enemy name)

let skill_damage name=
  let s= Engine.get_player(Engine.game_state) in
  name|> Player.get_skill_by_skill_name s
  |> Player.extract_skill_strength_single_skill

let enemy_skill_image name=
  match name with 
  |"punch"->draw_a_image Color_convert.enemy_punch 500 350
  |"scratch"->draw_a_image Color_convert.enemy_scar 500 350
  |_->failwith"unbound image"

let enemy_skill t=
  let (name,damage)=Engine.choose_skill_random t in 
  player_reduce_health damage;
  health_bar();
  enemy_skill_image name;
  Graphics.set_color black;
  Graphics.moveto 100 715;
  Graphics.draw_string ("-"^string_of_int damage)

let skill_helper name=
  Graphics.moveto 850 500;
  Graphics.set_color red;
  Graphics.draw_string ("-"^string_of_int(skill_damage name));
  Enemy.reduce_hp (get_one_enemy cplace.enemy_to_combat (enemy_list ()))
    (skill_damage name);
  Thread.delay 1.5;
  Graphics.clear_graph(); status_bar ();
  normal_four_botton cplace;
  health_bar ();
  combat_four_botton cplace;
  let the_enemy=get_one_enemy cplace.enemy_to_combat (enemy_list()) in
  let image_of_e=find_enemy_image_data cplace.enemy_to_combat
      Color_convert.enemy_data in 
  draw_a_image image_of_e 900 550;
  enemy_health_bar the_enemy;
  let the_enemy=get_one_enemy cplace.enemy_to_combat (enemy_list())in
  enemy_skill the_enemy;
  draw_a_image Color_convert.player_in_combat 10 205

let skill_image name=
  match name with 
  |"punch"->draw_a_image Color_convert.the_stab 500 350;
    skill_helper name
  |_->failwith"unbound image"

type trigger=
  |Command of string
  |Attack of string
  |Next_con of string
  |Tnone

let parse  c=
  match c with
  |Command d when d="easy"->cplace.difficulty<-"easy"
  |Command s-> cplace.dialog_in_progress<-true;dialog "this is a GUI tester"
      Color_convert.cute_cat "cute cat"
  |Attack sk->skill_image sk;
  |Next_con s->cplace.dialog_in_progress<-false;Graphics.clear_graph()
  |Tnone->()


let tsensor(c:clist)=
  if not cplace.dialog_in_progress then () else
    (let sta=Graphics.wait_next_event [Button_down] in 
     let sense b s=
       match b with
       |Action_button _->() 
       |Dialog_sense s->parse  (Next_con s)
       |Bnone->() 
       |Enemy _->()
       |Action_circle _->()in
     let _=sense (c.dialog) sta in ())

let ksensor sta=
  let key=sta.key in 
  match key with 
  |'a'
  |'A'->Engine.move_player_left Engine.game_state; cplace.irefresh<-true
  |'d'
  |'D'->Engine.move_player_right Engine.game_state; cplace.irefresh<-true
  |'w'
  |'W'->Engine.move_player_up Engine.game_state; cplace.irefresh<-true
  |'s'
  |'S'->Engine.move_player_down Engine.game_state; cplace.irefresh<-true
  |_->()

let rec fensor (c:clist) i=
  let sta=Graphics.wait_next_event [Button_down;Key_pressed] in 
  let sense b (s:Graphics.status)=
    match b with
    |Action_button ((x,y,w,h),t)->if((x<s.mouse_x)&&((x+w)>s.mouse_x)&&
                                     (y<s.mouse_y)&&((y+h)>s.mouse_y)&&s.button)
                                    =true then (if i=Normal then 
                                                  parse  (Command t) else 
                                                  parse  (Attack t)) else ()
    |Action_circle ((x,y,r),t)->()
    |Dialog_sense s->()
    |Bnone->fensor c i
    |Enemy (n,x,y,r) when (radius_circle s.mouse_x s.mouse_y r x y=true)->
      cplace.enemy_to_combat<-n 
    |Enemy _->()in
  if sta.button then (let _=List.rev_map (fun butt->sense butt sta) c.fbutton in 
                      let _=List.rev_map 
                          (fun butt->sense butt sta) c.ecircle in ()) else 
    ksensor sta


let clear_screen ()=
  if cplace.irefresh=true then Graphics.clear_graph() else ()


let skill_mon ()=
  match Engine.game_state.player with
  |Player s->
    cstate.skill<-List.map (fun x->Player.skill_name x) (Player.skills_list s)
  |Died->()

let enemy_mon name=
  let hp=Enemy.get_hp (get_one_enemy name (enemy_list()))in 
  if hp<=0 then false else true 

let game_over_mon ()=
  let (m,h)=get_player_health()in
  if h<=0 then (  health_bar ();
                  dialog "Game over" Color_convert.cute_cat "cute cat"; 
                  tsensor cplace; Graphics.close_graph()) else ()

let rec combat name =
  status_bar ();
  normal_four_botton cplace;
  health_bar ();
  combat_four_botton cplace;
  let the_enemy=get_one_enemy name (enemy_list()) in
  let image_of_e=find_enemy_image_data name Color_convert.enemy_data in 
  draw_a_image image_of_e 900 550;
  enemy_health_bar the_enemy;
  draw_a_image Color_convert.player_in_combat 10 205;
  fensor cplace Combat;
  Thread.delay 1.0;
  game_over_mon();
  Graphics.clear_graph();
  if enemy_mon name then
    (combat name) else 
    cplace.enemy_to_combat<-"none";cplace.ecircle<-[];()

let combat_mon()=if cplace.enemy_to_combat<>"none" then 
    (Graphics.clear_graph();combat cplace.enemy_to_combat)
  else ()

let ms1_demo flag=
  if flag then
    draw_enemy "minion" 600 400 20 red else ()

let rec init flag =
  cplace.fbutton<-[];
  cplace.dialog<-Bnone;
  cplace.irefresh<-false;
  Map_builder.map_text_build();
  Map_builder.draw_player();
  status_bar ();
  experience_bar();
  normal_four_botton cplace;
  health_bar ();
  skill_mon();
  ms1_demo flag;
  fensor cplace Normal;
  tsensor cplace;
  combat_mon();
  clear_screen();
  if cplace.ecircle=[] then 
    init false else init true

let rec beginning () =
  Graphics.moveto 500 650;
  Graphics.draw_string "Welcome to the game";
  Graphics.moveto 500 620;
  Graphics.draw_string "please select difficulty to begin:";
  cplace.fbutton<-[create_button "easy" green black 500 550 200 50 "easy"];
  fensor cplace Normal;
  if cplace.difficulty<>"empty" then 
    (Graphics.clear_graph(); init true )else beginning ()

let open_g () =
  Graphics.open_graph " 1200x800+100";
  beginning ()

let ()=open_g ()
