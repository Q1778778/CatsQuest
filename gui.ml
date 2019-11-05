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
    mutable ecombat:string}

type state={
  mutable health: int;
  mutable skill: string list;
}



let cplace= {fbutton=[];ecircle=[];dialog=Bnone; irefresh=false;
             difficulty="empty";ecombat="none"}

let cstate={health=100;skill=["push"]}

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
  cplace.dialog<-Dialog_sense name

let get_player_health ()=
  match Engine.get_player(Engine.init ()) with
  |Engine.Player s-> (Player.max_health, Player.health s)
  |Engine.Died ->(Player.max_health, 0)

let health_bar ()=
  whitebox_draw 100 730 300 750 5;
  let (m,h)=get_player_health() in
  Graphics.set_color red;
  Graphics.fill_rect 100 730 (h*200/m) 20;
  Graphics.set_color black;
  Graphics.moveto 180 735;
  Graphics.draw_string ((string_of_int h)^"/"^string_of_int m);
  Graphics.moveto 40 733;
  Graphics.draw_string"Health:"

let enemy_health_bar health name=
  whitebox_draw 850 530 1050 550 5;
  Graphics.set_color red;
  Graphics.fill_rect 850 530 (health*2) 20;
  Graphics.set_color white;
  Graphics.moveto 930 535;
  Graphics.draw_string ((string_of_int health)^"/100");
  Graphics.set_color black;
  Graphics.moveto 850 515;
  Graphics.draw_string (name^" Health:")

let status_bar state=
  Graphics.set_color black;
  Graphics.set_line_width 5;
  Graphics.moveto 0 200;
  Graphics.lineto 1200 200;
  let player=Graphics.make_image Color_convert.the_player in 
  Graphics.draw_image player 10 0;
  Graphics.moveto 10 175;
  Graphics.draw_string ("The Hero Level: "^"1")



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
  let forth=create_button "forth" 
      green black 1060 10 130 85 ("second")in
  c.fbutton<-(first::second::third::[forth])

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


type trigger=
  |Command of string
  |Attack of string
  |Next_con of string
  |Tnone

let parse  c=
  match c with
  |Command d when d="easy"->cplace.difficulty<-"easy"
  |Command s-> dialog "this is a GUI tester" Color_convert.cute_cat "cute cat"
  |Attack sk->()
  |Next_con s->Graphics.clear_graph()
  |Tnone->()


let tsensor(c:clist)=
  let sta=Graphics.wait_next_event [Button_down] in 
  let sense b s=
    match b with
    |Action_button _->() 
    |Dialog_sense s->parse  (Next_con s)
    |Bnone->() 
    |Enemy _->()
    |Action_circle _->()in
  let _=sense (c.dialog) sta in ()

let fensor (c:clist) i=
  let sta=Graphics.wait_next_event [Button_down] in 
  let sense b (s:Graphics.status)=
    match b with
    |Action_button ((x,y,w,h),t)->if((x<s.mouse_x)&&((x+w)>s.mouse_x)&&
                                     (y<s.mouse_y)&&((y+h)>s.mouse_y)&&s.button)
                                    =true then (if i=Normal then 
                                                  parse  (Command t) else 
                                                  parse  (Attack t)) else ()
    |Action_circle ((x,y,r),t)->()
    |Dialog_sense _->()
    |Bnone->() 
    |Enemy (n,x,y,r) when (radius_circle s.mouse_x s.mouse_y r x y=true)->
      cplace.ecombat<-n 
    |Enemy _->()in
  let _=List.rev_map (fun butt->sense butt sta) c.fbutton in 
  let _=List.rev_map (fun butt->sense butt sta) c.ecircle in ()


let clear_screen ()=
  if cplace.irefresh=true then Graphics.clear_graph() else ()

let rec find_enemy_data name (lst:Color_convert.eimage list)=
  match lst with
  |h::t when h.name_data=name->h.image_data
  |h::t->find_enemy_data name t 
  |[]->failwith "can not find the image"

let rec combat name =
  status_bar cstate;
  normal_four_botton cplace;
  health_bar ();
  combat_four_botton cplace;
  let image_of_e=find_enemy_data name Color_convert.enemy_data in 
  draw_a_image image_of_e 900 550;
  enemy_health_bar 100 name;
  draw_a_image Color_convert.player_in_combat 10 205;
  fensor cplace Combat;
  combat name 

let combat_mon()=if cplace.ecombat<>"none" then 
    (Graphics.clear_graph();combat cplace.ecombat)
  else ()

let ms1_demo()=
  draw_enemy "minion" 600 400 20 red 

let rec init () =
  cplace.fbutton<-[];
  cplace.dialog<-Bnone;
  cplace.irefresh<-false;
  status_bar cstate;
  normal_four_botton cplace;
  health_bar ();
  ms1_demo();
  fensor cplace Normal;
  combat_mon();
  tsensor cplace;
  clear_screen();
  init ()

let rec beginning () =
  Graphics.moveto 500 650;
  Graphics.draw_string "Welcome to the game";
  Graphics.moveto 500 620;
  Graphics.draw_string "please select difficulty to begin:";
  cplace.fbutton<-[create_button "easy" green black 500 550 200 50 "easy"];
  fensor cplace Normal;
  if cplace.difficulty<>"empty" then 
    (Graphics.clear_graph(); init ()) else beginning ()

let open_g state =
  Graphics.open_graph " 1200x800+100";
  beginning ()

