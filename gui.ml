open Graphics

type stage=
  |Combat
  |Normal

type box=
  | Action_button of (int*int*int*int)*string
  | Dialog_sense of string
  | Bnone
  | Refresh

type clist=
  { mutable fbutton : box list;
    mutable dialog: box;
    mutable irefresh: bool}

type state={
  mutable health: int;
}

let cplace= {fbutton=[];dialog=Bnone; irefresh=false}
let cstate={health=100}



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

let health_bar health=
  whitebox_draw 100 730 300 750 5;
  Graphics.set_color red;
  Graphics.fill_rect 100 730 (health*2) 20;
  Graphics.set_color white;
  Graphics.moveto 180 735;
  Graphics.draw_string ((string_of_int health)^"/100");
  Graphics.set_color black;
  Graphics.moveto 40 733;
  Graphics.draw_string"Health:"

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

let rec four_botton c=
  let first=create_button "cat" red black 920 105 130 85 ("first") in
  let second=create_button "second" 
      magenta black 1060 105 130 85 ("second")in
  let third=create_button "third" blue black 920 10 130 85("second") in
  let forth=create_button "forth" 
      green black 1060 10 130 85 ("second")in
  c.fbutton<-(first::second::third::[forth])


type trigger=
  |Command of string
  |Next_con of string
  |Tnone

let parse  c=
  match c with
  |Command s-> dialog "this is a GUI tester" Color_convert.cute_cat "cute cat"
  |Next_con s->Graphics.clear_graph()
  |Tnone->()


let tsensor(c:clist)=
  let sta=Graphics.wait_next_event [Button_down] in 
  let sense b s=
    match b with
    |Action_button _->() 
    |Dialog_sense s->parse  (Next_con s)
    |Bnone->() in
  let _=sense (c.dialog) sta in ()

let fensor (c:clist)=
  let sta=Graphics.wait_next_event [Button_down] in 
  let sense b (s:Graphics.status)=
    match b with
    |Action_button ((x,y,w,h),t)->if((x<s.mouse_x)&&((x+w)>s.mouse_x)&&
                                     (y<s.mouse_y)&&((y+h)>s.mouse_y)&&s.button)
                                    =true then parse  (Command t) else ()
    |Dialog_sense _->()
    |Bnone->() in
  let _=List.rev_map (fun butt->sense butt sta) c.fbutton in ()

let clear_screen=
  if cplace.irefresh=true then Graphics.clear_graph() else ()

let rec init state =
  cplace.fbutton<-[];
  cplace.dialog<-Bnone;
  cplace.irefresh<-false;
  status_bar state;
  four_botton cplace;
  health_bar 100;
  fensor cplace;
  tsensor cplace;
  clear_screen;
  init state

let open_g state =
  Graphics.open_graph " 1200x800+100";
  init state

