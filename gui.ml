open Graphics
open Enemy
open Player


type stage=
  |Combat
  |Normal

type box=
  | Action_button of (int*int*int*int)*(string*string)
  | Action_box of (int*int*int*int)*(string*int)
  | Dialog_sense of string
  | Bnone

type trigger=
    Guide of string
  |Command of string
  |Attack of string
  |Next_con of string
  |Item of string*int
  |Order of string*string
  |Tnone

type clist=
  { mutable fbutton : box list;
    mutable dialog: box;
    mutable irefresh: bool;
    mutable difficulty: string;
    mutable enemy_to_combat:string;
    mutable dialog_in_progress:bool;
    mutable item_selected: (string*int*string) option;
    mutable item_ground: bool;
    mutable message_display: string;
    mutable skills: string list;
    mutable player_level: int;
  }

type cd_skill={
  mutable fire: int;
  mutable trial: int;
  mutable punishment: int;
}

exception Not_such_enemy of string

let cplace= {fbutton=[];dialog=Bnone; irefresh=false;
             difficulty="empty";enemy_to_combat="none";dialog_in_progress=false;
             item_selected=None;item_ground=false;
             message_display="welcome to the game";
             skills=[];player_level=1}

let cd_store={fire= -1;trial= -1;punishment= -1}


let lblue=Graphics.rgb 82 219 255

let grey=Graphics.rgb 192 192 192

(** [whitebox_draw a b c d width] draws a whitebox with lowerleft point at [a,b]
    and upperright point at [c,d] with linewidth [width]
    Require: [a b c d width] are non-negative ints*)
let whitebox_draw a b c d width=
  Graphics.set_line_width width;
  Graphics.moveto a b;
  Graphics.lineto c b;
  Graphics.lineto c d;
  Graphics.lineto a d;
  Graphics.lineto a b

let text_draw_dialog text=
  if String.length text<120 then
    (Graphics.moveto 200 260;
     Graphics.draw_string text)
  else if String.length text<240 then
    (let text1=String.sub text 0 120 in 
     let text2=String.sub text 120 (String.length text-120) in 
     Graphics.moveto 200 260;
     Graphics.draw_string text1;
     Graphics.moveto 200 245;
     Graphics.draw_string text2)
  else (let text1=String.sub text 0 120 in 
        let text2=String.sub text 120 120 in 
        let text3=String.sub text 240  (String.length text-240) in
        Graphics.moveto 200 260;
        Graphics.draw_string text1;
        Graphics.moveto 200 245;
        Graphics.draw_string text2;
        Graphics.moveto 200 230;
        Graphics.draw_string text3)

(** [dialog text npc name] draws a dialog box, showing [text] on screen with 
    picture [npc] shown at upper-left corner with [name]
    Requires: [text] and [name] are string,
    [npc] is a color matrix*)
let dialog text npc name=
  Graphics.set_color white;
  Graphics.fill_rect 150 100 900 200;
  Graphics.set_color black;
  whitebox_draw 150 100 1050 300 4;
  let pic_npc=Graphics.make_image npc in
  Graphics.draw_image pic_npc 150 304;
  Graphics.moveto 175 275;
  Graphics.draw_string (name^":");
  text_draw_dialog text;
  Graphics.moveto 920 120;
  Graphics.draw_string "Click to continue #";
  cplace.dialog<-Dialog_sense name

(** [get_player_health ()] is the max health of the player
    with the current health*)
let get_player_health ()=
  let s= Engine.get_player(Engine.game_state) in
  (Player.max_health s, Player.health s)


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
  let upper_bound=(30+get_player_level()*30)in
  Graphics.set_color green;
  Graphics.fill_rect 130 20 20 (170*(get_player_expeience())/upper_bound);
  ()

let health_bar ()=
  Graphics.set_color black;
  whitebox_draw 100 730 300 750 5;
  Graphics.set_color white;
  Graphics.fill_rect 100 730 200 20;
  let (max,health)=get_player_health() in
  Graphics.set_color red;
  let hp=if health*200/max>=0 then health*200/max else 0 in
  Graphics.fill_rect 100 730 (hp) 20;
  Graphics.set_color black;
  Graphics.moveto 180 735;
  Graphics.draw_string ((string_of_int health)^"/"^string_of_int max);
  Graphics.moveto 120 735;
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

let box_drawing_helper ()=
  Graphics.set_color black;
  Graphics.moveto 200 50;
  Graphics.draw_string "weapons:";
  whitebox_draw 260 20 320 80 3;
  whitebox_draw 360 20 420 80 3;
  whitebox_draw 460 20 520 80 3;
  Graphics.moveto 200 150;
  Graphics.draw_string "foods:";
  whitebox_draw 260 120 320 180 3;
  whitebox_draw 360 120 420 180 3;
  Action_box ((260,20,60,60),("weapon",0))::
  Action_box ((360,20,60,60),("weapon",1))::
  Action_box ((460,20,60,60),("weapon",2))::
  Action_box ((260,120,60,60),("food",0))::
  Action_box ((360,120,60,60),("food",1))::
  Action_box ((460,120,60,60),("food",2))::
  cplace.fbutton

let draw_inventory ()=
  whitebox_draw 460 120 520 180 3;
  let fb=box_drawing_helper() in 
  if Option.is_some cplace.item_selected then 
    let (t,i,c)=Option.get cplace.item_selected in 
    match t with 
    |"food"-> Graphics.set_color red; 
      whitebox_draw (260+(i*100)) 120 (320+(i*100)) 180 3
    |"weapon"-> Graphics.set_color red; 
      whitebox_draw (260+(i*100)) 20 (320+(i*100)) 80 3
    |_->() else ();
  cplace.fbutton<- fb


let down_row_info int string=
  Graphics.moveto (545-int*10) 175;
  Graphics.draw_string string

let info_bar()=
  whitebox_draw 540 10 900 190 3;
  Graphics.moveto 545 120;
  Graphics.draw_string cplace.message_display


let draw_a_image image x y=
  let p=Graphics.make_image image in
  Graphics.draw_image p x y

let string_cal text tcolor x y w h=
  let pixel=(String.length text)-1 in
  (Graphics.moveto (x+w/2-pixel*3) (y+h/2);
   Graphics.set_color tcolor;
   Graphics.draw_string text)

(** require: [text] is a non-empty string*)
let create_button text color tcolor x y w h trigger=
  Graphics.set_color color;
  Graphics.fill_rect x y w h;
  string_cal text tcolor x y w h;
  Action_button ((x,y,w,h),trigger)

let normal_four_botton c=
  let first=create_button "Guide cat" red black
      920 105 130 85 ("dialog","first") in
  let second=if Option.is_some cplace.item_selected then
      (let (t,i,n)=Option.get cplace.item_selected in
       create_button "use" 
         magenta black 1060 105 130 85 ("use",n)) else 
      (create_button "use" 
         grey white 1060 105 130 85 ("","second")) in
  let third=if (cplace.item_ground)then 
      create_button "pick up" blue black 920 10 130 85("weapon","pick") else
      create_button "pick up" grey white 920 10 130 85("weapon","pick") in
  let fourth=if (Option.is_some cplace.item_selected)then create_button "drop" 
        green black 1060 10 130 85 ("drop","second") else 
      create_button "drop" grey white 1060 10 130 85 ("drop","second")in
  c.fbutton<-(first::second::third::[fourth])


let draw_cd ()=
  let (fire,trial,punishment)=
    (cd_store.fire,cd_store.trial,cd_store.punishment) in 
  let max=max (max fire trial) punishment in
  let lst=List.filter (fun int->int>0)[fire;trial;punishment] in 
  if max <=0 then () else let length=List.length cplace.skills in 
    if length >=3 then
      let message="Available in "^string_of_int max^" round." in 
      string_cal message black 1060 0 130 85 else
      let min_in_lst=List.fold_left (fun a b->min a b) 5 lst in 
      let message="Available in "^string_of_int min_in_lst^" round." in 
      if length=1 then
        (string_cal message black 1060 95 130 85) else
        string_cal message black 9200 0 130 85

let botton_drawing_helper int =
  if int=4 then (let four_skill=(List.nth (cplace.skills) 3) in
                 let fourth_botton=create_button four_skill 
                     lblue black 1060 10 130 85 ("skill", four_skill)in
                 cplace.fbutton<-([fourth_botton])) else 
    (let _=create_button "None" 
         grey black 1060 10 130 85 ("skill","None") in ());
  if int>=3 then (let thd_skill=(List.nth (cplace.skills) 2) in
                  let third_botton=create_button thd_skill
                      lblue black 920 10 130 85("skill", thd_skill) in
                  cplace.fbutton<-(third_botton::cplace.fbutton)) else
    (let _=create_button "None" 
         grey black 920 10 130 85 ("skill","None") in ());
  if int>=2 then ( let sec_skill=(List.nth (cplace.skills) 1) in
                   let second_botton=create_button sec_skill
                       lblue black 1060 105 130 85 ("skill",sec_skill)in
                   cplace.fbutton<-(second_botton::cplace.fbutton)) else
    (let _=create_button "None"
         grey black 1060 105 130 85 ("skill","None") in ())

let combat_four_botton int =
  let fst_skill=(List.nth (cplace.skills) 0) in
  botton_drawing_helper int;
  (let first_botton=create_button fst_skill
       lblue black 920 105 130 85 ("skill",fst_skill) in
   cplace.fbutton<-(first_botton::cplace.fbutton))

let combat_botton_helper ()=
  cplace.fbutton<-[];
  let lst=cplace.skills in 
  combat_four_botton (List.length lst)

let rec find_enemy_image_data name (lst:Color_convert.eimage list)=
  match lst with
  |h::t when h.name_data=name->h.image_data
  |h::t->find_enemy_image_data name t 
  |[]->failwith "can not find the image"

let enemy_list()=Array.to_list(( Engine.game_state).all_enemies_in_current_map)

let rec get_one_enemy id lst=
  match lst with
  |Engine.Enemy s::t when ((Enemy.get_id s)=id)->s
  |h::t -> get_one_enemy id t
  |[]->raise (Not_such_enemy id)

let skill_damage name=
  let state= Engine.get_player(Engine.game_state) in
  let skill= name|> Player.get_skill_by_skill_name state in 
  let skill_bool=List.mem skill (Player.available_skills_list state) in 
  if skill_bool then 
    (Player.choose_skill skill;
     Player.skill_strength skill) else 0

let enemy_skill_image name=
  match name with 
  |"punch"->draw_a_image Color_convert.enemy_punch 500 350
  |"scratch"->draw_a_image Color_convert.enemy_scar 500 350
  |"fire ball"->draw_a_image Color_convert.fireball 500 300
  |"curses"->draw_a_image Color_convert.curse 500 300
  |"stab"->draw_a_image Color_convert.hit 500 300
  |_->failwith"unbound image"

let enemy_skill t=
  let (name,damage)=Engine.choose_skill_random t in 
  player_reduce_health damage;
  health_bar();
  enemy_skill_image name;
  cplace.message_display<-("The enemy used "^name^" and you lose "
                           ^string_of_int damage^" points of health");
  Graphics.set_color black;
  Graphics.moveto 100 715;
  Graphics.draw_string ("-"^string_of_int damage);
  Thread.delay 1.5

let enemy_mon id=
  let hp = Enemy.get_hp (get_one_enemy id (enemy_list())) in 
  if hp<=0 then false else true

let game_over_mon ()=
  let (max,health)=get_player_health()in
  if health<=0 then 
    (dialog "Game over" Color_convert.cute_cat "cute cat"; 
     let _=Graphics.wait_next_event [Button_down] in 
     Graphics.close_graph()) else ()

let skill_image name=
  match name with 
  |"punch"->draw_a_image Color_convert.the_stab 500 350
  |"fire"->draw_a_image Color_convert.fire 500 300
  |"divine trial"->draw_a_image Color_convert.trial 500 300
  |"divine punishment"->draw_a_image Color_convert.lighting 500 300
  |_->failwith"unbound image"

let skill_info_helper ()=
  status_bar ();
  normal_four_botton cplace;
  health_bar ();
  info_bar();
  combat_botton_helper ();
  let the_enemy=get_one_enemy cplace.enemy_to_combat (enemy_list()) in
  let image_of_enemy=find_enemy_image_data (Enemy.get_name the_enemy)
      Color_convert.enemy_data in 
  draw_a_image image_of_enemy 900 550;
  enemy_health_bar the_enemy;
  draw_a_image Color_convert.player_in_combat 10 205

let enemy_skill_drawing_helper ()=
  skill_info_helper();
  Thread.delay 0.3;
  let the_enemy=get_one_enemy cplace.enemy_to_combat (enemy_list()) in
  enemy_skill the_enemy;
  clear_graph();
  skill_info_helper();
  draw_a_image Color_convert.player_in_combat 10 205;
  game_over_mon()


let skill_helper name=
  Graphics.clear_graph(); 
  Graphics.moveto 850 500;
  Graphics.set_color red;
  let skill_damage_int=skill_damage name in 
  cplace.message_display<-"You used "^name;
  Graphics.draw_string ("-"^string_of_int skill_damage_int);
  Enemy.reduce_hp (get_one_enemy cplace.enemy_to_combat (enemy_list ()))
    skill_damage_int;
  skill_info_helper();
  skill_image name;
  Thread.delay 1.5;
  Graphics.clear_graph(); 
  if enemy_mon cplace.enemy_to_combat then
    enemy_skill_drawing_helper() else 
    ()

let food_check s i=
  let food=Array.to_list (Engine.game_state).food_inventory in 
  match List.nth food i with 
  |Engine.Eaten ->()
  |Engine.Food f->
    cplace.item_selected <-Some ("food",i,Foods.Food.get_name f);
    cplace.message_display<-Foods.Food.get_description f;
    cplace.irefresh<-true

let weapon_check s i=
  let weapon=Array.to_list (Engine.game_state).weapon_inventory in 
  match List.nth weapon i with 
  |Engine.Empty->()
  |Engine.Weapon w->
    cplace.item_selected <-Some ("weapon",i,Weapons.Weapon.get_name w);
    cplace.message_display<-Weapons.Weapon.get_description w;
    cplace.irefresh<-true

let item_check s i=
  if s="food" then 
    food_check s i
  else
    weapon_check s i

let draw_inventory_item_helper name=
  match name with 
  |"bread"->Color_convert.bread_80
  |"Coffee"->Color_convert.coffee_80
  |"jade sword"->Color_convert.sword_80
  |"dagger"->Color_convert.dagger_80
  |_->Color_convert.cute_cat

let rec draw_food foods int : unit =
  match foods with
  |h::t->(match h with 
      |Engine.Eaten->draw_food t (int+1)
      |Engine.Food f->let name=Foods.Food.get_name f in 
        let pic= draw_inventory_item_helper name in 
        draw_a_image pic (260+(int*100)) 120;
        draw_food t (int+1))
  |[]->() 

let rec draw_weapon weapons int : unit =
  match weapons with
  |h::t->(match h with 
      |Engine.Empty->draw_weapon t (int+1)
      |Engine.Weapon w->let name=Weapons.Weapon.get_name w in 
        let pic= draw_inventory_item_helper name in 
        draw_a_image pic (260+(int*100)) 20;
        draw_weapon t (int+1))
  |[]->()

let item_draw ()=
  let food=Array.to_list (Engine.game_state).food_inventory in 
  draw_food food 0;
  let weapons=Array.to_list (Engine.game_state).weapon_inventory in 
  draw_weapon weapons 0


let ground_probe()= let player= Engine.game_state.player in 
  match player with 
  |Engine.Player _->
    (let (f,w)=Engine.check_item_on_player_ground Engine.game_state in 
     match f,w with
     |[],[]->"None" 
     |_,h::t->"Weapon"
     |h::t,_->"Food")
  |Engine.Died->"None"

let food_full_mon ()=
  let lst= Array.to_list Engine.game_state.food_inventory in 
  let lst_filter=List.filter (fun x->x=Engine.Eaten) lst in 
  List.length lst_filter=0

let weapon_full_mon ()=
  let lst= Array.to_list Engine.game_state.weapon_inventory in 
  let lst_filter=List.filter (fun x->x=Engine.Empty) lst in 
  List.length lst_filter=0


let rec parse  c=
  match c with
  |Command d when d="easy"->cplace.difficulty<-"easy"
  |Guide s when s="first"-> cplace.dialog_in_progress<-true;
    dialog (Engine.system_instr Engine.game_state)
      Color_convert.cute_cat "cute cat"
  |Guide s->()
  |Command s->()
  |Attack sk->skill_helper sk;
  |Item (s,i)->item_check s i
  |Next_con s->cplace.dialog_in_progress<-false;Graphics.clear_graph()
  |Order (c,t)->order_helper c t
  |Tnone->()

and order_drop_helper c=
  if Option.is_some cplace.item_selected then 
    (let (trigger,int,name)=Option.get cplace.item_selected in 
     match trigger with 
     |"weapon"->
       Engine.drop_one_weapon_to_current_location Engine.game_state int;
       cplace.item_selected<-None;
       cplace.irefresh<-true;
       cplace.message_display<-"you have dropped down "^name
     |"food"->
       Engine.drop_one_food_to_current_location Engine.game_state int;
       cplace.item_selected<-None;
       cplace.irefresh<-true;
       cplace.message_display<-"you have dropped down "^name
     |_->() ) else ()

and order_pick_helper c=
  match ground_probe () with
  |"Weapon"-> 
    (if not (weapon_full_mon()) then 
       cplace.message_display<-"you have picked up a weapon" else 
       cplace.message_display<-"you inventory is full");
    Engine.equip_weapon_in_current_loc Engine.game_state;
    cplace.irefresh<-true
  |"Food"->
    (if not (food_full_mon()) then 
       cplace.message_display<-"you have picked up a food" else 
       cplace.message_display<-"you inventory is full");
    Engine.take_one_food_in_current_location Engine.game_state;
    cplace.irefresh<-true
  |_->()

and order_helper c t=
  match c with 
  |"dialog"->parse (Guide t)
  |"weapon"->order_pick_helper c
  |"use"->(let (trigger,int,name)=Option.get cplace.item_selected in 
           match trigger with 
           |"food"->Engine.eat_one_food_in_inventory Engine.game_state int;
             cplace.item_selected<-None;cplace.irefresh<-true;
             cplace.message_display<-"You ate "^name^ " and recovered health"
           |_->())
  |"drop"->order_drop_helper c
  |_->parse  (Command t)


let tsensor(c:clist)=
  if not cplace.dialog_in_progress then () else
    (let status=Graphics.wait_next_event [Button_down] in 
     let sense b s=
       match b with
       |Action_button _->() 
       |Dialog_sense s->parse  (Next_con s)
       |Bnone->() 
       |Action_box _->()in
     let _=sense (c.dialog) status in ())

let ksensor status=
  let key=status.key in 
  match key with 
  |'a'
  |'A'->Engine.move_player_down Engine.game_state; cplace.irefresh<-true
  |'d'
  |'D'->Engine.move_player_up Engine.game_state; cplace.irefresh<-true
  |'w'
  |'W'->Engine.move_player_right Engine.game_state; cplace.irefresh<-true
  |'s'
  |'S'->Engine.move_player_left Engine.game_state; cplace.irefresh<-true
  |_->()

let action_button_helper s i (x,y,w,h) (c,t)=
  (if((x<s.mouse_x)&&((x+w)>s.mouse_x)&&
      (y<s.mouse_y)&&((y+h)>s.mouse_y)&&s.button)
     =true then (if i=Normal then 
                   parse (Order(c,t)) else 
                   (match c with 
                    |"skill"->
                      parse  (Attack t)
                    |_->())) else ())

let rec fensor (c:clist) i=
  let status=Graphics.wait_next_event [Button_down;Key_pressed] in 
  let sense b (s:Graphics.status)=
    match b with
    |Action_button ((x,y,w,h),(command,trigger))->
      action_button_helper s i (x,y,w,h) (command,trigger)
    |Action_box ((x,y,w,h),(string,int))->if((x<s.mouse_x)&&((x+w)>s.mouse_x)&&
                                             (y<s.mouse_y)&&((y+h)>s.mouse_y)&&
                                             s.button) then
        (parse (Item (string,int))) else ()
    |Dialog_sense s->()
    |_->fensor c i in
  if status.button then 
    (let _=List.rev_map (fun butt->sense butt status) c.fbutton in 
     ()) else 
    ksensor status


let clear_screen ()=
  if cplace.irefresh=true then Graphics.clear_graph() else ()

let cd_mon ()=
  let cd_lst=Player.get_all_skill_format(Engine.get_player Engine.game_state) in 
  let update_cd skill int=
    match skill with 
    |"fire"->cd_store.fire<-int
    |"divine trial"->cd_store.trial<-int
    |"divine punishment"->cd_store.punishment<-int
    |_->() in 
  let rec update_cd_matcher lst=
    match lst with 
    |(skill,int)::t-> update_cd skill int; update_cd_matcher t 
    |[]->() in 
  update_cd_matcher cd_lst


let skill_mon ()=
  match Engine.game_state.player with
  |Player s->
    cplace.skills<-List.map (fun x->Player.skill_name x) 
        (Player.available_skills_list (Engine.get_player Engine.game_state));
    cd_mon()
  |Died->()

let level_mon int=
  if int<>cplace.player_level then
    (dialog ("you defeated the enemy and you are upgraded to level"^
             (string_of_int int)^".") Color_convert.cute_cat
       "cute cat";cplace.player_level<-int;
     let _=Graphics.wait_next_event[Button_down]in ()) else ()

let rec combat id=
  skill_mon();
  skill_info_helper();
  draw_cd();
  fensor cplace Combat;
  game_over_mon();
  Graphics.clear_graph();
  if enemy_mon id then
    (combat id) else 
    let the_enemy=get_one_enemy cplace.enemy_to_combat (enemy_list()) in
    let name=Enemy.get_name the_enemy in
    let expeience=Enemy.get_experience the_enemy in
    (cplace.enemy_to_combat<-"none";
     cplace.message_display<-
       ("you have defeated "^name^" and got "^
        (string_of_int expeience)^" points of expeience.");
     Engine.delete_one_enemy_from_state Engine.game_state)

let combat_mon()=if cplace.enemy_to_combat<>"none" then 
    let name=(get_one_enemy cplace.enemy_to_combat (enemy_list())
              |>Enemy.get_name) in
    (Graphics.clear_graph();
     cplace.message_display<-"You entered combat with "^name
                             ^". It's your turn!";
     cplace.player_level<-get_player_level ();
     combat cplace.enemy_to_combat )
  else ()

let ground_mon ()=
  let player= Engine.game_state.player in 
  match player with 
  |Engine.Player _->
    (let (f,w)=Engine.check_item_on_player_ground Engine.game_state in 
     match f,w with
     |[],[]->cplace.item_ground<-false 
     |_,h::t->cplace.item_ground<-true
     |h::t,_->cplace.item_ground<-true)
  |Engine.Died->()

let enemy_loc_mon()=
  let (bool,id)=Engine.check_enemy_in_current_loc Engine.game_state in 
  if bool then cplace.enemy_to_combat<-id else 
    ()

let win_mon ()=
  if Engine.check_wins Engine.game_state then 
    (dialog "You win!" Color_convert.cute_cat "cute cat"; 
     let _=Graphics.wait_next_event [Button_down] in 
     Graphics.close_graph()) else ()

let normal_bars()=
  status_bar ();
  experience_bar();
  health_bar ();
  info_bar();
  draw_inventory()


let rec main () =
  try (cplace.fbutton<-[];
       cplace.dialog<-Bnone;
       cplace.irefresh<-false;
       Engine.transfer_player_to_branch_map Engine.game_state;
       Engine.transfer_player_to_main_map Engine.game_state;
       Map_builder.draw();
       normal_bars ();
       ground_mon();
       normal_four_botton cplace;
       item_draw ();
       win_mon();
       fensor cplace Normal;
       tsensor cplace;
       enemy_loc_mon();
       combat_mon();
       level_mon (get_player_level());
       clear_screen();
       main ()) with Graphics.Graphic_failure s->()

let rec beginning () =
  Graphics.moveto 500 650;
  Graphics.draw_string "Welcome to Cat Quest";
  Graphics.moveto 500 620;
  Graphics.draw_string "please click to begin:";
  draw_a_image Color_convert.logo 450 200;
  cplace.fbutton<-[create_button "Start" 
                     green black 500 550 200 50 ("diff","easy")];
  fensor cplace Normal;
  if cplace.difficulty<>"empty" then 
    (Graphics.clear_graph(); main () )else beginning ()

let init () =
  Graphics.open_graph " 1200x800+100";
  Map_builder.text_init();
  beginning ()

let ()=init ()

