open Graphics

type stage=
  |Combat
  |Normal

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
  Graphics.draw_string text

let health_bar health=
  whitebox_draw 100 730 300 750 5;
  Graphics.set_color red;
  Graphics.fill_rect 100 730 (health*2) 20;
  Graphics.set_color white;
  Graphics.moveto 180 735;
  Graphics.draw_string ((string_of_int health)^"/100");
  Graphics.set_color black

let init state =
  Graphics.open_graph " 1200x800+100";
  Graphics.set_color black;
  Graphics.set_line_width 5;
  Graphics.moveto 0 150;
  Graphics.lineto 1200 150;
  health_bar 100;
  Graphics.moveto 40 733;
  Graphics.draw_string"Health:"


