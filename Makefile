build:
	ocamlbuild -use-ocamlfind color_convert.byte player.byte enemy.byte foods.byte weapons.byte maps.byte  engine.byte map_builder.byte -tag thread gui.byte 

clean:
	ocamlbuild -clean

play:
	ocamlbuild -use-ocamlfind gui.byte && ./gui.byte

test: 
	ocamlbuild -use-ocamlfind -tag 'debug' test.byte && ./test.byte