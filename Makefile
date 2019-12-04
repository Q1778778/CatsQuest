build:
	ocamlbuild -use-ocamlfind color_convert.byte enemy.byte  foods.byte weapons.byte maps.byte  engine.byte player.byte map_builder.byte -tag thread gui.byte 

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report search_src.zip bisect*.out

play:
	ocamlbuild -use-ocamlfind gui.byte && ./gui.byte

test: 
	ocamlbuild -use-ocamlfind -tag 'debug' test.byte && ./test.byte