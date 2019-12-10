OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=oUnit,graphics,threads,Str,yojson,ANSITerminal,threads.posix

build:
	ocamlbuild -use-ocamlfind color_convert.byte player.byte enemy.byte foods.byte weapons.byte maps.byte  engine.byte map_builder.byte -tag thread gui.byte 

clean:
	ocamlbuild -clean

play:
	ocamlbuild -use-ocamlfind gui.byte && ./gui.byte

test: 
	ocamlbuild -use-ocamlfind -tag 'debug' test.byte && ./test.byte

docs: build
	mkdir -p doc
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html  -d doc \
		-m A engine.ml player.ml foods.ml weapons.ml maps.ml enemy.ml color_convert.ml map_builder.ml gui.ml