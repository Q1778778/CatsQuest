build:
	ocamlbuild -pkgs Graphics -use-ocamlfind color_convert.byte enemy.byte maps.byte  engine.byte player.byte -tag thread gui.byte

