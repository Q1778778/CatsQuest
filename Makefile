color:
	ocamlbuild -pkg graphics -use-ocamlfind color_convert.byte
	utop -init color_convert.ml
