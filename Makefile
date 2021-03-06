OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=oUnit,graphics,threads,yojson,threads.posix


MAPS_FILE = src/game_models/maps.ml
PLAYER_FILE = src/game_models/player.ml
WEAPONS_FILE = src/game_models/weapons.ml
ENEMY_FILE = src/game_models/enemy.ml
FOODS_FILE = src/game_models/foods.ml
BUILDER_FILE = src/engine/builder.ml
ENGINE_FILE = src/engine/engine.ml
MAP_BUILDER_FILE = src/user_interface/map_builder.ml
GUI_FILE = src/user_interface/gui.ml
COLOR_CONVERT_FILE = src/user_interface/color_convert.ml

play:
	utop src/main/main.ml

checkenv:
	bash checkenv.sh

clean:
	rm -rf */_build
	rm -rf */_digest
	rm -rf */_log
	rm -rf *.byte
	rm -rf *.cmo
	rm -rf *.cmi
	rm -rf .DS_Store

utest: 
	utop test/test.ml

docs: build
	mkdir -p doc
	ocamlfind ocamldoc -I _build -package -open -slash $(PKGS) \
		-html  -d doc \
		-m  A $(MAPS_FILE) $(PLAYER_FILE) $(FOODS_FILE) $(ENEMY_FILE) \
		 $(WEAPONS_FILE) $(BUILDER_FILE) $(ENGINE_FILE) $(COLOR_CONVERT_FILE) \
		 $(MAP_BUILDER_FILE) $(GUI_FILE)