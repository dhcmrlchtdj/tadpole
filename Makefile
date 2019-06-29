mlis := $(patsubst %.ml,%,$(wildcard src/*.ml))

.PHONY: main
main: byte

.PHONY: test
test: main
	# @ ./main -token ./test/address.wast
	@ ./main -ast ./test/address.wast

.PHONY: byte
byte: $(mlis)
	@ ocamlbuild -use-ocamlfind src/main.byte
	@ ln -sf ./main.byte ./main

.PHONY: native
native: $(mlis)
	@ ocamlbuild -use-ocamlfind src/main.native
	@ ln -sf ./main.native ./main

.PHONY: $(mlis)
$(mlis):
	-@ ocamlbuild -use-ocamlfind $@.inferred.mli

.PHONY: clean
clean:
	@ ocamlbuild -clean
	@ rm -rf ./main
	@ rm -rf ./main.byte
	@ rm -rf ./main.native

.PHONY: fmt
fmt:
	ocamlformat -i src/*.ml
	ocp-indent -i src/*.ml
