mlis := $(patsubst %.ml,%,$(wildcard src/*.ml))

.PHONY: build
build: wat wasm

.PHONY: wat
wat: $(mlis)
	@ ocamlbuild -use-ocamlfind src/wat.byte

.PHONY: wat
wasm: $(mlis)
	# @ ocamlbuild -use-ocamlfind src/wasm.native

.PHONY: $(mlis)
$(mlis):
	-@ ocamlbuild -use-ocamlfind $@.inferred.mli

.PHONY: clean
clean:
	@ ocamlbuild -clean
	@ rm -rf ./*.native

.PHONY: fmt
fmt:
	ocamlformat -i src/*.ml
