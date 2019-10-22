.PHONY: build
build:
	dune build @all

.PHONY: clean
clean:
	dune clean

.PHONY: fmt
fmt:
	ocamlformat -i src/*.ml
