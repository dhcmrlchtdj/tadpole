SHELL := /usr/local/bin/bash

# run: build
#     ./_build/default/main.bc

build:
	dune build @all

test:
	echo "TODO"

clean:
	dune clean

fmt:
	ocamlformat -i src/*.ml

doc:
	echo "TODO"

install: build
	dune install

.PHONY: run build test clean fmt doc
.PHONY: install
