SHELL := /usr/local/bin/bash

run: build

build:
	dune build @default

test:
	dune runtest

clean:
	dune clean

fmt:
	ocamlformat -i */*.ml

doc:
	dune build @doc

install:
	dune build @install
	dune install

.PHONY: run build test clean fmt doc
.PHONY: install
