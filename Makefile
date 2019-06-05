.PHONY: test build

test:
	dune exec ./test/test.exe

build:
	dune build @all
