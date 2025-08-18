all:
	dune build
	dune exec bin/main.exe -- --oxcaml

clean:
	dune clean

.PHONY: all clean
