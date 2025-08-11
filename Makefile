all:
	dune build
	dune exec bin/main.exe -- --jane

clean:
	dune clean

.PHONY: all clean
