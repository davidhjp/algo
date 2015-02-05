all:
	ocamlfind ocamlopt -annot  -linkpkg -package batteries -o algo.exe algo.ml
	ctags -R .

clean:
	rm -f *.cmx *.cmi *.o *.exe *.annot
