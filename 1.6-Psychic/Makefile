all:
	ocamlfind ocamlopt -linkpkg -package batteries -annot -o algo.exe algo.ml
	ctags -R .

clean:
	rm -f *.cmx *.cmi *.o *.exe *.annot tags
