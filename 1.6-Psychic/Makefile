all:
	ocamlfind ocamlopt -linkpkg -package batteries -annot -g -o algo.exe algo.ml
	ctags -R .

clean:
	rm -f *.cmx *.cmi *.o *.exe *.annot tags *.*~
