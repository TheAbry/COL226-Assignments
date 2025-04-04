OCAMLC=ocamlc
OCAMLYACC=ocamlyacc -v
OCAMLLEX=ocamllex

all: a

a: ast.cmo parser.cmo lexer.cmo main.cmo
	$(OCAMLC) -o $@ $^

parser.cmo: parser.ml parser.cmi
	$(OCAMLC) -c $<

parser.cmi: parser.mli
	$(OCAMLC) -c $<

parser.ml: parser.mly
	$(OCAMLYACC) $<

lexer.cmo: lexer.ml
	$(OCAMLC) -c $<

lexer.ml: lexer.mll
	$(OCAMLLEX) $<

ast.cmo: ast.ml
	$(OCAMLC) -c $<

main.cmo: main.ml
	$(OCAMLC) -c $<

clean:
	rm -f *.cmo *.cmi parser.ml lexer.ml a.out parser.mli parser.output
