MODULES=state stock portfolio command main authors date
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind 
PKGS=unix,oUnit,str,qcheck,yojson,ANSITerminal

default: clean build 
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

buildco:
	ocamlbuild -pkg cohttp-lwt-unix trialrun.native 

use: build
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) 

zip:
	zip stock_simulator.zip *.ml* *_tags Makefile json/*.json *.txt
	
docs: docs-public docs-private
	
# docs-public: build
# 	mkdir -p doc.public
# 	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
# 		-html -stars -d doc.public $(MLIS)

# docs-private: build
# 	mkdir -p doc.private
# 	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
# 		-html -stars -d doc.private \
# 		-inv-merge-ml-mli -m A $(MLIS) $(MLS)
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public -hide-warnings $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)


clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private stock_simulator.zip

