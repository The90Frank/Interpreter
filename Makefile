OCAML = ocaml

.PHONY: install test-estivo test-inverno test run

install:
	@which $(OCAML) > /dev/null 2>&1 || (echo "OCaml is required. Install it with: apt install ocaml / brew install ocaml" && exit 1)
	@echo "OCaml found: $$($(OCAML) -version)"

test-estivo:
	@echo "=== Test Estivo 1617 ==="
	$(OCAML) InterpreteEstivo1617.ml testEstivo1617.ml

test-inverno:
	@echo "=== Test Inverno 1718 ==="
	$(OCAML) InterpreteInverno1718.ml testInverno1718.ml

test: test-estivo test-inverno

run: test
