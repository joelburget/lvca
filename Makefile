format:
	@fd --extension ml --extension mli | xargs ocamlformat --enable-outside-detected-project --inplace

doc:
	@dune build @doc

# Note: zarith requires gmp, digestif requires pkg-config
install-deps:
	@opam install angstrom base brr cbor crowbar digestif fmt note omd ppx_jane ppxlib zarith zarith_stubs_js ppx_blob

lint:
	@opam-dune-lint
	@dune build @fmt
	@dune build @check @runtest

server:
	@echo "http://localhost:8000"
	@python3 server.py

.PHONY: format doc install-deps lint server
