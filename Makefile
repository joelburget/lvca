format:
	@fd --extension ml --extension mli | xargs ocamlformat --enable-outside-detected-project --inplace

doc:
	@dune build @doc

# Note: zarith requires gmp, digestif requires pkg-config
install-deps:
	@opam install angstrom base cbor crowbar digestif fmt js_of_ocaml-lwt js_of_ocaml-ppx js_of_ocaml-tyxml ppx_jane re react reactiveData tyxml-ppx zarith zarith_stubs_js

# TODO: opam-dune-lint
lint:
	@dune build @fmt

.PHONY: format doc install-deps lint
