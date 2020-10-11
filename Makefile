format:
	@fd --extension ml --extension mli | xargs ocamlformat --enable-outside-detected-project --inplace

doc:
	@dune build @doc

install-deps:
	@opam install base zarith cbor digestif js_of_ocaml ppx_jane ppx_inline_test sha dune

.PHONY: format doc install-deps
