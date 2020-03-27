format:
	@fd --extension ml --extension mli | xargs ocamlformat --enable-outside-detected-project --inplace

doc:
	@dune build @doc

install-deps:
	@opam install base bignum bonsai cbor core_kernel digestif js_of_ocaml ppx_jane re

.PHONY: format doc install-deps
