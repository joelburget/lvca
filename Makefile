format:
	@fd --extension ml --extension mli | xargs ocamlformat --enable-outside-detected-project --inplace

install-deps:
	opam install base bignum bonsai cbor core_kernel digestif js_of_ocaml ppx_jane re
