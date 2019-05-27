
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | SEMICOLON
    | RIGHT_PAREN
    | RIGHT_BRACK
    | LEFT_PAREN
    | LEFT_BRACK
    | ID of (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 16 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
  )
    | EOF
    | DOT
    | COMMA
    | BAR
    | ASSIGN
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState45
  | MenhirState41
  | MenhirState34
  | MenhirState29
  | MenhirState27
  | MenhirState21
  | MenhirState20
  | MenhirState19
  | MenhirState14
  | MenhirState13
  | MenhirState11
  | MenhirState9
  | MenhirState7
  | MenhirState6
  | MenhirState4
  | MenhirState0

let rec _menhir_goto_separated_nonempty_list_SEMICOLON_valence_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_valence_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv209 * _menhir_state * (
# 20 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 64 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_valence_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv207 * _menhir_state * (
# 20 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 72 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_valence_) : 'tv_separated_nonempty_list_SEMICOLON_valence_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 20 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 79 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_valence_ = 
# 243 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 85 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_valence_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv208)) : 'freshtv210)
    | MenhirState34 | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv213) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_valence_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv211) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_SEMICOLON_valence_) : 'tv_separated_nonempty_list_SEMICOLON_valence_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_valence__ = 
# 144 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x )
# 100 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_valence__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv212)) : 'freshtv214)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_BAR_operatorDef_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_BAR_operatorDef_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv201) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_BAR_operatorDef_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv199) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_BAR_operatorDef_) : 'tv_separated_nonempty_list_BAR_operatorDef_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_BAR_operatorDef__ = 
# 144 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x )
# 121 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_BAR_operatorDef__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv200)) : 'freshtv202)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv205 * _menhir_state * (
# 17 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 129 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_BAR_operatorDef_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv203 * _menhir_state * (
# 17 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 137 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_BAR_operatorDef_) : 'tv_separated_nonempty_list_BAR_operatorDef_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 17 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 144 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_BAR_operatorDef_ = 
# 243 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 150 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_separated_nonempty_list_BAR_operatorDef_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv204)) : 'freshtv206)
    | _ ->
        _menhir_fail ()

and _menhir_goto_fixedValence : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_fixedValence -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv193 * _menhir_state * (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 164 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_fixedValence) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv191 * _menhir_state * (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 172 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_3 : 'tv_fixedValence) : 'tv_fixedValence) = _v in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 179 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))), _) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_fixedValence = 
# 30 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
  ( match _3 with | Types.FixedValence (binds, result) -> FixedValence (_1 :: binds, result) )
# 185 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_fixedValence _menhir_env _menhir_stack _menhir_s _v) : 'freshtv192)) : 'freshtv194)
    | MenhirState34 | MenhirState6 | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv197) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_fixedValence) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv195) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_fixedValence) : 'tv_fixedValence) = _v in
        ((let _v : (
# 20 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 200 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ) = 
# 36 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
                                   ( _1                       )
# 204 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_valence _menhir_env _menhir_stack _menhir_s _v) : 'freshtv196)) : 'freshtv198)
    | _ ->
        _menhir_fail ()

and _menhir_goto_valence : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 20 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 213 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv189 * _menhir_state * (
# 20 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 221 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv183 * _menhir_state * (
# 20 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 231 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
        | LEFT_PAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19) : 'freshtv184)
    | RIGHT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv185 * _menhir_state * (
# 20 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 249 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : (
# 20 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 254 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_valence_ = 
# 241 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 259 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_valence_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv186)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv187 * _menhir_state * (
# 20 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 269 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv188)) : 'freshtv190)

and _menhir_goto_arity : _menhir_env -> 'ttv_tail -> (
# 19 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.arity)
# 277 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv181 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 284 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) = Obj.magic _menhir_stack in
    let (_v : (
# 19 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.arity)
# 289 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv179 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 295 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) = Obj.magic _menhir_stack in
    let ((_2 : (
# 19 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.arity)
# 300 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) : (
# 19 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.arity)
# 304 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) = _v in
    ((let (_menhir_stack, _menhir_s, (_1 : (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 309 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    ))) = _menhir_stack in
    let _v : (
# 17 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 314 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    ) = 
# 48 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
                      ( OperatorDef(_1, _2) )
# 318 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv177) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 17 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 326 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv175 * _menhir_state * (
# 17 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 333 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv169 * _menhir_state * (
# 17 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 343 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41) : 'freshtv170)
    | EOF | ID _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv171 * _menhir_state * (
# 17 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 359 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : (
# 17 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 364 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_BAR_operatorDef_ = 
# 241 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 369 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_separated_nonempty_list_BAR_operatorDef_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv172)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv173 * _menhir_state * (
# 17 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 379 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)) : 'freshtv176)) : 'freshtv178)) : 'freshtv180)) : 'freshtv182)

and _menhir_goto_sort : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 387 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv151 * _menhir_state) * _menhir_state * (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 397 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | LEFT_PAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv149 * _menhir_state) * _menhir_state * (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 411 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState9 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv147 * _menhir_state) * _menhir_state * (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 419 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 425 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 432 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ) = 
# 24 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
                                ( _2                   )
# 436 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
             in
            _menhir_goto_sort _menhir_env _menhir_stack _menhir_s _v) : 'freshtv148)) : 'freshtv150)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9) : 'freshtv152)
    | MenhirState20 | MenhirState14 | MenhirState11 | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv155 * _menhir_state * (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 448 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) * _menhir_state * (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 452 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | LEFT_PAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | DOT | RIGHT_BRACK | RIGHT_PAREN | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv153 * _menhir_state * (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 466 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) * _menhir_state * (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 470 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 475 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))), _, (_2 : (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 479 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) = _menhir_stack in
            let _v : (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 484 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ) = 
# 26 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
                                ( SortAp(_1, _2) )
# 488 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
             in
            _menhir_goto_sort _menhir_env _menhir_stack _menhir_s _v) : 'freshtv154)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11) : 'freshtv156)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv161 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 500 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) * _menhir_state * (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 504 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | LEFT_PAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | RIGHT_BRACK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv159 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 518 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) * _menhir_state * (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 522 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState14 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv157 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 530 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) * _menhir_state * (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 534 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 540 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))), _, (_3 : (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 544 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (
# 20 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 551 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ) = 
# 35 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
                                   ( VariableValence (_1, _3) )
# 555 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
             in
            _menhir_goto_valence _menhir_env _menhir_stack _menhir_s _v) : 'freshtv158)) : 'freshtv160)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14) : 'freshtv162)
    | MenhirState34 | MenhirState6 | MenhirState21 | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv167 * _menhir_state * (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 567 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv163 * _menhir_state * (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 577 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState20 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
            | LEFT_PAREN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21) : 'freshtv164)
        | ID _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
        | LEFT_PAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | RIGHT_PAREN | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv165 * _menhir_state * (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 601 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 606 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) = _menhir_stack in
            let _v : 'tv_fixedValence = 
# 32 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
  ( FixedValence ([], _1) )
# 611 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
             in
            _menhir_goto_fixedValence _menhir_env _menhir_stack _menhir_s _v) : 'freshtv166)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20) : 'freshtv168)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_valence__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMICOLON_valence__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv145) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_loption_separated_nonempty_list_SEMICOLON_valence__) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv143) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((xs : 'tv_loption_separated_nonempty_list_SEMICOLON_valence__) : 'tv_loption_separated_nonempty_list_SEMICOLON_valence__) = _v in
    ((let _v : 'tv_valenceList = let _1 = 
# 232 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 634 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
     in
    
# 38 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
                                                ( _1 )
# 639 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv141) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_valenceList) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv131) * _menhir_state * 'tv_valenceList) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv127) * _menhir_state * 'tv_valenceList) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv125) * _menhir_state * 'tv_valenceList) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, (valenceList : 'tv_valenceList)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 19 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.arity)
# 665 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ) = 
# 46 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
    ( Arity ([], valenceList) )
# 669 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
             in
            _menhir_goto_arity _menhir_env _menhir_stack _v) : 'freshtv126)) : 'freshtv128)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv129) * _menhir_state * 'tv_valenceList) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)) : 'freshtv132)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv139) * _menhir_state * 'tv_nameList))) * _menhir_state * 'tv_valenceList) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv135) * _menhir_state * 'tv_nameList))) * _menhir_state * 'tv_valenceList) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv133) * _menhir_state * 'tv_nameList))) * _menhir_state * 'tv_valenceList) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _, (_2 : 'tv_nameList)), _, (_5 : 'tv_valenceList)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : (
# 19 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.arity)
# 699 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ) = 
# 44 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
    ( Arity (_2, _5) )
# 703 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
             in
            _menhir_goto_arity _menhir_env _menhir_stack _v) : 'freshtv134)) : 'freshtv136)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv137) * _menhir_state * 'tv_nameList))) * _menhir_state * 'tv_valenceList) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)) : 'freshtv140)
    | _ ->
        _menhir_fail ()) : 'freshtv142)) : 'freshtv144)) : 'freshtv146)

and _menhir_reduce26 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 719 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 725 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    ))) = _menhir_stack in
    let _v : (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 730 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    ) = 
# 25 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
                                ( SortName _1    )
# 734 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
     in
    _menhir_goto_sort _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 741 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack)

and _menhir_goto_separated_nonempty_list_COMMA_ID_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_ID_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv119 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 756 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_ID_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv117 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 764 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_ID_) : 'tv_separated_nonempty_list_COMMA_ID_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 771 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_COMMA_ID_ = 
# 243 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 777 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv118)) : 'freshtv120)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv123) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_ID_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv121) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_ID_) : 'tv_separated_nonempty_list_COMMA_ID_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_ID__ = 
# 144 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x )
# 792 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv122)) : 'freshtv124)
    | _ ->
        _menhir_fail ()

and _menhir_reduce12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_valence__ = 
# 142 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 803 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_SEMICOLON_valence__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | LEFT_PAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 825 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_BRACK ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 837 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | LEFT_PAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13) : 'freshtv114)
    | DOT | ID _ | LEFT_PAREN | RIGHT_PAREN | SEMICOLON ->
        _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 859 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)

and _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_ID__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv111) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_loption_separated_nonempty_list_COMMA_ID__) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv109) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((xs : 'tv_loption_separated_nonempty_list_COMMA_ID__) : 'tv_loption_separated_nonempty_list_COMMA_ID__) = _v in
    ((let _v : 'tv_nameList = let _1 = 
# 232 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 877 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
     in
    
# 40 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
                                    ( _1 )
# 882 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv107) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_nameList) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv105) * _menhir_state * 'tv_nameList) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHT_BRACK ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv101) * _menhir_state * 'tv_nameList) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv97) * _menhir_state * 'tv_nameList)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | LEFT_PAREN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | RIGHT_PAREN ->
                _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34) : 'freshtv98)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv99) * _menhir_state * 'tv_nameList)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)) : 'freshtv102)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv103) * _menhir_state * 'tv_nameList) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)) : 'freshtv106)) : 'freshtv108)) : 'freshtv110)) : 'freshtv112)

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 934 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv91 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 946 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv92)
    | RIGHT_BRACK ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 962 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 967 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_ID_ = 
# 241 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 972 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv94)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 982 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_loption_separated_nonempty_list_BAR_operatorDef__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_BAR_operatorDef__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv89 * 'tv_option_BAR_) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_loption_separated_nonempty_list_BAR_operatorDef__) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv87 * 'tv_option_BAR_) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    let ((xs : 'tv_loption_separated_nonempty_list_BAR_operatorDef__) : 'tv_loption_separated_nonempty_list_BAR_operatorDef__) = _v in
    ((let (_menhir_stack, (_1 : 'tv_option_BAR_)) = _menhir_stack in
    let _v : (
# 18 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef list)
# 1006 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    ) = let lst = 
# 232 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 1010 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
     in
    
# 50 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
                                                          ( lst )
# 1015 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv85) = _menhir_stack in
    let (_v : (
# 18 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef list)
# 1022 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv83 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1028 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    ))) = Obj.magic _menhir_stack in
    let (_v : (
# 18 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef list)
# 1033 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv81 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1039 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    ))) = Obj.magic _menhir_stack in
    let ((defs : (
# 18 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef list)
# 1044 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) : (
# 18 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef list)
# 1048 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) = _v in
    ((let (_menhir_stack, _menhir_s, (sortName : (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1053 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    ))) = _menhir_stack in
    let _2 = () in
    let _v : (
# 16 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (string * Types.sortDef)
# 1059 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    ) = 
# 53 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
                                                  ( (sortName, SortDef ([], defs)) )
# 1063 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv79) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 16 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (string * Types.sortDef)
# 1071 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv77 * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (string * Types.sortDef)
# 1078 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | EOF ->
        _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45) : 'freshtv78)) : 'freshtv80)) : 'freshtv82)) : 'freshtv84)) : 'freshtv86)) : 'freshtv88)) : 'freshtv90)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1095 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_BRACK ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | RIGHT_BRACK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv69) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState27 in
            ((let _v : 'tv_loption_separated_nonempty_list_COMMA_ID__ = 
# 142 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 1117 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv70)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27) : 'freshtv72)
    | LEFT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
        | LEFT_PAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | RIGHT_PAREN ->
            _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6) : 'freshtv74)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1147 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)

and _menhir_goto_list_sortDef_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_sortDef_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv53 * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (string * Types.sortDef)
# 1161 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) * _menhir_state * 'tv_list_sortDef_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv51 * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (string * Types.sortDef)
# 1167 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) * _menhir_state * 'tv_list_sortDef_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : (
# 16 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (string * Types.sortDef)
# 1172 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))), _, (xs : 'tv_list_sortDef_)) = _menhir_stack in
        let _v : 'tv_list_sortDef_ = 
# 213 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 1177 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_list_sortDef_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv52)) : 'freshtv54)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv67 * _menhir_state * 'tv_list_sortDef_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv63 * _menhir_state * 'tv_list_sortDef_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv61 * _menhir_state * 'tv_list_sortDef_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_list_sortDef_)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 14 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.language)
# 1196 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ) = 
# 56 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
                      ( Language(Belt.Map.String.fromArray (Belt.List.toArray _1)) )
# 1200 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv59) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 14 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.language)
# 1208 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv57) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 14 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.language)
# 1216 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv55) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 14 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.language)
# 1224 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) : (
# 14 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.language)
# 1228 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv56)) : 'freshtv58)) : 'freshtv60)) : 'freshtv62)) : 'freshtv64)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv65 * _menhir_state * 'tv_list_sortDef_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)) : 'freshtv68)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_BAR_ : _menhir_env -> 'ttv_tail -> 'tv_option_BAR_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv49 * 'tv_option_BAR_) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | EOF ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv47) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState4 in
        ((let _v : 'tv_loption_separated_nonempty_list_BAR_operatorDef__ = 
# 142 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 1258 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_BAR_operatorDef__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv48)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4) : 'freshtv50)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (string * Types.sortDef)
# 1274 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv17 * _menhir_state * (
# 17 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 1283 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv19) * _menhir_state * 'tv_nameList))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv21 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1297 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv24)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state * (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 1310 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv27 * _menhir_state * (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 1319 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state * (
# 20 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 1328 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv31 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1337 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) * _menhir_state * (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 1341 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv33 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1350 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv35 * _menhir_state * (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 1359 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) * _menhir_state * (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 1363 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state) * _menhir_state * (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 1372 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv41) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv42)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43 * 'tv_option_BAR_) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv44)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv45) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv46)

and _menhir_reduce6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_sortDef_ = 
# 211 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 1399 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
     in
    _menhir_goto_list_sortDef_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1406 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1418 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv5) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
            ((let x = () in
            let _v : 'tv_option_BAR_ = 
# 116 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( Some x )
# 1433 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
             in
            _menhir_goto_option_BAR_ _menhir_env _menhir_stack _v) : 'freshtv4)) : 'freshtv6)
        | EOF | ID _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
            ((let _v : 'tv_option_BAR_ = 
# 114 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( None )
# 1442 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
             in
            _menhir_goto_option_BAR_ _menhir_env _menhir_stack _v) : 'freshtv8)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv9 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1452 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)) : 'freshtv12)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1463 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and languageDef : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 14 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.language)
# 1483 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | EOF ->
        _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 269 "/Users/joel/.opam/default/lib/menhir/standard.mly"
  

# 1514 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
