
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | SEMICOLON
    | RULE_NAME of (
# 15 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 12 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
  )
    | RIGHT_S_ARR
    | RIGHT_PAREN
    | RIGHT_OXFORD
    | RIGHT_D_ARR
    | RIGHT_BRACK
    | LINE
    | LEFT_S_ARR
    | LEFT_PAREN
    | LEFT_OXFORD
    | LEFT_D_ARR
    | LEFT_BRACK
    | ID of (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 28 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
  )
    | EQ
    | EOF
    | DOT
    | CTX_SEPARATOR
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
# 34 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 78 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_valence_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv207 * _menhir_state * (
# 34 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 86 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_valence_) : 'tv_separated_nonempty_list_SEMICOLON_valence_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 34 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 93 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_valence_ = 
# 243 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 99 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 114 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 135 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_BAR_operatorDef__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv200)) : 'freshtv202)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv205 * _menhir_state * (
# 31 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 143 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_BAR_operatorDef_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv203 * _menhir_state * (
# 31 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 151 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_BAR_operatorDef_) : 'tv_separated_nonempty_list_BAR_operatorDef_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 31 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 158 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_BAR_operatorDef_ = 
# 243 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 164 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 178 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_fixedValence) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv191 * _menhir_state * (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 186 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_3 : 'tv_fixedValence) : 'tv_fixedValence) = _v in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 193 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))), _) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_fixedValence = 
# 44 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
  ( match _3 with | Types.FixedValence (binds, result) -> FixedValence (_1 :: binds, result) )
# 199 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 34 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 214 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ) = 
# 50 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
                                   ( _1                       )
# 218 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_valence _menhir_env _menhir_stack _menhir_s _v) : 'freshtv196)) : 'freshtv198)
    | _ ->
        _menhir_fail ()

and _menhir_goto_valence : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 34 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 227 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv189 * _menhir_state * (
# 34 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 235 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv183 * _menhir_state * (
# 34 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 245 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 34 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 263 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : (
# 34 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 268 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_valence_ = 
# 241 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 273 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_valence_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv186)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv187 * _menhir_state * (
# 34 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 283 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv188)) : 'freshtv190)

and _menhir_goto_arity : _menhir_env -> 'ttv_tail -> (
# 33 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.arity)
# 291 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv181 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 298 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) = Obj.magic _menhir_stack in
    let (_v : (
# 33 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.arity)
# 303 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv179 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 309 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) = Obj.magic _menhir_stack in
    let ((_2 : (
# 33 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.arity)
# 314 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) : (
# 33 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.arity)
# 318 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) = _v in
    ((let (_menhir_stack, _menhir_s, (_1 : (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 323 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    ))) = _menhir_stack in
    let _v : (
# 31 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 328 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    ) = 
# 62 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
                      ( OperatorDef(_1, _2) )
# 332 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv177) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 31 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 340 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv175 * _menhir_state * (
# 31 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 347 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv169 * _menhir_state * (
# 31 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 357 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 31 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 373 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : (
# 31 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 378 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_BAR_operatorDef_ = 
# 241 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 383 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_separated_nonempty_list_BAR_operatorDef_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv172)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv173 * _menhir_state * (
# 31 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 393 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)) : 'freshtv176)) : 'freshtv178)) : 'freshtv180)) : 'freshtv182)

and _menhir_goto_sort : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 401 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv151 * _menhir_state) * _menhir_state * (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 411 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 425 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState9 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv147 * _menhir_state) * _menhir_state * (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 433 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 439 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 446 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ) = 
# 38 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
                                ( _2                   )
# 450 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
             in
            _menhir_goto_sort _menhir_env _menhir_stack _menhir_s _v) : 'freshtv148)) : 'freshtv150)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9) : 'freshtv152)
    | MenhirState20 | MenhirState14 | MenhirState11 | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv155 * _menhir_state * (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 462 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) * _menhir_state * (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 466 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 480 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) * _menhir_state * (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 484 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 489 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))), _, (_2 : (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 493 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) = _menhir_stack in
            let _v : (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 498 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ) = 
# 40 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
                                ( SortAp(_1, _2) )
# 502 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 514 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) * _menhir_state * (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 518 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 532 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) * _menhir_state * (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 536 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState14 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv157 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 544 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) * _menhir_state * (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 548 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 554 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))), _, (_3 : (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 558 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (
# 34 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 565 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ) = 
# 49 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
                                   ( VariableValence (_1, _3) )
# 569 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
             in
            _menhir_goto_valence _menhir_env _menhir_stack _menhir_s _v) : 'freshtv158)) : 'freshtv160)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14) : 'freshtv162)
    | MenhirState34 | MenhirState6 | MenhirState21 | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv167 * _menhir_state * (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 581 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv163 * _menhir_state * (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 591 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 615 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 620 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) = _menhir_stack in
            let _v : 'tv_fixedValence = 
# 46 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
  ( FixedValence ([], _1) )
# 625 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 648 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
     in
    
# 52 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
                                                ( _1 )
# 653 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 33 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.arity)
# 679 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ) = 
# 60 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
    ( Arity ([], valenceList) )
# 683 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 33 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.arity)
# 713 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ) = 
# 58 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
    ( Arity (_2, _5) )
# 717 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 733 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 739 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    ))) = _menhir_stack in
    let _v : (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 744 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    ) = 
# 39 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
                                ( SortName _1    )
# 748 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
     in
    _menhir_goto_sort _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 755 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 770 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_ID_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv117 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 778 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_ID_) : 'tv_separated_nonempty_list_COMMA_ID_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 785 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_COMMA_ID_ = 
# 243 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 791 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 806 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv122)) : 'freshtv124)
    | _ ->
        _menhir_fail ()

and _menhir_reduce12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_valence__ = 
# 142 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 817 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 839 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 851 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 873 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 891 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
     in
    
# 54 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
                                    ( _1 )
# 896 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 948 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 960 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 976 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 981 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_ID_ = 
# 241 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 986 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv94)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 996 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 32 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef list)
# 1020 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    ) = let lst = 
# 232 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 1024 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
     in
    
# 64 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
                                                          ( lst )
# 1029 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv85) = _menhir_stack in
    let (_v : (
# 32 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef list)
# 1036 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv83 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1042 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    ))) = Obj.magic _menhir_stack in
    let (_v : (
# 32 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef list)
# 1047 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv81 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1053 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    ))) = Obj.magic _menhir_stack in
    let ((defs : (
# 32 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef list)
# 1058 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) : (
# 32 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef list)
# 1062 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) = _v in
    ((let (_menhir_stack, _menhir_s, (sortName : (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1067 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    ))) = _menhir_stack in
    let _2 = () in
    let _v : (
# 30 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (string * Types.sortDef)
# 1073 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    ) = 
# 67 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
                                                  ( (sortName, SortDef ([], defs)) )
# 1077 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv79) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 30 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (string * Types.sortDef)
# 1085 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv77 * _menhir_state * (
# 30 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (string * Types.sortDef)
# 1092 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 1109 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 1131 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 1161 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 30 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (string * Types.sortDef)
# 1175 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) * _menhir_state * 'tv_list_sortDef_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv51 * _menhir_state * (
# 30 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (string * Types.sortDef)
# 1181 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) * _menhir_state * 'tv_list_sortDef_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : (
# 30 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (string * Types.sortDef)
# 1186 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))), _, (xs : 'tv_list_sortDef_)) = _menhir_stack in
        let _v : 'tv_list_sortDef_ = 
# 213 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 1191 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 28 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.language)
# 1210 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            ) = 
# 70 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
                      ( Language(Belt.Map.String.fromArray (Belt.List.toArray _1)) )
# 1214 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv59) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 28 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.language)
# 1222 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv57) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 28 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.language)
# 1230 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv55) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 28 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.language)
# 1238 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) : (
# 28 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.language)
# 1242 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 1272 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 30 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (string * Types.sortDef)
# 1288 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv17 * _menhir_state * (
# 31 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 1297 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 1311 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 1324 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv27 * _menhir_state * (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 1333 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state * (
# 34 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 1342 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv31 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1351 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) * _menhir_state * (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 1355 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv33 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1364 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv35 * _menhir_state * (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 1373 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) * _menhir_state * (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 1377 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state) * _menhir_state * (
# 29 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 1386 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 1413 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
     in
    _menhir_goto_list_sortDef_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1420 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 1432 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 1447 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
             in
            _menhir_goto_option_BAR_ _menhir_env _menhir_stack _v) : 'freshtv4)) : 'freshtv6)
        | EOF | ID _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
            ((let _v : 'tv_option_BAR_ = 
# 114 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( None )
# 1456 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
             in
            _menhir_goto_option_BAR_ _menhir_env _menhir_stack _v) : 'freshtv8)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv9 * _menhir_state * (
# 1 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1466 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 1477 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
# 28 "/Users/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.language)
# 1497 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
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
  

# 1528 "/Users/joel/code/lvca-bucklescript/src/languageParser.ml"
