
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | SEMICOLON
    | RULE_NAME of (
# 9 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 12 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
  )
    | RIGHT_PAREN
    | RIGHT_D_ARR
    | LINE
    | LEFT_PAREN
    | LEFT_D_ARR
    | ID of (
# 4 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 22 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
  )
    | EOF
    | DOT
    | CTX_SEPARATOR
    | CTX
  
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
  | MenhirState38
  | MenhirState34
  | MenhirState32
  | MenhirState29
  | MenhirState24
  | MenhirState22
  | MenhirState16
  | MenhirState13
  | MenhirState6
  | MenhirState4
  | MenhirState2
  | MenhirState0

let rec _menhir_goto_separated_nonempty_list_SEMICOLON_scope_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_scope_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv163) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_scope_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv161) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_SEMICOLON_scope_) : 'tv_separated_nonempty_list_SEMICOLON_scope_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_scope__ = 
# 144 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x )
# 72 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_scope__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv162)) : 'freshtv164)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv167 * _menhir_state * (
# 18 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.scope)
# 80 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_scope_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv165 * _menhir_state * (
# 18 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.scope)
# 88 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_scope_) : 'tv_separated_nonempty_list_SEMICOLON_scope_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 18 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.scope)
# 95 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_scope_ = 
# 243 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 101 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_scope_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv166)) : 'freshtv168)
    | _ ->
        _menhir_fail ()

and _menhir_goto_typingClause : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 21 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.typingClause)
# 110 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv159 * _menhir_state)) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 21 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.typingClause)
# 119 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv157 * _menhir_state)) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    let ((clause : (
# 21 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.typingClause)
# 127 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
    )) : (
# 21 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.typingClause)
# 131 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
    )) = _v in
    ((let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _2 = () in
    let _1 = () in
    let _v : (
# 22 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.hypothesis)
# 139 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
    ) = 
# 47 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
  ( (Types.Statics.M.empty, clause) )
# 143 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv155) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 22 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.hypothesis)
# 151 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
    )) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv151 * _menhir_state * 'tv_list_hypothesis_)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.hypothesis)
# 160 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv149 * _menhir_state * 'tv_list_hypothesis_)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.hypothesis)
# 166 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (hyps : 'tv_list_hypothesis_)), _, (conclusion : (
# 22 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.hypothesis)
# 171 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 23 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule)
# 177 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ) = 
# 51 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
  ( Types.Statics.Rule (hyps, None, conclusion) )
# 181 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv147) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 23 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule)
# 189 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv145 * _menhir_state * (
# 23 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule)
# 196 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CTX ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | EOF ->
            _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | LINE ->
            _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv146)) : 'freshtv148)) : 'freshtv150)) : 'freshtv152)
    | MenhirState0 | MenhirState34 | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv153 * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.hypothesis)
# 216 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CTX ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LINE ->
            _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34) : 'freshtv154)
    | _ ->
        _menhir_fail ()) : 'freshtv156)) : 'freshtv158)) : 'freshtv160)

and _menhir_goto_scope : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 18 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.scope)
# 235 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv143 * _menhir_state * (
# 18 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.scope)
# 243 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
    )) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv137 * _menhir_state * (
# 18 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.scope)
# 253 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | DOT ->
            _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13) : 'freshtv138)
    | RIGHT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv139 * _menhir_state * (
# 18 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.scope)
# 271 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : (
# 18 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.scope)
# 276 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_scope_ = 
# 241 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 281 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_scope_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv140)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv141 * _menhir_state * (
# 18 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.scope)
# 291 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)) : 'freshtv144)

and _menhir_run6 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 4 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 299 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv135) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState6 in
        let (_v : (
# 4 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 312 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv133 * _menhir_state * (
# 4 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 327 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)) : 'freshtv136)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_term : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 344 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState13 | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 354 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 360 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 365 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = _menhir_stack in
        let _v : (
# 18 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.scope)
# 370 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ) = 
# 35 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
                                     ( Types.Statics.Scope ([], _1) )
# 374 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        _menhir_goto_scope _menhir_env _menhir_stack _menhir_s _v) : 'freshtv84)) : 'freshtv86)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv89 * _menhir_state * 'tv_loption_separated_nonempty_list_DOT_ID__)) * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 382 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv87 * _menhir_state * 'tv_loption_separated_nonempty_list_DOT_ID__)) * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 388 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (xs : 'tv_loption_separated_nonempty_list_DOT_ID__)), _, (_3 : (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 393 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 18 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.scope)
# 399 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ) = let _1 = 
# 232 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 403 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        
# 34 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
                                     ( Types.Statics.Scope (_1, _3) )
# 408 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        _menhir_goto_scope _menhir_env _menhir_stack _menhir_s _v) : 'freshtv88)) : 'freshtv90)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97 * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 416 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFT_D_ARR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv91 * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 426 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24) : 'freshtv92)
        | RIGHT_D_ARR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv93 * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 442 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22) : 'freshtv94)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv95 * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 460 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)) : 'freshtv98)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv107 * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 469 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 473 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv105 * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 479 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 483 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 488 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))), _, (_3 : (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 492 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 19 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.inferenceRule)
# 498 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ) = 
# 39 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
                                     ( Types.Statics.InferenceRule (_1, _3) )
# 502 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv103) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 19 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.inferenceRule)
# 510 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 19 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.inferenceRule)
# 518 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv99) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : (
# 19 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.inferenceRule)
# 526 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) : (
# 19 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.inferenceRule)
# 530 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = _v in
        ((let _v : (
# 21 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.typingClause)
# 535 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ) = 
# 43 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
                  ( InferenceRule _1 )
# 539 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        _menhir_goto_typingClause _menhir_env _menhir_stack _menhir_s _v) : 'freshtv100)) : 'freshtv102)) : 'freshtv104)) : 'freshtv106)) : 'freshtv108)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv117 * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 547 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 551 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv115 * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 557 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 561 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 566 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))), _, (_3 : (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 570 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 20 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.checkingRule)
# 576 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ) = 
# 40 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
                                     ( Types.Statics.CheckingRule  (_1, _3) )
# 580 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 20 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.checkingRule)
# 588 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 20 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.checkingRule)
# 596 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : (
# 20 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.checkingRule)
# 604 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) : (
# 20 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.checkingRule)
# 608 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = _v in
        ((let _v : (
# 21 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.typingClause)
# 613 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ) = 
# 44 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
                  ( CheckingRule  _1 )
# 617 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        _menhir_goto_typingClause _menhir_env _menhir_stack _menhir_s _v) : 'freshtv110)) : 'freshtv112)) : 'freshtv114)) : 'freshtv116)) : 'freshtv118)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv131 * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 625 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv127 * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 635 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv125 * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 641 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 646 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 17 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 652 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
            ) = 
# 37 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
                   ( _1 )
# 656 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv123) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 17 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 664 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv121) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 17 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 672 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv119) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 17 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 680 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) : (
# 17 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 684 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv120)) : 'freshtv122)) : 'freshtv124)) : 'freshtv126)) : 'freshtv128)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv129 * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 694 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)) : 'freshtv132)
    | _ ->
        _menhir_fail ()

and _menhir_reduce9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_DOT_ID__ = 
# 142 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 706 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv81) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_loption_separated_nonempty_list_DOT_ID__) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv79 * _menhir_state * 'tv_loption_separated_nonempty_list_DOT_ID__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * _menhir_state * 'tv_loption_separated_nonempty_list_DOT_ID__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16) : 'freshtv76)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv77 * _menhir_state * 'tv_loption_separated_nonempty_list_DOT_ID__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)) : 'freshtv80)) : 'freshtv82)

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_scope__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMICOLON_scope__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv73 * _menhir_state * (
# 4 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 745 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv69 * _menhir_state * (
# 4 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 755 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv67 * _menhir_state * (
# 4 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 762 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope__) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 4 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 767 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))), _, (xs : 'tv_loption_separated_nonempty_list_SEMICOLON_scope__)) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 774 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ) = let _3 = 
# 232 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 778 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        
# 29 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
  ( Types.Statics.Term (_1, _3) )
# 783 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv68)) : 'freshtv70)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv71 * _menhir_state * (
# 4 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 793 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)) : 'freshtv74)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 4 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 801 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
    | LEFT_PAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack)
    | RIGHT_PAREN | SEMICOLON ->
        _menhir_reduce22 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65 * _menhir_state * (
# 4 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 821 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)

and _menhir_goto_list_hypothesis_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_hypothesis_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * _menhir_state * 'tv_list_hypothesis_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LINE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv55 * _menhir_state * 'tv_list_hypothesis_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CTX ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32) : 'freshtv56)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv57 * _menhir_state * 'tv_list_hypothesis_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)) : 'freshtv60)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv63 * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.hypothesis)
# 860 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) * _menhir_state * 'tv_list_hypothesis_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.hypothesis)
# 866 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) * _menhir_state * 'tv_list_hypothesis_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : (
# 22 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.hypothesis)
# 871 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))), _, (xs : 'tv_list_hypothesis_)) = _menhir_stack in
        let _v : 'tv_list_hypothesis_ = 
# 213 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 876 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        _menhir_goto_list_hypothesis_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv62)) : 'freshtv64)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_rule_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_rule_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv39 * _menhir_state * (
# 23 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule)
# 891 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) * _menhir_state * 'tv_list_rule_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state * (
# 23 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule)
# 897 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) * _menhir_state * 'tv_list_rule_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : (
# 23 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule)
# 902 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))), _, (xs : 'tv_list_rule_)) = _menhir_stack in
        let _v : 'tv_list_rule_ = 
# 213 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 907 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        _menhir_goto_list_rule_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv38)) : 'freshtv40)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * _menhir_state * 'tv_list_rule_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv49 * _menhir_state * 'tv_list_rule_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv47 * _menhir_state * 'tv_list_rule_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (rules : 'tv_list_rule_)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 24 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule list)
# 926 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
            ) = 
# 53 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
                              ( rules )
# 930 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv45) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 24 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule list)
# 938 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv43) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 24 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule list)
# 946 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv41) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 24 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule list)
# 954 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) : (
# 24 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule list)
# 958 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv42)) : 'freshtv44)) : 'freshtv46)) : 'freshtv48)) : 'freshtv50)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv51 * _menhir_state * 'tv_list_rule_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)) : 'freshtv54)
    | _ ->
        _menhir_fail ()

and _menhir_reduce22 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 4 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 974 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 4 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 980 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
    ))) = _menhir_stack in
    let _v : (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 985 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
    ) = 
# 31 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
  ( Types.Statics.Free _1 )
# 989 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
     in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 4 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 996 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | RIGHT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv35) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState4 in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_scope__ = 
# 142 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 1011 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_scope__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv36)
    | DOT ->
        _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_reduce5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_hypothesis_ = 
# 211 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 1026 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
     in
    _menhir_goto_list_hypothesis_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_rule_ = 
# 211 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 1035 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
     in
    _menhir_goto_list_rule_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CTX_SEPARATOR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2) : 'freshtv32)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv8)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9 * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.hypothesis)
# 1077 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv11 * _menhir_state * 'tv_list_hypothesis_)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * _menhir_state * (
# 23 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule)
# 1091 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv15 * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 1100 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv17 * _menhir_state * (
# 16 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 1109 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv19 * _menhir_state * 'tv_loption_separated_nonempty_list_DOT_ID__)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv21 * _menhir_state * (
# 18 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.scope)
# 1123 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv23 * _menhir_state * (
# 4 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 1132 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state * (
# 4 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 1141 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv27 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv29) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv30)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 4 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 1158 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_PAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack)
    | CTX | EOF | LEFT_D_ARR | LINE | RIGHT_D_ARR | RIGHT_PAREN | SEMICOLON ->
        _menhir_reduce22 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5 * _menhir_state * (
# 4 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 1176 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv6)

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

and _menhir_init : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> _menhir_env =
  fun lexer lexbuf ->
    let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and rules : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 24 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule list)
# 1206 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CTX ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LINE ->
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv4))

and term_top : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 17 "/Users/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 1229 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38) : 'freshtv2))

# 269 "/Users/joel/.opam/default/lib/menhir/standard.mly"
  

# 1248 "/Users/joel/code/lvca-bucklescript/src/staticsParser.ml"
