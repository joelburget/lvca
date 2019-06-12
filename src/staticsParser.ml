
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | SEMICOLON
    | RULE_NAME of (
# 9 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 12 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
  )
    | RIGHT_PAREN
    | RIGHT_D_ARR
    | LINE
    | LEFT_PAREN
    | LEFT_D_ARR
    | ID of (
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 22 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
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
# 144 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 72 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_scope__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv162)) : 'freshtv164)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv167 * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.scope)
# 80 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_scope_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv165 * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.scope)
# 88 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_scope_) : 'tv_separated_nonempty_list_SEMICOLON_scope_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.scope)
# 95 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_scope_ = 
# 231 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 101 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_scope_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv166)) : 'freshtv168)
    | _ ->
        _menhir_fail ()

and _menhir_goto_typingClause : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 21 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.typingClause)
# 110 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv159 * _menhir_state)) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 21 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.typingClause)
# 119 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv157 * _menhir_state)) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    let ((clause : (
# 21 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.typingClause)
# 127 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
    )) : (
# 21 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.typingClause)
# 131 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
    )) = _v in
    ((let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _2 = () in
    let _1 = () in
    let _v : (
# 22 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.hypothesis)
# 139 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
    ) = 
# 47 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
  ( (Types.Statics.M.empty, clause) )
# 143 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv155) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 22 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.hypothesis)
# 151 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
    )) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv151 * _menhir_state * 'tv_list_hypothesis_)) * _menhir_state * (
# 22 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.hypothesis)
# 160 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv149 * _menhir_state * 'tv_list_hypothesis_)) * _menhir_state * (
# 22 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.hypothesis)
# 166 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (hyps : 'tv_list_hypothesis_)), _, (conclusion : (
# 22 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.hypothesis)
# 171 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 23 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule)
# 177 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ) = 
# 51 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
  ( Types.Statics.Rule (hyps, None, conclusion) )
# 181 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv147) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 23 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule)
# 189 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv145 * _menhir_state * (
# 23 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule)
# 196 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
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
# 22 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.hypothesis)
# 216 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
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
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.scope)
# 235 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv143 * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.scope)
# 243 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
    )) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv137 * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.scope)
# 253 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
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
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.scope)
# 271 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.scope)
# 276 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_scope_ = 
# 229 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 281 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_scope_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv140)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv141 * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.scope)
# 291 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)) : 'freshtv144)

and _menhir_run6 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 299 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
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
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 312 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
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
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 327 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)) : 'freshtv136)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_term : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 344 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState13 | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 354 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 360 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 365 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = _menhir_stack in
        let _v : (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.scope)
# 370 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ) = 
# 35 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
                                     ( Types.Statics.Scope ([], _1) )
# 374 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        _menhir_goto_scope _menhir_env _menhir_stack _menhir_s _v) : 'freshtv84)) : 'freshtv86)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv89 * _menhir_state * 'tv_loption_separated_nonempty_list_DOT_ID__)) * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 382 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv87 * _menhir_state * 'tv_loption_separated_nonempty_list_DOT_ID__)) * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 388 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (xs0 : 'tv_loption_separated_nonempty_list_DOT_ID__)), _, (_3 : (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 393 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.scope)
# 399 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ) = let _1 =
          let xs = xs0 in
          
# 220 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 405 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
          
        in
        
# 34 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
                                     ( Types.Statics.Scope (_1, _3) )
# 411 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        _menhir_goto_scope _menhir_env _menhir_stack _menhir_s _v) : 'freshtv88)) : 'freshtv90)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97 * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 419 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFT_D_ARR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv91 * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 429 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
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
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 445 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
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
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 463 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)) : 'freshtv98)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv107 * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 472 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 476 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv105 * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 482 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 486 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 491 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))), _, (_3 : (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 495 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 19 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.inferenceRule)
# 501 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ) = 
# 39 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
                                     ( Types.Statics.InferenceRule (_1, _3) )
# 505 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv103) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 19 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.inferenceRule)
# 513 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 19 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.inferenceRule)
# 521 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv99) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : (
# 19 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.inferenceRule)
# 529 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) : (
# 19 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.inferenceRule)
# 533 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = _v in
        ((let _v : (
# 21 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.typingClause)
# 538 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ) = 
# 43 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
                  ( InferenceRule _1 )
# 542 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        _menhir_goto_typingClause _menhir_env _menhir_stack _menhir_s _v) : 'freshtv100)) : 'freshtv102)) : 'freshtv104)) : 'freshtv106)) : 'freshtv108)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv117 * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 550 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 554 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv115 * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 560 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 564 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 569 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))), _, (_3 : (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 573 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.checkingRule)
# 579 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ) = 
# 40 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
                                     ( Types.Statics.CheckingRule  (_1, _3) )
# 583 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.checkingRule)
# 591 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.checkingRule)
# 599 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.checkingRule)
# 607 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) : (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.checkingRule)
# 611 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = _v in
        ((let _v : (
# 21 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.typingClause)
# 616 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ) = 
# 44 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
                  ( CheckingRule  _1 )
# 620 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        _menhir_goto_typingClause _menhir_env _menhir_stack _menhir_s _v) : 'freshtv110)) : 'freshtv112)) : 'freshtv114)) : 'freshtv116)) : 'freshtv118)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv131 * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 628 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv127 * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 638 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv125 * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 644 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 649 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 655 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
            ) = 
# 37 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
                   ( _1 )
# 659 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv123) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 667 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv121) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 675 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv119) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 683 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) : (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 687 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv120)) : 'freshtv122)) : 'freshtv124)) : 'freshtv126)) : 'freshtv128)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv129 * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 697 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)) : 'freshtv132)
    | _ ->
        _menhir_fail ()

and _menhir_reduce9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_DOT_ID__ = 
# 142 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 709 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
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
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 748 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv69 * _menhir_state * (
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 758 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv67 * _menhir_state * (
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 765 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope__) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 770 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))), _, (xs0 : 'tv_loption_separated_nonempty_list_SEMICOLON_scope__)) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 777 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ) = let _3 =
          let xs = xs0 in
          
# 220 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 783 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
          
        in
        
# 29 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
  ( Types.Statics.Term (_1, _3) )
# 789 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
         in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv68)) : 'freshtv70)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv71 * _menhir_state * (
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 799 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)) : 'freshtv74)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 807 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
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
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 827 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
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
# 22 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.hypothesis)
# 866 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) * _menhir_state * 'tv_list_hypothesis_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state * (
# 22 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.hypothesis)
# 872 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) * _menhir_state * 'tv_list_hypothesis_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : (
# 22 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.hypothesis)
# 877 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))), _, (xs : 'tv_list_hypothesis_)) = _menhir_stack in
        let _v : 'tv_list_hypothesis_ = 
# 201 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 882 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
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
# 23 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule)
# 897 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) * _menhir_state * 'tv_list_rule_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state * (
# 23 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule)
# 903 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) * _menhir_state * 'tv_list_rule_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : (
# 23 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule)
# 908 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))), _, (xs : 'tv_list_rule_)) = _menhir_stack in
        let _v : 'tv_list_rule_ = 
# 201 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 913 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
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
# 24 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule list)
# 932 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
            ) = 
# 53 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
                              ( rules )
# 936 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv45) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 24 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule list)
# 944 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv43) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 24 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule list)
# 952 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv41) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 24 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule list)
# 960 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
            )) : (
# 24 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule list)
# 964 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
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
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 980 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 986 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
    ))) = _menhir_stack in
    let _v : (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 991 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
    ) = 
# 31 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
  ( Types.Statics.Free _1 )
# 995 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
     in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 1002 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
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
# 142 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 1017 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
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
# 199 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 1032 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
     in
    _menhir_goto_list_hypothesis_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_rule_ = 
# 199 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 1041 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
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
# 22 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.hypothesis)
# 1083 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
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
# 23 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule)
# 1097 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv15 * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 1106 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv17 * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 1115 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
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
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.scope)
# 1129 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv23 * _menhir_state * (
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 1138 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state * (
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 1147 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
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
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 1164 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
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
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
       (string)
# 1182 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
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
# 24 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.rule list)
# 1212 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
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
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.mly"
      (Types.Statics.term)
# 1235 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
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

# 233 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
  

# 1254 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/staticsParser.ml"
