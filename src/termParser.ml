
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | TRUE
    | STRING of (
# 4 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 12 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
  )
    | SEMICOLON
    | RIGHT_PAREN
    | RIGHT_BRACK
    | LEFT_PAREN
    | LEFT_BRACK
    | INT of (
# 1 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (Bigint.t)
# 22 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
  )
    | ID of (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 27 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
  )
    | FLOAT of (
# 2 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (float)
# 32 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
  )
    | FALSE
    | EOF
    | DOT
    | COMMA
  
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
  | MenhirState33
  | MenhirState24
  | MenhirState19
  | MenhirState12
  | MenhirState9
  | MenhirState6
  | MenhirState3
  | MenhirState0

let rec _menhir_reduce16 : _menhir_env -> (('ttv_tail * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 66 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope__ -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (_1 : (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 72 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
    ))), _, (xs : 'tv_loption_separated_nonempty_list_SEMICOLON_scope__)) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _v : (
# 17 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 79 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = let _3 = 
# 232 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 83 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    
# 25 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
  ( Term (_1, _3) )
# 88 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_SEMICOLON_scope_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_scope_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState33 | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_scope_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_SEMICOLON_scope_) : 'tv_separated_nonempty_list_SEMICOLON_scope_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_scope__ = 
# 144 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x )
# 107 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_loption_separated_nonempty_list_SEMICOLON_scope__) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState9 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv101 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 120 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope__) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RIGHT_PAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv97 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 130 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope__) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv98)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv99 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 141 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope__) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)) : 'freshtv102)
        | MenhirState33 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv107 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 150 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope__) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RIGHT_PAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv103 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 160 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope__) = Obj.magic _menhir_stack in
                (_menhir_reduce16 _menhir_env (Obj.magic _menhir_stack) : 'freshtv104)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv105 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 170 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope__) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)) : 'freshtv108)
        | _ ->
            _menhir_fail ()) : 'freshtv110)) : 'freshtv112)) : 'freshtv114)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv117 * _menhir_state * 'tv_scope)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_scope_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv115 * _menhir_state * 'tv_scope)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_scope_) : 'tv_separated_nonempty_list_SEMICOLON_scope_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_scope)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_scope_ = 
# 243 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 190 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_scope_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv116)) : 'freshtv118)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce18 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_term__ -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), _, (xs : 'tv_loption_separated_nonempty_list_SEMICOLON_term__)) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : (
# 17 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 209 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = let _2 = 
# 232 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 213 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    
# 29 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
  ( Sequence _2   )
# 218 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_SEMICOLON_term_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_term_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv91 * _menhir_state * (
# 17 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 230 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_term_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * _menhir_state * (
# 17 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 238 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_term_) : 'tv_separated_nonempty_list_SEMICOLON_term_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 17 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 245 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_term_ = 
# 243 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 251 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_term_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv90)) : 'freshtv92)
    | MenhirState3 | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_term_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_SEMICOLON_term_) : 'tv_separated_nonempty_list_SEMICOLON_term_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_term__ = 
# 144 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x )
# 266 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_term__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv94)) : 'freshtv96)
    | _ ->
        _menhir_fail ()

and _menhir_goto_scope : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_scope -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv79 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 281 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) * _menhir_state * 'tv_scope) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv77 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 287 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) * _menhir_state * 'tv_scope) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 292 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        ))), _, (_3 : 'tv_scope)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_scope = 
# 34 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
                   ( match _3 with | Scope (scope, tm) -> Scope (_1 :: scope, tm) )
# 298 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
         in
        _menhir_goto_scope _menhir_env _menhir_stack _menhir_s _v) : 'freshtv78)) : 'freshtv80)
    | MenhirState33 | MenhirState19 | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv87 * _menhir_state * 'tv_scope) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv81 * _menhir_state * 'tv_scope) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FALSE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | ID _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
            | INT _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
            | LEFT_BRACK ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | STRING _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19) : 'freshtv82)
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv83 * _menhir_state * 'tv_scope) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_scope)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_scope_ = 
# 241 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 336 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_scope_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv84)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv85 * _menhir_state * 'tv_scope) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)) : 'freshtv88)
    | _ ->
        _menhir_fail ()

and _menhir_reduce19 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 18 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 352 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (_1 : (
# 18 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 357 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
  )) ->
    let _v : (
# 17 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 362 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = 
# 31 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
  ( Primitive _1  )
# 366 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_term__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMICOLON_term__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_term__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv65 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_term__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv66)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv67 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_term__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)) : 'freshtv70)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv75 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_term__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv71 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_term__) = Obj.magic _menhir_stack in
            (_menhir_reduce18 _menhir_env (Obj.magic _menhir_stack) : 'freshtv72)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv73 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_term__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)) : 'freshtv76)
    | _ ->
        _menhir_fail ()

and _menhir_goto_term : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 17 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 415 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState33 | MenhirState19 | MenhirState9 | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * _menhir_state * (
# 17 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 425 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49 * _menhir_state * (
# 17 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 431 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 17 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 436 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_scope = 
# 35 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
                   ( Scope ([], _1) )
# 441 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
         in
        _menhir_goto_scope _menhir_env _menhir_stack _menhir_s _v) : 'freshtv50)) : 'freshtv52)
    | MenhirState3 | MenhirState24 | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * _menhir_state * (
# 17 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 449 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv53 * _menhir_state * (
# 17 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 459 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FALSE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | ID _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | INT _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | LEFT_BRACK ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | STRING _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24) : 'freshtv54)
        | RIGHT_BRACK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv55 * _menhir_state * (
# 17 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 485 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : (
# 17 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 490 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
            ))) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_term_ = 
# 241 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 495 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_term_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv56)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv57 * _menhir_state * (
# 17 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 505 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)) : 'freshtv60)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * _menhir_state * (
# 17 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 514 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61 * _menhir_state * (
# 17 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 520 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 17 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 525 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = _menhir_stack in
        Obj.magic _1) : 'freshtv62)) : 'freshtv64)

and _menhir_reduce17 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 532 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 538 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
    ))) = _menhir_stack in
    let _v : (
# 17 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 543 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = 
# 27 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
  ( Var _1        )
# 547 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 554 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | INT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | LEFT_BRACK ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | RIGHT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv47 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 573 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState9 in
        ((let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_goto_primitive : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 18 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 590 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState33 | MenhirState3 | MenhirState6 | MenhirState24 | MenhirState9 | MenhirState19 | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 18 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 601 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = _v in
        (_menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v : 'freshtv44)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv45) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 18 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 611 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = _v in
        (_menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v : 'freshtv46)

and _menhir_reduce7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _1 = () in
    let _v : (
# 18 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 621 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = 
# 40 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
           ( PrimBool    true  )
# 625 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    _menhir_goto_primitive _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 4 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 632 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (_1 : (
# 4 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 637 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
  )) ->
    let _v : (
# 18 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 642 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = 
# 39 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
           ( PrimString  _1    )
# 646 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    _menhir_goto_primitive _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_term__ = 
# 142 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 655 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_SEMICOLON_term__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 662 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
    | RIGHT_BRACK | SEMICOLON ->
        _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv41 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 680 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)

and _menhir_reduce5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 1 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (Bigint.t)
# 688 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (_1 : (
# 1 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (Bigint.t)
# 693 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
  )) ->
    let _v : (
# 18 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 698 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = 
# 38 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
           ( PrimInteger _1    )
# 702 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    _menhir_goto_primitive _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 714 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv27 * _menhir_state * (
# 17 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 723 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state * 'tv_scope)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv31 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 737 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv33 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 746 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv35 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv37 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv40)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 4 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 773 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_reduce15 : _menhir_env -> ('ttv_tail * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 782 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
)) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 788 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
    ))) = _menhir_stack in
    let _3 = () in
    let _2 = () in
    let _v : (
# 17 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 795 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = 
# 23 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
  ( Term (_1, []) )
# 799 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | ID _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | INT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | LEFT_BRACK ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | RIGHT_BRACK ->
        _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 1 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (Bigint.t)
# 831 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 840 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 852 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FALSE ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | ID _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | INT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | LEFT_BRACK ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | STRING _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12) : 'freshtv22)
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
    | RIGHT_PAREN | SEMICOLON ->
        _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 884 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _1 = () in
    let _v : (
# 18 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 900 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = 
# 41 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
           ( PrimBool    false )
# 904 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    _menhir_goto_primitive _menhir_env _menhir_stack _menhir_s _v

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

and term : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 17 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 923 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
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
    let (_menhir_stack : 'freshtv19) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        (_menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv2)
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        let (_v : (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 954 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv5 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 965 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FALSE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState33
            | ID _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
            | INT _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
            | LEFT_BRACK ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState33
            | RIGHT_PAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv3 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 983 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
                ))) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState33 in
                (_menhir_reduce15 _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv4)
            | STRING _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState33
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33) : 'freshtv6)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv7 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 1002 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)) : 'freshtv10)
    | INT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        let (_v : (
# 1 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (Bigint.t)
# 1013 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = _v in
        (_menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v : 'freshtv12)
    | LEFT_BRACK ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FALSE ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | ID _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | INT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | LEFT_BRACK ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | STRING _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | RIGHT_BRACK ->
            _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3) : 'freshtv14)
    | STRING _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        let (_v : (
# 4 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 1049 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = _v in
        (_menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v : 'freshtv16)
    | TRUE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        (_menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv18)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv20))

# 269 "/Users/joel/.opam/default/lib/menhir/standard.mly"
  

# 1065 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
