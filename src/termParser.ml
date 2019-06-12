
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | TRUE
    | STRING of (
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 12 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
  )
    | SEMICOLON
    | RIGHT_PAREN
    | RIGHT_BRACK
    | LEFT_PAREN
    | LEFT_BRACK
    | INT of (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (Bigint.t)
# 22 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
  )
    | ID of (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
  )
    | FLOAT of (
# 2 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (float)
# 32 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 66 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope__ -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (_1 : (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 72 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ))), _, (xs0 : 'tv_loption_separated_nonempty_list_SEMICOLON_scope__)) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _v : (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 79 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = let _3 =
      let xs = xs0 in
      
# 220 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 85 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
      
    in
    
# 25 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
  ( Term (_1, _3) )
# 91 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 144 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 110 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 123 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope__) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RIGHT_PAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv97 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 133 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope__) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv98)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv99 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 144 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope__) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)) : 'freshtv102)
        | MenhirState33 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv107 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 153 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope__) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RIGHT_PAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv103 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 163 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope__) = Obj.magic _menhir_stack in
                (_menhir_reduce16 _menhir_env (Obj.magic _menhir_stack) : 'freshtv104)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv105 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 173 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 231 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 193 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_scope_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv116)) : 'freshtv118)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce18 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_term__ -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), _, (xs0 : 'tv_loption_separated_nonempty_list_SEMICOLON_term__)) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 212 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = let _2 =
      let xs = xs0 in
      
# 220 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 218 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
      
    in
    
# 29 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
  ( Sequence _2   )
# 224 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_SEMICOLON_term_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_term_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv91 * _menhir_state * (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 236 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_term_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * _menhir_state * (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 244 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_term_) : 'tv_separated_nonempty_list_SEMICOLON_term_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 251 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_term_ = 
# 231 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 257 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 144 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 272 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 287 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) * _menhir_state * 'tv_scope) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv77 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 293 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) * _menhir_state * 'tv_scope) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 298 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))), _, (_3 : 'tv_scope)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_scope = 
# 34 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
                   ( match _3 with | Scope (scope, tm) -> Scope (_1 :: scope, tm) )
# 304 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 229 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 342 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 358 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (_1 : (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 363 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
  )) ->
    let _v : (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 368 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = 
# 31 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
  ( Primitive _1  )
# 372 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 421 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState33 | MenhirState19 | MenhirState9 | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * _menhir_state * (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 431 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49 * _menhir_state * (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 437 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 442 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_scope = 
# 35 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
                   ( Scope ([], _1) )
# 447 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
         in
        _menhir_goto_scope _menhir_env _menhir_stack _menhir_s _v) : 'freshtv50)) : 'freshtv52)
    | MenhirState3 | MenhirState24 | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * _menhir_state * (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 455 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv53 * _menhir_state * (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 465 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 491 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 496 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            ))) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_term_ = 
# 229 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 501 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_term_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv56)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv57 * _menhir_state * (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 511 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)) : 'freshtv60)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * _menhir_state * (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 520 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61 * _menhir_state * (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 526 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 531 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = _menhir_stack in
        Obj.magic _1) : 'freshtv62)) : 'freshtv64)

and _menhir_reduce17 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 538 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 544 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ))) = _menhir_stack in
    let _v : (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 549 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = 
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
  ( Var _1        )
# 553 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 560 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 579 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 596 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState33 | MenhirState3 | MenhirState6 | MenhirState24 | MenhirState9 | MenhirState19 | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 607 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = _v in
        (_menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v : 'freshtv44)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv45) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 617 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = _v in
        (_menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v : 'freshtv46)

and _menhir_reduce7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _1 = () in
    let _v : (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 627 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = 
# 40 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
           ( PrimBool    true  )
# 631 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    _menhir_goto_primitive _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 638 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (_1 : (
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 643 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
  )) ->
    let _v : (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 648 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = 
# 39 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
           ( PrimString  _1    )
# 652 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    _menhir_goto_primitive _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_term__ = 
# 142 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 661 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_SEMICOLON_term__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 668 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 686 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)

and _menhir_reduce5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (Bigint.t)
# 694 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (_1 : (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (Bigint.t)
# 699 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
  )) ->
    let _v : (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 704 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = 
# 38 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
           ( PrimInteger _1    )
# 708 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    _menhir_goto_primitive _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 720 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv27 * _menhir_state * (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 729 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 743 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv33 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 752 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 779 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_reduce15 : _menhir_env -> ('ttv_tail * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 788 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
)) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 794 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ))) = _menhir_stack in
    let _3 = () in
    let _2 = () in
    let _v : (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 801 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = 
# 23 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
  ( Term (_1, []) )
# 805 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (Bigint.t)
# 837 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 846 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 858 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 890 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 906 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = 
# 41 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
           ( PrimBool    false )
# 910 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Ast.term)
# 929 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 960 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv5 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 971 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 989 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 1008 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)) : 'freshtv10)
    | INT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        let (_v : (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (Bigint.t)
# 1019 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 1055 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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

# 233 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
  

# 1071 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
