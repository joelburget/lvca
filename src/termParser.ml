
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
  | MenhirState31
  | MenhirState25
  | MenhirState20
  | MenhirState12
  | MenhirState9
  | MenhirState6
  | MenhirState3
  | MenhirState0

let rec _menhir_reduce10 : _menhir_env -> (('ttv_tail * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 66 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
))) * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.scope list)
# 70 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (_1 : (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 76 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ))), _, (_3 : (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.scope list)
# 80 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ))) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _v : (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term)
# 87 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = 
# 25 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
                                          ( Term (_1, _3) )
# 91 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce12 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (
# 19 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term list)
# 98 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), _, (_2 : (
# 19 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term list)
# 104 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ))) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term)
# 111 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = 
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
                                          ( Sequence _2   )
# 115 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_subterms : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.scope list)
# 127 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv97 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 137 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.scope list)
# 141 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv93 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 151 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            ))) * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.scope list)
# 155 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv94)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv95 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 166 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            ))) * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.scope list)
# 170 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)) : 'freshtv98)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv101 * _menhir_state * 'tv_scope)) * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.scope list)
# 179 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv99 * _menhir_state * 'tv_scope)) * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.scope list)
# 185 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_scope)), _, (_3 : (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.scope list)
# 190 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.scope list)
# 196 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ) = 
# 31 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
                             ( _1 :: _3 )
# 200 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
         in
        _menhir_goto_subterms _menhir_env _menhir_stack _menhir_s _v) : 'freshtv100)) : 'freshtv102)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv107 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 208 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.scope list)
# 212 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv103 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 222 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            ))) * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.scope list)
# 226 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            )) = Obj.magic _menhir_stack in
            (_menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) : 'freshtv104)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv105 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 236 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            ))) * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.scope list)
# 240 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)) : 'freshtv108)
    | _ ->
        _menhir_fail ()

and _menhir_goto_terms : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 19 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term list)
# 250 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state) * _menhir_state * (
# 19 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term list)
# 260 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv77 * _menhir_state) * _menhir_state * (
# 19 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term list)
# 270 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv78)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv79 * _menhir_state) * _menhir_state * (
# 19 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term list)
# 281 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)) : 'freshtv82)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv85 * _menhir_state * (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term)
# 290 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) * _menhir_state * (
# 19 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term list)
# 294 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv83 * _menhir_state * (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term)
# 300 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) * _menhir_state * (
# 19 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term list)
# 304 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term)
# 309 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))), _, (_3 : (
# 19 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term list)
# 313 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 19 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term list)
# 319 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ) = 
# 39 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
                         ( _1 :: _3 )
# 323 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
         in
        _menhir_goto_terms _menhir_env _menhir_stack _menhir_s _v) : 'freshtv84)) : 'freshtv86)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv91 * _menhir_state) * _menhir_state * (
# 19 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term list)
# 331 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv87 * _menhir_state) * _menhir_state * (
# 19 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term list)
# 341 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            )) = Obj.magic _menhir_stack in
            (_menhir_reduce12 _menhir_env (Obj.magic _menhir_stack) : 'freshtv88)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv89 * _menhir_state) * _menhir_state * (
# 19 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term list)
# 351 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)) : 'freshtv92)
    | _ ->
        _menhir_fail ()

and _menhir_goto_scope : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_scope -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv67 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 367 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) * _menhir_state * 'tv_scope) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv65 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 373 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) * _menhir_state * 'tv_scope) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 378 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))), _, (_3 : 'tv_scope)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_scope = 
# 35 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
                   ( match _3 with | Scope (scope, tm) -> Scope (_1 :: scope, tm) )
# 384 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
         in
        _menhir_goto_scope _menhir_env _menhir_stack _menhir_s _v) : 'freshtv66)) : 'freshtv68)
    | MenhirState31 | MenhirState20 | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * _menhir_state * 'tv_scope) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv69 * _menhir_state * 'tv_scope) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FALSE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | ID _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
            | INT _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
            | LEFT_BRACK ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | STRING _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20) : 'freshtv70)
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv71 * _menhir_state * 'tv_scope) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_scope)) = _menhir_stack in
            let _v : (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.scope list)
# 422 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            ) = 
# 32 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
                             ( [_1]     )
# 426 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
             in
            _menhir_goto_subterms _menhir_env _menhir_stack _menhir_s _v) : 'freshtv72)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv73 * _menhir_state * 'tv_scope) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)) : 'freshtv76)
    | _ ->
        _menhir_fail ()

and _menhir_reduce13 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 442 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (_1 : (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 447 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
  )) ->
    let _v : (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term)
# 452 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = 
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
                                          ( Primitive _1  )
# 456 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_term : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term)
# 463 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState31 | MenhirState20 | MenhirState9 | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * _menhir_state * (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term)
# 473 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49 * _menhir_state * (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term)
# 479 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term)
# 484 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_scope = 
# 36 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
                   ( Scope ([], _1) )
# 489 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
         in
        _menhir_goto_scope _menhir_env _menhir_stack _menhir_s _v) : 'freshtv50)) : 'freshtv52)
    | MenhirState3 | MenhirState25 | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * _menhir_state * (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term)
# 497 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv53 * _menhir_state * (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term)
# 507 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FALSE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | ID _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | INT _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | LEFT_BRACK ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | STRING _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25) : 'freshtv54)
        | RIGHT_BRACK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv55 * _menhir_state * (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term)
# 533 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term)
# 538 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            ))) = _menhir_stack in
            let _v : (
# 19 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term list)
# 543 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            ) = 
# 40 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
                         ( [_1]     )
# 547 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
             in
            _menhir_goto_terms _menhir_env _menhir_stack _menhir_s _v) : 'freshtv56)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv57 * _menhir_state * (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term)
# 557 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)) : 'freshtv60)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * _menhir_state * (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term)
# 566 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61 * _menhir_state * (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term)
# 572 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term)
# 577 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = _menhir_stack in
        Obj.magic _1) : 'freshtv62)) : 'freshtv64)

and _menhir_reduce11 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 584 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 590 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ))) = _menhir_stack in
    let _v : (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term)
# 595 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = 
# 26 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
                                          ( Var _1        )
# 599 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 606 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 625 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState9 in
        ((let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_goto_primitive : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 642 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState31 | MenhirState3 | MenhirState6 | MenhirState25 | MenhirState9 | MenhirState20 | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 653 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = _v in
        (_menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v : 'freshtv44)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv45) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 663 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = _v in
        (_menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v : 'freshtv46)

and _menhir_reduce3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _1 = () in
    let _v : (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 673 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = 
# 45 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
           ( PrimBool    true  )
# 677 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    _menhir_goto_primitive _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 684 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (_1 : (
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 689 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
  )) ->
    let _v : (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 694 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = 
# 44 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
           ( PrimString  _1    )
# 698 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    _menhir_goto_primitive _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 705 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
    | RIGHT_BRACK | SEMICOLON ->
        _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv41 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 723 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)

and _menhir_reduce1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (Bigint.t)
# 731 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (_1 : (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (Bigint.t)
# 736 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
  )) ->
    let _v : (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 741 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = 
# 43 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
           ( PrimInteger _1    )
# 745 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
     in
    _menhir_goto_primitive _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 757 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv27 * _menhir_state * (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term)
# 766 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state * 'tv_scope)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv31 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 780 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv33 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 789 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
    _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 4 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 816 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_reduce9 : _menhir_env -> ('ttv_tail * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 825 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
)) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 831 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ))) = _menhir_stack in
    let _3 = () in
    let _2 = () in
    let _v : (
# 17 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.Abt.term)
# 838 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = 
# 24 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
                                          ( Term (_1, []) )
# 842 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (Bigint.t)
# 872 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 881 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 893 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
        _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 925 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _1 = () in
    let _v : (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
      (Types.primitive)
# 941 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
    ) = 
# 46 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
           ( PrimBool    false )
# 945 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
      (Types.Abt.term)
# 964 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
        (_menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv2)
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        let (_v : (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 995 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 1006 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FALSE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | ID _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
            | INT _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
            | LEFT_BRACK ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | RIGHT_PAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv3 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 1024 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
                ))) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState31 in
                (_menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv4)
            | STRING _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31) : 'freshtv6)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv7 * _menhir_state * (
# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.mly"
       (string)
# 1043 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
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
# 1054 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = _v in
        (_menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v : 'freshtv12)
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
# 1088 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
        )) = _v in
        (_menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v : 'freshtv16)
    | TRUE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        (_menhir_reduce3 _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv18)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv20))

# 233 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
  

# 1104 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/termParser.ml"
