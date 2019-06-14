
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | UNDERSCORE
    | STRING of (
# 9 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 12 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
  )
    | SEMICOLON
    | RIGHT_S_ARR
    | RIGHT_PAREN
    | RIGHT_OXFORD
    | LEFT_PAREN
    | LEFT_OXFORD
    | LAM
    | INT of (
# 7 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (Bigint.t)
# 24 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
  )
    | ID of (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 29 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
  )
    | HASH
    | EQ
    | EOF
    | DOT
    | DEFAULT
    | CASE
    | APP
  
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
  | MenhirState90
  | MenhirState82
  | MenhirState79
  | MenhirState76
  | MenhirState70
  | MenhirState63
  | MenhirState60
  | MenhirState57
  | MenhirState52
  | MenhirState49
  | MenhirState46
  | MenhirState43
  | MenhirState35
  | MenhirState32
  | MenhirState26
  | MenhirState18
  | MenhirState14
  | MenhirState10
  | MenhirState6
  | MenhirState4
  | MenhirState1
  | MenhirState0

# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  
  open Types.Core

# 81 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"

let rec _menhir_goto_loption_separated_nonempty_list_SEMICOLON_core_pat__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv343 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 90 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv339 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 100 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv337 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 107 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 112 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))), _, (xs : 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__)) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : 'tv_core_pat = let _3 = 
# 232 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 119 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        
# 81 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( PatternTerm (_1, _3) )
# 124 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv338)) : 'freshtv340)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv341 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 134 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv342)) : 'freshtv344)

and _menhir_goto_separated_nonempty_list_SEMICOLON_core_pat_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_core_pat_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv331) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_core_pat_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv329) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_SEMICOLON_core_pat_) : 'tv_separated_nonempty_list_SEMICOLON_core_pat_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__ = 
# 144 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( x )
# 154 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_core_pat__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv330)) : 'freshtv332)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv335 * _menhir_state * 'tv_core_pat)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_core_pat_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv333 * _menhir_state * 'tv_core_pat)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_core_pat_) : 'tv_separated_nonempty_list_SEMICOLON_core_pat_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_core_pat)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_core_pat_ = 
# 243 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 171 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_core_pat_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv334)) : 'freshtv336)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMICOLON_case_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_case_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv323) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_case_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv321) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_SEMICOLON_case_) : 'tv_separated_nonempty_list_SEMICOLON_case_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_case__ = 
# 144 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( x )
# 192 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_case__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv322)) : 'freshtv324)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv327 * _menhir_state * 'tv_case)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_case_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv325 * _menhir_state * 'tv_case)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_case_) : 'tv_separated_nonempty_list_SEMICOLON_case_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_case)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_case_ = 
# 243 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 209 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_case_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv326)) : 'freshtv328)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_case__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMICOLON_case__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (((('freshtv319 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 222 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_case__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv315 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 232 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_case__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv313 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 239 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_case__) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (arg : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 244 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))), _, (xs : 'tv_loption_separated_nonempty_list_SEMICOLON_case__)) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 253 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ) = let cases = 
# 232 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 257 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        
# 72 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Case (arg, Ty, cases) )
# 262 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv314)) : 'freshtv316)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv317 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 272 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_case__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv318)) : 'freshtv320)

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv311) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_core_pat = 
# 83 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( PatternVar None )
# 287 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_core_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv312)

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 294 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv305 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 306 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DEFAULT ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | ID _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | INT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | STRING _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | UNDERSCORE ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv303) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState63 in
            ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__ = 
# 142 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 328 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_SEMICOLON_core_pat__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv304)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63) : 'freshtv306)
    | RIGHT_PAREN | RIGHT_S_ARR | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv307 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 340 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 345 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_core_pat = 
# 85 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( PatternVar (Some _1) )
# 350 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv308)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv309 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 360 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv310)

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv301) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_core_pat = 
# 89 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( PatternDefault )
# 375 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_core_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv302)

and _menhir_goto_separated_nonempty_list_SEMICOLON_core_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_core_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv295) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_core_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv293) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_SEMICOLON_core_) : 'tv_separated_nonempty_list_SEMICOLON_core_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_core__ = 
# 144 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( x )
# 394 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_core__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv294)) : 'freshtv296)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv299 * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 402 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_core_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv297 * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 410 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_core_) : 'tv_separated_nonempty_list_SEMICOLON_core_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 417 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_core_ = 
# 243 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 423 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_core_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv298)) : 'freshtv300)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_core__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMICOLON_core__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (((('freshtv291 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 436 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv287 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 446 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv285 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 453 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core__) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (_3 : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 458 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))), _, (xs : 'tv_loption_separated_nonempty_list_SEMICOLON_core__)) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 467 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ) = let _5 = 
# 232 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 471 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        
# 62 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( CoreApp (_3, _5) )
# 476 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv286)) : 'freshtv288)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv289 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 486 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv290)) : 'freshtv292)

and _menhir_goto_core_pat : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_core_pat -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState70 | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv277 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv271 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DEFAULT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | ID _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | STRING _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | UNDERSCORE ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70) : 'freshtv272)
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv273 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_core_pat)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_core_pat_ = 
# 241 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 528 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_core_pat_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv274)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv275 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv276)) : 'freshtv278)
    | MenhirState79 | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv283 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_S_ARR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv279 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APP ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | CASE ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | HASH ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | ID _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
            | INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
            | LAM ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | LEFT_OXFORD ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | STRING _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76) : 'freshtv280)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv281 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv282)) : 'freshtv284)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMICOLON_core_val_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_core_val_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv265 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 589 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_val_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv261 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 599 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_val_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv259 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 606 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_val_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 611 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))), _, (_3 : 'tv_separated_nonempty_list_SEMICOLON_core_val_)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : 'tv_core_val = 
# 52 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( ValTm (_1, _3) )
# 618 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_core_val _menhir_env _menhir_stack _menhir_s _v) : 'freshtv260)) : 'freshtv262)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv263 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 628 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_val_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv264)) : 'freshtv266)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv269 * _menhir_state * 'tv_core_val)) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_val_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv267 * _menhir_state * 'tv_core_val)) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_val_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_core_val)), _, (xs : 'tv_separated_nonempty_list_SEMICOLON_core_val_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_core_val_ = 
# 243 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 642 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_core_val_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv268)) : 'freshtv270)
    | _ ->
        _menhir_fail ()

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 651 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_PAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv257 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 667 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv258)

and _menhir_goto_core : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 675 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv201 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 685 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv197 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 695 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APP ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | CASE ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | HASH ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | ID _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
            | INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
            | LAM ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | LEFT_OXFORD ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | STRING _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
            | RIGHT_PAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv195) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState52 in
                ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_core__ = 
# 142 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 723 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
                 in
                _menhir_goto_loption_separated_nonempty_list_SEMICOLON_core__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv196)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52) : 'freshtv198)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv199 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 737 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)) : 'freshtv202)
    | MenhirState57 | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv209 * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 746 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv203 * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 756 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APP ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | CASE ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | HASH ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | ID _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | LAM ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | LEFT_OXFORD ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | STRING _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57) : 'freshtv204)
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv205 * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 786 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 791 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_core_ = 
# 241 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 796 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_core_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv206)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv207 * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 806 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv208)) : 'freshtv210)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv217 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 815 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv213 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 825 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DEFAULT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | ID _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | STRING _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | UNDERSCORE ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | RIGHT_PAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv211) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState60 in
                ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_case__ = 
# 142 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 847 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
                 in
                _menhir_goto_loption_separated_nonempty_list_SEMICOLON_case__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv212)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60) : 'freshtv214)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv215 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 861 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv216)) : 'freshtv218)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv231 * _menhir_state * 'tv_core_pat)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 870 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv229 * _menhir_state * 'tv_core_pat)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 876 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_core_pat)), _, (_3 : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 881 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_case = 
# 77 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
                                ( (_1, _3) )
# 887 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv227) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_case) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv225 * _menhir_state * 'tv_case) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv219 * _menhir_state * 'tv_case) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DEFAULT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | ID _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | STRING _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | UNDERSCORE ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79) : 'freshtv220)
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv221 * _menhir_state * 'tv_case) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_case)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_case_ = 
# 241 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 926 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_case_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv222)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv223 * _menhir_state * 'tv_case) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv224)) : 'freshtv226)) : 'freshtv228)) : 'freshtv230)) : 'freshtv232)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv239 * _menhir_state)) * _menhir_state * 'tv_separated_nonempty_list_DOT_ID_)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 941 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv235 * _menhir_state)) * _menhir_state * 'tv_separated_nonempty_list_DOT_ID_)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 951 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv233 * _menhir_state)) * _menhir_state * 'tv_separated_nonempty_list_DOT_ID_)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 958 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_3 : 'tv_separated_nonempty_list_DOT_ID_)), _, (_5 : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 963 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 972 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = 
# 68 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Lam (_3, _5) )
# 976 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv234)) : 'freshtv236)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv237 * _menhir_state)) * _menhir_state * 'tv_separated_nonempty_list_DOT_ID_)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 986 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv238)) : 'freshtv240)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv247 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 995 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv243 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1005 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv241 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1012 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_3 : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1017 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1025 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = 
# 70 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Lam ([], _3) )
# 1029 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv242)) : 'freshtv244)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv245 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1039 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv246)) : 'freshtv248)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv255 * _menhir_state) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1048 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )))) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1052 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv253 * _menhir_state) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1058 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )))) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1062 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (_2 : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1067 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))), _, (_5 : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1071 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _4 = () in
        let _3 = () in
        let _1 = () in
        let _v : (
# 29 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 1079 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ) = 
# 96 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
                                                    ( (_2, _5) )
# 1083 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv251) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 29 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 1091 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv249 * _menhir_state * (
# 29 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 1098 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFT_OXFORD ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | EOF ->
            _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90) : 'freshtv250)) : 'freshtv252)) : 'freshtv254)) : 'freshtv256)
    | _ ->
        _menhir_fail ()

and _menhir_goto_prim : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_prim -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState26 | MenhirState32 | MenhirState82 | MenhirState76 | MenhirState46 | MenhirState57 | MenhirState52 | MenhirState49 | MenhirState43 | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv189) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_prim) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv187) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_prim) : 'tv_prim) = _v in
        ((let _v : 'tv_core_val = 
# 54 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( ValLit _1 )
# 1129 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core_val _menhir_env _menhir_stack _menhir_s _v) : 'freshtv188)) : 'freshtv190)
    | MenhirState79 | MenhirState60 | MenhirState70 | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv193) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_prim) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv191) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_prim) : 'tv_prim) = _v in
        ((let _v : 'tv_core_pat = 
# 87 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( PatternLit _1 )
# 1144 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv192)) : 'freshtv194)
    | _ ->
        _menhir_fail ()

and _menhir_reduce4 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1153 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1159 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ))) = _menhir_stack in
    let _v : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1164 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ) = 
# 66 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( CoreVar _1 )
# 1168 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1175 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | HASH ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | ID _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | STRING _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_goto_core_val : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_core_val -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState43 | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv181 * _menhir_state * 'tv_core_val) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv175 * _menhir_state * 'tv_core_val) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | HASH ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | ID _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
            | INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
            | STRING _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43) : 'freshtv176)
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv177 * _menhir_state * 'tv_core_val) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_core_val)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_core_val_ = 
# 241 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 1229 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_core_val_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv178)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv179 * _menhir_state * 'tv_core_val) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)) : 'freshtv182)
    | MenhirState26 | MenhirState32 | MenhirState82 | MenhirState76 | MenhirState46 | MenhirState57 | MenhirState52 | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv185 * _menhir_state * 'tv_core_val) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv183 * _menhir_state * 'tv_core_val) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_core_val)) = _menhir_stack in
        let _v : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1248 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ) = 
# 64 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( CoreVal _1 )
# 1252 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv184)) : 'freshtv186)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMICOLON_scope_pat_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_scope_pat_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState4 | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv169) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_scope_pat_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv167) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_SEMICOLON_scope_pat_) : 'tv_separated_nonempty_list_SEMICOLON_scope_pat_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__ = 
# 144 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( x )
# 1273 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_scope_pat__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv168)) : 'freshtv170)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv173 * _menhir_state * 'tv_scope_pat)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_scope_pat_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv171 * _menhir_state * 'tv_scope_pat)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_scope_pat_) : 'tv_separated_nonempty_list_SEMICOLON_scope_pat_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_scope_pat)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_scope_pat_ = 
# 243 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 1290 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_scope_pat_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv172)) : 'freshtv174)
    | _ ->
        _menhir_fail ()

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1299 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv165) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 9 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1309 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    )) : (
# 9 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1313 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    )) = _v in
    ((let _v : 'tv_prim = 
# 94 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
           ( PrimString  _1     )
# 1318 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_prim _menhir_env _menhir_stack _menhir_s _v) : 'freshtv166)

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv161 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1334 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_OXFORD ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv157 * _menhir_state) * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1345 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv155 * _menhir_state) * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1352 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), (_2 : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1357 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1364 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = 
# 74 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Meaning _2 )
# 1368 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv156)) : 'freshtv158)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv159 * _menhir_state) * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1378 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)) : 'freshtv162)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv163 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv151 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | APP ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | CASE ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | HASH ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv149) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState32 in
            let (_v : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1415 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DOT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
            | LEFT_PAREN ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
            | RIGHT_PAREN ->
                _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv147 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1434 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
                )) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)) : 'freshtv150)
        | INT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
        | LAM ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | LEFT_OXFORD ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | STRING _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32) : 'freshtv152)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv153 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (Bigint.t)
# 1461 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv145) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 7 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (Bigint.t)
# 1471 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    )) : (
# 7 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (Bigint.t)
# 1475 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    )) = _v in
    ((let _v : 'tv_prim = 
# 93 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
           ( PrimInteger _1     )
# 1480 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_prim _menhir_env _menhir_stack _menhir_s _v) : 'freshtv146)

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1487 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_PAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
    | EOF | LEFT_OXFORD | RIGHT_PAREN | SEMICOLON ->
        _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv143 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1505 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv139 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1522 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = _v in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv137 * _menhir_state) = Obj.magic _menhir_stack in
        let ((_2 : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1530 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1534 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_core_val = 
# 56 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( ValPrimop _2 )
# 1541 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core_val _menhir_env _menhir_stack _menhir_s _v) : 'freshtv138)) : 'freshtv140)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv141 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv133 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | APP ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | CASE ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | HASH ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | ID _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
        | INT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
        | LAM ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | LEFT_OXFORD ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | STRING _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46) : 'freshtv134)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv135 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv129 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | APP ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | CASE ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | HASH ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | ID _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | INT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | LAM ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LEFT_OXFORD ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | STRING _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49) : 'freshtv130)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv131 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)

and _menhir_goto_scope_pat : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_scope_pat -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv127 * _menhir_state * 'tv_scope_pat) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv121 * _menhir_state * 'tv_scope_pat) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | APP ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | ID _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | UNDERSCORE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | DOT ->
            _menhir_reduce21 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14) : 'freshtv122)
    | RIGHT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv123 * _menhir_state * 'tv_scope_pat) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_scope_pat)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_scope_pat_ = 
# 241 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 1665 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_scope_pat_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv124)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv125 * _menhir_state * 'tv_scope_pat) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)) : 'freshtv128)

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_scope_pat__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv111 * _menhir_state)) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv107 * _menhir_state)) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv105 * _menhir_state)) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (xs : 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1699 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = let _3 = 
# 232 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 1703 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            
# 37 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( DPatternTm ("app", _3) )
# 1708 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv106)) : 'freshtv108)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv109 * _menhir_state)) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)) : 'freshtv112)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv119 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1723 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv115 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1733 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv113 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1740 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1745 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1752 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = let _3 = 
# 232 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 1756 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            
# 35 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( DPatternTm (_1, _3) )
# 1761 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv114)) : 'freshtv116)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv117 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1771 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)) : 'freshtv120)
    | _ ->
        _menhir_fail ()

and _menhir_run6 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1781 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv103) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState6 in
        let (_v : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1794 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
            let (_menhir_stack : 'freshtv101 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1809 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)) : 'freshtv104)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_pat : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1826 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState4 | MenhirState10 | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1836 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1842 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1847 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_scope_pat = 
# 48 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( DenotationScopePat ([], _1) )
# 1852 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_scope_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv84)) : 'freshtv86)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv89 * _menhir_state * 'tv_loption_separated_nonempty_list_DOT_ID__)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1860 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv87 * _menhir_state * 'tv_loption_separated_nonempty_list_DOT_ID__)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1866 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (xs : 'tv_loption_separated_nonempty_list_DOT_ID__)), _, (_3 : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1871 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_scope_pat = let _1 = 
# 232 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 1877 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        
# 46 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( DenotationScopePat (_1, _3) )
# 1882 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_scope_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv88)) : 'freshtv90)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * _menhir_state) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1890 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_OXFORD ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv95 * _menhir_state) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1900 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQ ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv91 * _menhir_state) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1910 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | APP ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | CASE ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | HASH ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | ID _v ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
                | INT _v ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
                | LAM ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | LEFT_OXFORD ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | STRING _v ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26) : 'freshtv92)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv93 * _menhir_state) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1942 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)) : 'freshtv96)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv97 * _menhir_state) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1953 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)) : 'freshtv100)
    | _ ->
        _menhir_fail ()

and _menhir_reduce34 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1963 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1969 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ))) = _menhir_stack in
    let _v : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1974 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ) = 
# 42 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( DVar (Some _1) )
# 1978 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1985 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | APP ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | ID _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | UNDERSCORE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | RIGHT_PAREN ->
        _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | DOT ->
        _menhir_reduce21 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_reduce21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_DOT_ID__ = 
# 142 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 2011 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
        | APP ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | UNDERSCORE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18) : 'freshtv76)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv77 * _menhir_state * 'tv_loption_separated_nonempty_list_DOT_ID__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)) : 'freshtv80)) : 'freshtv82)

and _menhir_reduce29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__ = 
# 142 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 2052 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_SEMICOLON_scope_pat__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2059 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
        _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2079 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)

and _menhir_goto_list_dynamics_rule_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_dynamics_rule_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv67 * _menhir_state * 'tv_list_dynamics_rule_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv63 * _menhir_state * 'tv_list_dynamics_rule_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv61 * _menhir_state * 'tv_list_dynamics_rule_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_list_dynamics_rule_)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 30 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.denotation_chart)
# 2104 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = 
# 98 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
                                  ( DenotationChart _1 )
# 2108 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv59) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 30 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.denotation_chart)
# 2116 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv57) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 30 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.denotation_chart)
# 2124 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv55) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 30 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.denotation_chart)
# 2132 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) : (
# 30 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.denotation_chart)
# 2136 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv56)) : 'freshtv58)) : 'freshtv60)) : 'freshtv62)) : 'freshtv64)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv65 * _menhir_state * 'tv_list_dynamics_rule_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)) : 'freshtv68)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state * (
# 29 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 2151 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) * _menhir_state * 'tv_list_dynamics_rule_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state * (
# 29 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 2157 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) * _menhir_state * 'tv_list_dynamics_rule_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : (
# 29 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 2162 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))), _, (xs : 'tv_list_dynamics_rule_)) = _menhir_stack in
        let _v : 'tv_list_dynamics_rule_ = 
# 213 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 2167 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_list_dynamics_rule_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv70)) : 'freshtv72)
    | _ ->
        _menhir_fail ()

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv53) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 2183 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ) = 
# 40 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( DVar None )
# 2187 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv54)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2194 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_PAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack)
    | RIGHT_OXFORD | RIGHT_PAREN | SEMICOLON ->
        _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2212 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv47 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | APP ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | ID _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | UNDERSCORE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | RIGHT_PAREN ->
            _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | DOT ->
            _menhir_reduce21 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10) : 'freshtv48)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3 * _menhir_state * (
# 29 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 2259 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv4)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv5 * _menhir_state)) * _menhir_state * 'tv_separated_nonempty_list_DOT_ID_)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv6)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv7 * _menhir_state * 'tv_case)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv9 * _menhir_state * 'tv_core_pat)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv11 * _menhir_state * 'tv_core_pat)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv13 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2288 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv15 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 2297 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv17 * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 2306 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv19 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 2315 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv21 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv23 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state * 'tv_core_val)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv27 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2339 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv31 * _menhir_state) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 2353 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv33 * _menhir_state * 'tv_loption_separated_nonempty_list_DOT_ID__)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv35 * _menhir_state * 'tv_scope_pat)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv39 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2377 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2386 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv45) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv46)

and _menhir_reduce19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_dynamics_rule_ = 
# 211 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 2405 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_list_dynamics_rule_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | APP ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | UNDERSCORE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

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

and dynamics : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 30 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.denotation_chart)
# 2441 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
    | LEFT_OXFORD ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 269 "/home/joel/.opam/default/lib/menhir/standard.mly"
  

# 2472 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
