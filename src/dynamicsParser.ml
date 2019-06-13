
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
        ))), _, (xs0 : 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__)) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : 'tv_core_pat = let _3 =
          let xs = xs0 in
          
# 220 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 121 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
          
        in
        
# 81 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( PatternTerm (_1, _3) )
# 127 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv338)) : 'freshtv340)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv341 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 137 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 144 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 157 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 231 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 174 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 144 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 195 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 231 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 212 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 225 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_case__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv315 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 235 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_case__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv313 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 242 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_case__) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (arg : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 247 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))), _, (xs0 : 'tv_loption_separated_nonempty_list_SEMICOLON_case__)) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 256 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ) = let cases =
          let xs = xs0 in
          
# 220 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 262 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
          
        in
        
# 72 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Case (arg, Ty, cases) )
# 268 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv314)) : 'freshtv316)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv317 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 278 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 293 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_core_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv312)

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 300 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 312 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 142 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 334 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 346 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 351 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_core_pat = 
# 85 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( PatternVar (Some _1) )
# 356 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv308)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv309 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 366 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 381 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 144 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 400 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_core__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv294)) : 'freshtv296)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv299 * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 408 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_core_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv297 * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 416 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_core_) : 'tv_separated_nonempty_list_SEMICOLON_core_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 423 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_core_ = 
# 231 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 429 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 442 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv287 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 452 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv285 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 459 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core__) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (_3 : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 464 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))), _, (xs0 : 'tv_loption_separated_nonempty_list_SEMICOLON_core__)) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 473 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ) = let _5 =
          let xs = xs0 in
          
# 220 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 479 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
          
        in
        
# 62 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( CoreApp (_3, _5) )
# 485 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv286)) : 'freshtv288)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv289 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 495 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 229 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 537 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 598 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_val_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv261 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 608 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_val_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv259 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 615 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_val_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 620 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))), _, (_3 : 'tv_separated_nonempty_list_SEMICOLON_core_val_)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : 'tv_core_val = 
# 52 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( ValTm (_1, _3) )
# 627 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_core_val _menhir_env _menhir_stack _menhir_s _v) : 'freshtv260)) : 'freshtv262)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv263 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 637 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 231 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 651 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_core_val_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv268)) : 'freshtv270)
    | _ ->
        _menhir_fail ()

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 660 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 676 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv258)

and _menhir_goto_core : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 684 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv201 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 694 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv197 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 704 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 142 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 732 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 746 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)) : 'freshtv202)
    | MenhirState57 | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv209 * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 755 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv203 * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 765 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 795 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 800 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_core_ = 
# 229 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 805 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_core_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv206)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv207 * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 815 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv208)) : 'freshtv210)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv217 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 824 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv213 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 834 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 142 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 856 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 870 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv216)) : 'freshtv218)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv231 * _menhir_state * 'tv_core_pat)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 879 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv229 * _menhir_state * 'tv_core_pat)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 885 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_core_pat)), _, (_3 : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 890 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_case = 
# 77 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
                                ( (_1, _3) )
# 896 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 229 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 935 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 950 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv235 * _menhir_state)) * _menhir_state * 'tv_separated_nonempty_list_DOT_ID_)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 960 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv233 * _menhir_state)) * _menhir_state * 'tv_separated_nonempty_list_DOT_ID_)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 967 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_3 : 'tv_separated_nonempty_list_DOT_ID_)), _, (_5 : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 972 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 981 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = 
# 68 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Lam (_3, _5) )
# 985 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv234)) : 'freshtv236)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv237 * _menhir_state)) * _menhir_state * 'tv_separated_nonempty_list_DOT_ID_)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 995 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv238)) : 'freshtv240)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv247 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1004 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv243 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1014 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv241 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1021 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_3 : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1026 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1034 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = 
# 70 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Lam ([], _3) )
# 1038 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv242)) : 'freshtv244)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv245 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1048 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv246)) : 'freshtv248)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv255 * _menhir_state) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1057 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )))) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1061 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv253 * _menhir_state) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1067 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )))) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1071 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (_2 : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1076 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))), _, (_5 : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1080 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _4 = () in
        let _3 = () in
        let _1 = () in
        let _v : (
# 29 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 1088 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ) = 
# 96 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
                                                    ( (_2, _5) )
# 1092 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv251) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 29 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 1100 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv249 * _menhir_state * (
# 29 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 1107 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 1138 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 1153 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv192)) : 'freshtv194)
    | _ ->
        _menhir_fail ()

and _menhir_reduce4 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1162 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1168 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ))) = _menhir_stack in
    let _v : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1173 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ) = 
# 66 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( CoreVar _1 )
# 1177 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1184 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 229 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 1238 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 1257 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ) = 
# 64 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( CoreVal _1 )
# 1261 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 144 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 1282 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 231 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 1299 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_scope_pat_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv172)) : 'freshtv174)
    | _ ->
        _menhir_fail ()

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1308 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv165) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 9 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1318 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    )) : (
# 9 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1322 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    )) = _v in
    ((let _v : 'tv_prim = 
# 94 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
           ( PrimString  _1     )
# 1327 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 1343 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 1354 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv155 * _menhir_state) * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1361 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), (_2 : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1366 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1373 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = 
# 74 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Meaning _2 )
# 1377 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv156)) : 'freshtv158)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv159 * _menhir_state) * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1387 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 1424 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 1443 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 1470 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv145) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 7 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (Bigint.t)
# 1480 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    )) : (
# 7 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (Bigint.t)
# 1484 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    )) = _v in
    ((let _v : 'tv_prim = 
# 93 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
           ( PrimInteger _1     )
# 1489 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_prim _menhir_env _menhir_stack _menhir_s _v) : 'freshtv146)

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1496 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 1514 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 1531 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = _v in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv137 * _menhir_state) = Obj.magic _menhir_stack in
        let ((_2 : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1539 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1543 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_core_val = 
# 56 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( ValPrimop _2 )
# 1550 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 229 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 1674 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
            ((let ((_menhir_stack, _menhir_s), _, (xs0 : 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1708 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = let _3 =
              let xs = xs0 in
              
# 220 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 1714 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
              
            in
            
# 37 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( DPatternTm ("app", _3) )
# 1720 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 1735 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv115 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1745 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv113 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1752 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1757 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))), _, (xs0 : 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1764 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = let _3 =
              let xs = xs0 in
              
# 220 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 1770 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
              
            in
            
# 35 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( DPatternTm (_1, _3) )
# 1776 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv114)) : 'freshtv116)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv117 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1786 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)) : 'freshtv120)
    | _ ->
        _menhir_fail ()

and _menhir_run6 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1796 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 1809 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 1824 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)) : 'freshtv104)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_pat : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1841 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState4 | MenhirState10 | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1851 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1857 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1862 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_scope_pat = 
# 48 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( DenotationScopePat ([], _1) )
# 1867 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_scope_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv84)) : 'freshtv86)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv89 * _menhir_state * 'tv_loption_separated_nonempty_list_DOT_ID__)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1875 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv87 * _menhir_state * 'tv_loption_separated_nonempty_list_DOT_ID__)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1881 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (xs0 : 'tv_loption_separated_nonempty_list_DOT_ID__)), _, (_3 : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1886 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_scope_pat = let _1 =
          let xs = xs0 in
          
# 220 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 1894 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
          
        in
        
# 46 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( DenotationScopePat (_1, _3) )
# 1900 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_scope_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv88)) : 'freshtv90)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * _menhir_state) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1908 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_OXFORD ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv95 * _menhir_state) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1918 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQ ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv91 * _menhir_state) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1928 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 1960 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 1971 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)) : 'freshtv100)
    | _ ->
        _menhir_fail ()

and _menhir_reduce34 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1981 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1987 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ))) = _menhir_stack in
    let _v : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1992 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ) = 
# 42 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( DVar (Some _1) )
# 1996 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2003 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 142 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 2029 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 142 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 2070 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_SEMICOLON_scope_pat__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2077 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 2097 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 2122 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = 
# 98 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
                                  ( DenotationChart _1 )
# 2126 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv59) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 30 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.denotation_chart)
# 2134 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv57) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 30 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.denotation_chart)
# 2142 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv55) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 30 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.denotation_chart)
# 2150 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) : (
# 30 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.denotation_chart)
# 2154 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 2169 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) * _menhir_state * 'tv_list_dynamics_rule_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state * (
# 29 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 2175 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) * _menhir_state * 'tv_list_dynamics_rule_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : (
# 29 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 2180 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))), _, (xs : 'tv_list_dynamics_rule_)) = _menhir_stack in
        let _v : 'tv_list_dynamics_rule_ = 
# 201 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 2185 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 2201 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ) = 
# 40 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( DVar None )
# 2205 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv54)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2212 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 2230 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 2277 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 2306 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv15 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 2315 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv17 * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 2324 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv19 * _menhir_state)) * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 2333 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 2357 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 2371 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 2395 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2404 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 199 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 2423 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 2459 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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

# 233 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
  

# 2490 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
