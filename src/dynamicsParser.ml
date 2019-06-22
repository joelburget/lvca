
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | UNDERSCORE
    | STRING of (
# 9 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 12 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
  )
    | SEMICOLON
    | RIGHT_S_ARR
    | RIGHT_PAREN
    | RIGHT_OXFORD
    | LEFT_PAREN
    | LEFT_OXFORD
    | LAM
    | INT of (
# 7 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (Bigint.t)
# 24 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
  )
    | ID of (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 29 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
  | MenhirState88
  | MenhirState80
  | MenhirState77
  | MenhirState74
  | MenhirState68
  | MenhirState61
  | MenhirState58
  | MenhirState55
  | MenhirState50
  | MenhirState47
  | MenhirState44
  | MenhirState41
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

# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  
  open Types.Core

# 81 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"

let rec _menhir_goto_loption_separated_nonempty_list_SEMICOLON_core_pat__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv337 * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 90 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv333 * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 100 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv331 * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 107 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 112 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))), _, (xs : 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__)) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : 'tv_core_pat = let _3 = 
# 232 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 119 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        
# 81 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( PatternTerm (_1, _3) )
# 124 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv332)) : 'freshtv334)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv335 * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 134 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv336)) : 'freshtv338)

and _menhir_goto_separated_nonempty_list_SEMICOLON_core_pat_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_core_pat_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv325) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_core_pat_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv323) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_SEMICOLON_core_pat_) : 'tv_separated_nonempty_list_SEMICOLON_core_pat_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__ = 
# 144 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x )
# 154 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_core_pat__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv324)) : 'freshtv326)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv329 * _menhir_state * 'tv_core_pat)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_core_pat_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv327 * _menhir_state * 'tv_core_pat)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_core_pat_) : 'tv_separated_nonempty_list_SEMICOLON_core_pat_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_core_pat)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_core_pat_ = 
# 243 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 171 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_core_pat_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv328)) : 'freshtv330)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMICOLON_core_val_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_core_val_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv317 * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 186 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_val_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv313 * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 196 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_val_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv311 * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 203 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_val_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 208 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))), _, (_3 : 'tv_separated_nonempty_list_SEMICOLON_core_val_)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : 'tv_core_val = 
# 52 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( ValTm (_1, _3) )
# 215 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_core_val _menhir_env _menhir_stack _menhir_s _v) : 'freshtv312)) : 'freshtv314)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv315 * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 225 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_val_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv316)) : 'freshtv318)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv321 * _menhir_state * 'tv_core_val)) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_val_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv319 * _menhir_state * 'tv_core_val)) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_val_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_core_val)), _, (xs : 'tv_separated_nonempty_list_SEMICOLON_core_val_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_core_val_ = 
# 243 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 239 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_core_val_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv320)) : 'freshtv322)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMICOLON_case_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_case_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv305) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_case_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv303) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_SEMICOLON_case_) : 'tv_separated_nonempty_list_SEMICOLON_case_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_case__ = 
# 144 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x )
# 260 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_case__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv304)) : 'freshtv306)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv309 * _menhir_state * 'tv_case)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_case_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv307 * _menhir_state * 'tv_case)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_case_) : 'tv_separated_nonempty_list_SEMICOLON_case_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_case)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_case_ = 
# 243 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 277 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_case_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv308)) : 'freshtv310)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_case__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMICOLON_case__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (((('freshtv301 * _menhir_state)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 290 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_case__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv297 * _menhir_state)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 300 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_case__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv295 * _menhir_state)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 307 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_case__) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (arg : (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 312 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))), _, (xs : 'tv_loption_separated_nonempty_list_SEMICOLON_case__)) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 321 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ) = let cases = 
# 232 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 325 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        
# 72 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Case (arg, Ty, cases) )
# 330 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv296)) : 'freshtv298)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv299 * _menhir_state)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 340 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_case__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv300)) : 'freshtv302)

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv293) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_core_pat = 
# 83 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( PatternVar None )
# 355 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_core_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv294)

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 362 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv287 * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 374 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DEFAULT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | ID _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | INT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | STRING _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | UNDERSCORE ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv285) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState61 in
            ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__ = 
# 142 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 396 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_SEMICOLON_core_pat__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv286)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61) : 'freshtv288)
    | RIGHT_PAREN | RIGHT_S_ARR | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv289 * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 408 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 413 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_core_pat = 
# 85 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( PatternVar (Some _1) )
# 418 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv290)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv291 * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 428 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv292)

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv283) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_core_pat = 
# 89 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( PatternDefault )
# 443 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_core_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv284)

and _menhir_goto_separated_nonempty_list_SEMICOLON_core_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_core_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv277) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_core_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv275) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_SEMICOLON_core_) : 'tv_separated_nonempty_list_SEMICOLON_core_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_core__ = 
# 144 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x )
# 462 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_core__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv276)) : 'freshtv278)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv281 * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 470 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_core_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv279 * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 478 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_core_) : 'tv_separated_nonempty_list_SEMICOLON_core_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 485 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_core_ = 
# 243 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 491 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_core_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv280)) : 'freshtv282)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_core__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMICOLON_core__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (((('freshtv273 * _menhir_state)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 504 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv269 * _menhir_state)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 514 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv267 * _menhir_state)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 521 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core__) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (_3 : (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 526 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))), _, (xs : 'tv_loption_separated_nonempty_list_SEMICOLON_core__)) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 535 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ) = let _5 = 
# 232 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 539 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        
# 66 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( CoreApp (_3, _5) )
# 544 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv268)) : 'freshtv270)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv271 * _menhir_state)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 554 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv272)) : 'freshtv274)

and _menhir_goto_core_pat : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_core_pat -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState68 | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv259 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv253 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DEFAULT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | ID _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | STRING _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | UNDERSCORE ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68) : 'freshtv254)
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv255 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_core_pat)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_core_pat_ = 
# 241 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 596 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_core_pat_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv256)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv257 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv258)) : 'freshtv260)
    | MenhirState77 | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv265 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_S_ARR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv261 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APP ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | CASE ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | ID _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
            | INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
            | LAM ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | LEFT_OXFORD ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | STRING _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74) : 'freshtv262)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv263 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv264)) : 'freshtv266)
    | _ ->
        _menhir_fail ()

and _menhir_goto_core_val : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_core_val -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState41 | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv247 * _menhir_state * 'tv_core_val) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv241 * _menhir_state * 'tv_core_val) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
            | INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
            | LAM ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | STRING _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41) : 'freshtv242)
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv243 * _menhir_state * 'tv_core_val) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_core_val)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_core_val_ = 
# 241 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 681 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_core_val_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv244)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv245 * _menhir_state * 'tv_core_val) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv246)) : 'freshtv248)
    | MenhirState26 | MenhirState32 | MenhirState80 | MenhirState74 | MenhirState44 | MenhirState55 | MenhirState50 | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv251 * _menhir_state * 'tv_core_val) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv249 * _menhir_state * 'tv_core_val) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_core_val)) = _menhir_stack in
        let _v : (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 700 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ) = 
# 68 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( CoreVal _1 )
# 704 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv250)) : 'freshtv252)
    | _ ->
        _menhir_fail ()

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 713 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
        let (_menhir_stack : 'freshtv239 * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 729 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv240)

and _menhir_goto_core : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 737 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv183 * _menhir_state)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 747 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv179 * _menhir_state)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 757 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APP ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | CASE ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | ID _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | LAM ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | LEFT_OXFORD ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | STRING _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | RIGHT_PAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv177) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState50 in
                ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_core__ = 
# 142 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 783 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
                 in
                _menhir_goto_loption_separated_nonempty_list_SEMICOLON_core__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv178)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50) : 'freshtv180)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv181 * _menhir_state)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 797 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)) : 'freshtv184)
    | MenhirState55 | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv191 * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 806 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv185 * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 816 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APP ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | CASE ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | ID _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | LAM ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | LEFT_OXFORD ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | STRING _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55) : 'freshtv186)
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv187 * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 844 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 849 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_core_ = 
# 241 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 854 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_core_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv188)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv189 * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 864 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv190)) : 'freshtv192)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv199 * _menhir_state)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 873 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv195 * _menhir_state)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 883 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DEFAULT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | ID _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | STRING _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | UNDERSCORE ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | RIGHT_PAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv193) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState58 in
                ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_case__ = 
# 142 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 905 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
                 in
                _menhir_goto_loption_separated_nonempty_list_SEMICOLON_case__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv194)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58) : 'freshtv196)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv197 * _menhir_state)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 919 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv198)) : 'freshtv200)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv213 * _menhir_state * 'tv_core_pat)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 928 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv211 * _menhir_state * 'tv_core_pat)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 934 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_core_pat)), _, (_3 : (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 939 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_case = 
# 77 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
                                ( (_1, _3) )
# 945 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv209) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_case) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv207 * _menhir_state * 'tv_case) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv201 * _menhir_state * 'tv_case) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DEFAULT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | ID _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | STRING _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | UNDERSCORE ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77) : 'freshtv202)
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv203 * _menhir_state * 'tv_case) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_case)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_case_ = 
# 241 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 984 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_case_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv204)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv205 * _menhir_state * 'tv_case) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv206)) : 'freshtv208)) : 'freshtv210)) : 'freshtv212)) : 'freshtv214)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv221 * _menhir_state)) * _menhir_state * 'tv_separated_nonempty_list_DOT_ID_)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 999 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv217 * _menhir_state)) * _menhir_state * 'tv_separated_nonempty_list_DOT_ID_)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1009 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv215 * _menhir_state)) * _menhir_state * 'tv_separated_nonempty_list_DOT_ID_)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1016 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_3 : 'tv_separated_nonempty_list_DOT_ID_)), _, (_5 : (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1021 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_core_val = 
# 56 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( ValLam (_3, _5) )
# 1030 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_core_val _menhir_env _menhir_stack _menhir_s _v) : 'freshtv216)) : 'freshtv218)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv219 * _menhir_state)) * _menhir_state * 'tv_separated_nonempty_list_DOT_ID_)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1040 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv220)) : 'freshtv222)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv229 * _menhir_state)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1049 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv225 * _menhir_state)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1059 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv223 * _menhir_state)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1066 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_3 : (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1071 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_core_val = 
# 58 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( ValLam ([], _3) )
# 1079 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_core_val _menhir_env _menhir_stack _menhir_s _v) : 'freshtv224)) : 'freshtv226)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv227 * _menhir_state)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1089 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv228)) : 'freshtv230)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv237 * _menhir_state) * _menhir_state * (
# 27 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1098 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )))) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1102 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv235 * _menhir_state) * _menhir_state * (
# 27 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1108 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )))) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1112 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (_2 : (
# 27 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1117 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))), _, (_5 : (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1121 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _4 = () in
        let _3 = () in
        let _1 = () in
        let _v : (
# 29 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 1129 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ) = 
# 96 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
                                                    ( (_2, _5) )
# 1133 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv233) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 29 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 1141 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv231 * _menhir_state * (
# 29 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 1148 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFT_OXFORD ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | EOF ->
            _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88) : 'freshtv232)) : 'freshtv234)) : 'freshtv236)) : 'freshtv238)
    | _ ->
        _menhir_fail ()

and _menhir_goto_prim : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_prim -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState26 | MenhirState32 | MenhirState80 | MenhirState74 | MenhirState44 | MenhirState55 | MenhirState50 | MenhirState47 | MenhirState41 | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv171) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_prim) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv169) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_prim) : 'tv_prim) = _v in
        ((let _v : 'tv_core_val = 
# 54 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( ValPrim _1 )
# 1179 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core_val _menhir_env _menhir_stack _menhir_s _v) : 'freshtv170)) : 'freshtv172)
    | MenhirState77 | MenhirState58 | MenhirState68 | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv175) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_prim) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv173) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_prim) : 'tv_prim) = _v in
        ((let _v : 'tv_core_pat = 
# 87 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( PatternLit _1 )
# 1194 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv174)) : 'freshtv176)
    | _ ->
        _menhir_fail ()

and _menhir_reduce4 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1203 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1209 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ))) = _menhir_stack in
    let _v : (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1214 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ) = 
# 70 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( CoreVar _1 )
# 1218 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1225 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | LAM ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | STRING _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_goto_separated_nonempty_list_SEMICOLON_scope_pat_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_scope_pat_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState4 | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv163) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_scope_pat_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv161) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_SEMICOLON_scope_pat_) : 'tv_separated_nonempty_list_SEMICOLON_scope_pat_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__ = 
# 144 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x )
# 1259 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_scope_pat__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv162)) : 'freshtv164)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv167 * _menhir_state * 'tv_scope_pat)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_scope_pat_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv165 * _menhir_state * 'tv_scope_pat)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_scope_pat_) : 'tv_separated_nonempty_list_SEMICOLON_scope_pat_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_scope_pat)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_scope_pat_ = 
# 243 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 1276 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_scope_pat_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv166)) : 'freshtv168)
    | _ ->
        _menhir_fail ()

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1285 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv159) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 9 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1295 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    )) : (
# 9 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1299 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    )) = _v in
    ((let _v : 'tv_prim = 
# 94 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
           ( PrimString  _1     )
# 1304 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_prim _menhir_env _menhir_stack _menhir_s _v) : 'freshtv160)

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv155 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1320 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_OXFORD ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv151 * _menhir_state) * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1331 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv149 * _menhir_state) * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1338 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), (_2 : (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1343 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1350 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = 
# 74 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Meaning _2 )
# 1354 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv150)) : 'freshtv152)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv153 * _menhir_state) * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1364 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)) : 'freshtv156)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv157 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv145 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | APP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | CASE ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv143) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState32 in
            let (_v : (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1399 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
                let (_menhir_stack : 'freshtv141 * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1418 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
                )) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)) : 'freshtv144)
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32) : 'freshtv146)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv147 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (Bigint.t)
# 1445 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv139) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 7 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (Bigint.t)
# 1455 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    )) : (
# 7 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (Bigint.t)
# 1459 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    )) = _v in
    ((let _v : 'tv_prim = 
# 93 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
           ( PrimInteger _1     )
# 1464 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_prim _menhir_env _menhir_stack _menhir_s _v) : 'freshtv140)

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1471 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
        let (_menhir_stack : 'freshtv137 * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1489 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | CASE ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | ID _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | INT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | LAM ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | LEFT_OXFORD ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | STRING _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44) : 'freshtv134)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv135 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | CASE ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | ID _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | INT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | LAM ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | LEFT_OXFORD ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | STRING _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47) : 'freshtv130)
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
            _menhir_reduce20 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14) : 'freshtv122)
    | RIGHT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv123 * _menhir_state * 'tv_scope_pat) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_scope_pat)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_scope_pat_ = 
# 241 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 1603 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 27 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1637 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = let _3 = 
# 232 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 1641 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            
# 37 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( DPatternTm ("app", _3) )
# 1646 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1661 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv115 * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1671 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv113 * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1678 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1683 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (
# 27 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1690 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = let _3 = 
# 232 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 1694 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            
# 35 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( DPatternTm (_1, _3) )
# 1699 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv114)) : 'freshtv116)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv117 * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1709 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)) : 'freshtv120)
    | _ ->
        _menhir_fail ()

and _menhir_run6 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1719 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1732 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1747 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 27 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1764 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState4 | MenhirState10 | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * _menhir_state * (
# 27 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1774 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * _menhir_state * (
# 27 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1780 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 27 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1785 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_scope_pat = 
# 48 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( DenotationScopePat ([], _1) )
# 1790 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_scope_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv84)) : 'freshtv86)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv89 * _menhir_state * 'tv_loption_separated_nonempty_list_DOT_ID__)) * _menhir_state * (
# 27 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1798 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv87 * _menhir_state * 'tv_loption_separated_nonempty_list_DOT_ID__)) * _menhir_state * (
# 27 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1804 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (xs : 'tv_loption_separated_nonempty_list_DOT_ID__)), _, (_3 : (
# 27 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1809 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_scope_pat = let _1 = 
# 232 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 1815 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        
# 46 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( DenotationScopePat (_1, _3) )
# 1820 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_scope_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv88)) : 'freshtv90)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * _menhir_state) * _menhir_state * (
# 27 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1828 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_OXFORD ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv95 * _menhir_state) * _menhir_state * (
# 27 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1838 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQ ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv91 * _menhir_state) * _menhir_state * (
# 27 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1848 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | APP ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | CASE ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | ID _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
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
# 27 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1878 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)) : 'freshtv96)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv97 * _menhir_state) * _menhir_state * (
# 27 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1889 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)) : 'freshtv100)
    | _ ->
        _menhir_fail ()

and _menhir_reduce33 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1899 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1905 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ))) = _menhir_stack in
    let _v : (
# 27 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1910 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ) = 
# 42 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( DVar (Some _1) )
# 1914 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1921 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
        _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | DOT ->
        _menhir_reduce20 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_reduce20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_DOT_ID__ = 
# 142 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 1947 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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

and _menhir_reduce28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__ = 
# 142 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 1988 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_SEMICOLON_scope_pat__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1995 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
        _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2015 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 30 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.denotation_chart)
# 2040 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = 
# 98 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
                                  ( DenotationChart _1 )
# 2044 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv59) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 30 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.denotation_chart)
# 2052 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv57) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 30 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.denotation_chart)
# 2060 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv55) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 30 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.denotation_chart)
# 2068 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) : (
# 30 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.denotation_chart)
# 2072 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv56)) : 'freshtv58)) : 'freshtv60)) : 'freshtv62)) : 'freshtv64)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv65 * _menhir_state * 'tv_list_dynamics_rule_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)) : 'freshtv68)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state * (
# 29 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 2087 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) * _menhir_state * 'tv_list_dynamics_rule_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state * (
# 29 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 2093 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) * _menhir_state * 'tv_list_dynamics_rule_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : (
# 29 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 2098 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))), _, (xs : 'tv_list_dynamics_rule_)) = _menhir_stack in
        let _v : 'tv_list_dynamics_rule_ = 
# 213 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 2103 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 27 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 2119 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ) = 
# 40 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( DVar None )
# 2123 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv54)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2130 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_PAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack)
    | RIGHT_OXFORD | RIGHT_PAREN | SEMICOLON ->
        _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2148 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
            _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | DOT ->
            _menhir_reduce20 _menhir_env (Obj.magic _menhir_stack) MenhirState10
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
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3 * _menhir_state * (
# 29 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 2195 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv4)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv5 * _menhir_state)) * _menhir_state * 'tv_separated_nonempty_list_DOT_ID_)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv6)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv7 * _menhir_state * 'tv_case)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv9 * _menhir_state * 'tv_core_pat)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv11 * _menhir_state * 'tv_core_pat)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv13 * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2224 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv15 * _menhir_state)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 2233 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv17 * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 2242 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv19 * _menhir_state)) * _menhir_state * (
# 28 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 2251 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv21 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv23 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state * 'tv_core_val)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv27 * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2275 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 27 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 2289 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2313 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state * (
# 8 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2322 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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

and _menhir_reduce18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_dynamics_rule_ = 
# 211 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 2341 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 30 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.denotation_chart)
# 2377 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
        _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 269 "/Users/joel/.opam/default/lib/menhir/standard.mly"
  

# 2408 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
