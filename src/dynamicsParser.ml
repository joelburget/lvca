
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
  | MenhirState54
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

# 3 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  
  open Types.Core

# 80 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"

let rec _menhir_goto_loption_separated_nonempty_list_SEMICOLON_core_pat__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv335 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 89 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv331 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 99 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv329 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 106 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 111 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))), _, (xs0 : 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__)) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : 'tv_core_pat = let _3 =
          let xs = xs0 in
          
# 220 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 120 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
          
        in
        
# 81 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( PatternTerm (_1, _3) )
# 126 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv330)) : 'freshtv332)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv333 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 136 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv334)) : 'freshtv336)

and _menhir_goto_separated_nonempty_list_SEMICOLON_core_pat_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_core_pat_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv323) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_core_pat_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv321) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_SEMICOLON_core_pat_) : 'tv_separated_nonempty_list_SEMICOLON_core_pat_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__ = 
# 144 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 156 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_core_pat__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv322)) : 'freshtv324)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv327 * _menhir_state * 'tv_core_pat)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_core_pat_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv325 * _menhir_state * 'tv_core_pat)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_core_pat_) : 'tv_separated_nonempty_list_SEMICOLON_core_pat_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_core_pat)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_core_pat_ = 
# 231 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 173 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_core_pat_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv326)) : 'freshtv328)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMICOLON_core_val_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_core_val_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv315 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 188 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_val_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv311 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 198 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_val_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv309 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 205 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_val_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 210 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))), _, (_3 : 'tv_separated_nonempty_list_SEMICOLON_core_val_)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : 'tv_core_val = 
# 51 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( ValTm (_1, _3) )
# 217 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_core_val _menhir_env _menhir_stack _menhir_s _v) : 'freshtv310)) : 'freshtv312)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv313 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 227 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_val_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv314)) : 'freshtv316)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv319 * _menhir_state * 'tv_core_val)) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_val_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv317 * _menhir_state * 'tv_core_val)) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_val_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_core_val)), _, (xs : 'tv_separated_nonempty_list_SEMICOLON_core_val_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_core_val_ = 
# 231 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 241 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_core_val_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv318)) : 'freshtv320)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMICOLON_case_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_case_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv303) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_case_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv301) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_SEMICOLON_case_) : 'tv_separated_nonempty_list_SEMICOLON_case_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_case__ = 
# 144 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 262 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_case__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv302)) : 'freshtv304)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv307 * _menhir_state * 'tv_case)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_case_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv305 * _menhir_state * 'tv_case)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_case_) : 'tv_separated_nonempty_list_SEMICOLON_case_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_case)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_case_ = 
# 231 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 279 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_case_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv306)) : 'freshtv308)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_case__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMICOLON_case__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (((('freshtv299 * _menhir_state)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 292 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_case__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv295 * _menhir_state)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 302 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_case__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv293 * _menhir_state)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 309 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_case__) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (arg : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 314 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))), _, (xs0 : 'tv_loption_separated_nonempty_list_SEMICOLON_case__)) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 323 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ) = let cases =
          let xs = xs0 in
          
# 220 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 329 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
          
        in
        
# 72 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Case (arg, Ty, cases) )
# 335 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv294)) : 'freshtv296)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv297 * _menhir_state)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 345 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_case__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv298)) : 'freshtv300)

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv291) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_core_pat = 
# 83 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( PatternVar None )
# 360 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_core_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv292)

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 367 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv285 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 379 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
            let (_menhir_stack : 'freshtv283) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState61 in
            ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__ = 
# 142 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 401 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_SEMICOLON_core_pat__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv284)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61) : 'freshtv286)
    | RIGHT_PAREN | RIGHT_S_ARR | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv287 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 413 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 418 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_core_pat = 
# 85 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( PatternVar (Some _1) )
# 423 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv288)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv289 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 433 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv290)

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv281) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_core_pat = 
# 89 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( PatternDefault )
# 448 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_core_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv282)

and _menhir_goto_separated_nonempty_list_SEMICOLON_core_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_core_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv275 * _menhir_state)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 461 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv271 * _menhir_state)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 471 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv269 * _menhir_state)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 478 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_3 : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 483 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))), _, (_5 : 'tv_separated_nonempty_list_SEMICOLON_core_)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 492 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = 
# 62 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( CoreApp (_3, _5) )
# 496 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv270)) : 'freshtv272)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv273 * _menhir_state)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 506 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv274)) : 'freshtv276)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv279 * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 515 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv277 * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 521 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 526 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))), _, (xs : 'tv_separated_nonempty_list_SEMICOLON_core_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_core_ = 
# 231 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 532 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_core_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv278)) : 'freshtv280)
    | _ ->
        _menhir_fail ()

and _menhir_goto_core_pat : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_core_pat -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState68 | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv261 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv255 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68) : 'freshtv256)
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv257 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_core_pat)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_core_pat_ = 
# 229 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 575 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_core_pat_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv258)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv259 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv260)) : 'freshtv262)
    | MenhirState77 | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv267 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_S_ARR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv263 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74) : 'freshtv264)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv265 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv266)) : 'freshtv268)
    | _ ->
        _menhir_fail ()

and _menhir_goto_core_val : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_core_val -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState41 | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv249 * _menhir_state * 'tv_core_val) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv243 * _menhir_state * 'tv_core_val) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
            | INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
            | STRING _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41) : 'freshtv244)
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv245 * _menhir_state * 'tv_core_val) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_core_val)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_core_val_ = 
# 229 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 658 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_core_val_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv246)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv247 * _menhir_state * 'tv_core_val) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv248)) : 'freshtv250)
    | MenhirState26 | MenhirState32 | MenhirState80 | MenhirState74 | MenhirState44 | MenhirState54 | MenhirState50 | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv253 * _menhir_state * 'tv_core_val) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv251 * _menhir_state * 'tv_core_val) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_core_val)) = _menhir_stack in
        let _v : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 677 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ) = 
# 64 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( CoreVal _1 )
# 681 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv252)) : 'freshtv254)
    | _ ->
        _menhir_fail ()

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 690 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
        let (_menhir_stack : 'freshtv241 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 706 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv242)

and _menhir_goto_core : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 714 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv185 * _menhir_state)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 724 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv179 * _menhir_state)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 734 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv177 * _menhir_state)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 741 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_3 : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 746 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 754 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = 
# 60 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( CoreApp (_3, []) )
# 758 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv178)) : 'freshtv180)
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv181 * _menhir_state)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 766 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50) : 'freshtv182)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv183 * _menhir_state)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 796 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv184)) : 'freshtv186)
    | MenhirState54 | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv193 * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 805 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv187 * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 815 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APP ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | CASE ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | ID _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | LAM ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | LEFT_OXFORD ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | STRING _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54) : 'freshtv188)
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv189 * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 843 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 848 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_core_ = 
# 229 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 853 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_core_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv190)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv191 * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 863 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)) : 'freshtv194)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv201 * _menhir_state)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 872 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv197 * _menhir_state)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 882 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
                let (_menhir_stack : 'freshtv195) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState58 in
                ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_case__ = 
# 142 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 904 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
                 in
                _menhir_goto_loption_separated_nonempty_list_SEMICOLON_case__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv196)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58) : 'freshtv198)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv199 * _menhir_state)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 918 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)) : 'freshtv202)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv215 * _menhir_state * 'tv_core_pat)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 927 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv213 * _menhir_state * 'tv_core_pat)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 933 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_core_pat)), _, (_3 : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 938 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_case = 
# 77 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
                                ( (_1, _3) )
# 944 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv211) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_case) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv209 * _menhir_state * 'tv_case) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv203 * _menhir_state * 'tv_case) = Obj.magic _menhir_stack in
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77) : 'freshtv204)
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv205 * _menhir_state * 'tv_case) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_case)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_case_ = 
# 229 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 983 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_case_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv206)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv207 * _menhir_state * 'tv_case) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv208)) : 'freshtv210)) : 'freshtv212)) : 'freshtv214)) : 'freshtv216)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv223 * _menhir_state)) * _menhir_state * 'tv_separated_nonempty_list_DOT_ID_)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 998 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv219 * _menhir_state)) * _menhir_state * 'tv_separated_nonempty_list_DOT_ID_)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1008 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv217 * _menhir_state)) * _menhir_state * 'tv_separated_nonempty_list_DOT_ID_)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1015 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_3 : 'tv_separated_nonempty_list_DOT_ID_)), _, (_5 : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1020 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1029 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = 
# 68 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Lam (_3, _5) )
# 1033 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv218)) : 'freshtv220)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv221 * _menhir_state)) * _menhir_state * 'tv_separated_nonempty_list_DOT_ID_)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1043 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv222)) : 'freshtv224)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv231 * _menhir_state)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1052 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv227 * _menhir_state)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1062 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv225 * _menhir_state)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1069 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_3 : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1074 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1082 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = 
# 70 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Lam ([], _3) )
# 1086 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv226)) : 'freshtv228)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv229 * _menhir_state)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1096 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv230)) : 'freshtv232)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv239 * _menhir_state) * _menhir_state * (
# 26 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1105 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )))) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1109 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv237 * _menhir_state) * _menhir_state * (
# 26 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1115 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )))) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1119 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (_2 : (
# 26 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1124 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))), _, (_5 : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1128 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _4 = () in
        let _3 = () in
        let _1 = () in
        let _v : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 1136 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ) = 
# 96 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
                                                    ( (_2, _5) )
# 1140 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv235) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 1148 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv233 * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 1155 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFT_OXFORD ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | EOF ->
            _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88) : 'freshtv234)) : 'freshtv236)) : 'freshtv238)) : 'freshtv240)
    | _ ->
        _menhir_fail ()

and _menhir_goto_prim : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_prim -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState26 | MenhirState32 | MenhirState80 | MenhirState74 | MenhirState44 | MenhirState54 | MenhirState50 | MenhirState47 | MenhirState41 | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv171) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_prim) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv169) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_prim) : 'tv_prim) = _v in
        ((let _v : 'tv_core_val = 
# 53 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( ValLit _1 )
# 1186 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 87 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( PatternLit _1 )
# 1201 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv174)) : 'freshtv176)
    | _ ->
        _menhir_fail ()

and _menhir_reduce5 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1210 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1216 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ))) = _menhir_stack in
    let _v : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1221 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ) = 
# 66 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( CoreVar _1 )
# 1225 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1232 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
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
# 144 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 1264 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 231 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 1281 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_scope_pat_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv166)) : 'freshtv168)
    | _ ->
        _menhir_fail ()

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1290 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv159) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 9 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1300 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    )) : (
# 9 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1304 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    )) = _v in
    ((let _v : 'tv_prim = 
# 94 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
           ( PrimString  _1     )
# 1309 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1325 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_OXFORD ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv151 * _menhir_state) * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1336 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv149 * _menhir_state) * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1343 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), (_2 : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1348 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 1355 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = 
# 74 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Metavar _2 )
# 1359 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv150)) : 'freshtv152)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv153 * _menhir_state) * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1369 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1404 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
                _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv141 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1423 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 7 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (Bigint.t)
# 1450 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv139) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 7 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (Bigint.t)
# 1460 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    )) : (
# 7 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (Bigint.t)
# 1464 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    )) = _v in
    ((let _v : 'tv_prim = 
# 93 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
           ( PrimInteger _1     )
# 1469 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_prim _menhir_env _menhir_stack _menhir_s _v) : 'freshtv140)

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1476 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_PAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
    | EOF | LEFT_OXFORD | RIGHT_PAREN | SEMICOLON ->
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv137 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1494 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 1608 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 26 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1642 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = let _3 =
              let xs = xs0 in
              
# 220 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 1648 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
              
            in
            
# 36 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( DPatternTm ("app", _3) )
# 1654 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 1669 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv115 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1679 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv113 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1686 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1691 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))), _, (xs0 : 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (
# 26 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1698 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = let _3 =
              let xs = xs0 in
              
# 220 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 1704 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
              
            in
            
# 34 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( DPatternTm (_1, _3) )
# 1710 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv114)) : 'freshtv116)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv117 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1720 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)) : 'freshtv120)
    | _ ->
        _menhir_fail ()

and _menhir_run6 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1730 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 1743 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 1758 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 26 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1775 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState4 | MenhirState10 | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * _menhir_state * (
# 26 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1785 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * _menhir_state * (
# 26 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1791 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 26 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1796 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_scope_pat = 
# 47 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( DenotationScopePat ([], _1) )
# 1801 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_scope_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv84)) : 'freshtv86)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv89 * _menhir_state * 'tv_loption_separated_nonempty_list_DOT_ID__)) * _menhir_state * (
# 26 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1809 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv87 * _menhir_state * 'tv_loption_separated_nonempty_list_DOT_ID__)) * _menhir_state * (
# 26 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1815 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (xs0 : 'tv_loption_separated_nonempty_list_DOT_ID__)), _, (_3 : (
# 26 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1820 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_scope_pat = let _1 =
          let xs = xs0 in
          
# 220 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 1828 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
          
        in
        
# 45 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( DenotationScopePat (_1, _3) )
# 1834 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_scope_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv88)) : 'freshtv90)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * _menhir_state) * _menhir_state * (
# 26 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1842 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_OXFORD ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv95 * _menhir_state) * _menhir_state * (
# 26 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1852 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQ ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv91 * _menhir_state) * _menhir_state * (
# 26 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1862 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 26 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1892 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)) : 'freshtv96)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv97 * _menhir_state) * _menhir_state * (
# 26 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1903 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)) : 'freshtv100)
    | _ ->
        _menhir_fail ()

and _menhir_reduce32 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1913 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1919 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ))) = _menhir_stack in
    let _v : (
# 26 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 1924 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ) = 
# 41 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( DVar (Some _1) )
# 1928 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1935 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
        _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState4
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
# 1961 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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

and _menhir_reduce27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__ = 
# 142 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 2002 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_SEMICOLON_scope_pat__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2009 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
        _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2029 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 29 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.denotation_chart)
# 2054 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = 
# 98 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
                                  ( DenotationChart _1 )
# 2058 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv59) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 29 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.denotation_chart)
# 2066 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv57) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 29 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.denotation_chart)
# 2074 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv55) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 29 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.denotation_chart)
# 2082 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) : (
# 29 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.denotation_chart)
# 2086 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 2101 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) * _menhir_state * 'tv_list_dynamics_rule_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 2107 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) * _menhir_state * 'tv_list_dynamics_rule_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 2112 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))), _, (xs : 'tv_list_dynamics_rule_)) = _menhir_stack in
        let _v : 'tv_list_dynamics_rule_ = 
# 201 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 2117 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 26 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 2133 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ) = 
# 39 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( DVar None )
# 2137 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv54)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2144 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_PAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack)
    | RIGHT_OXFORD | RIGHT_PAREN | SEMICOLON ->
        _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2162 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
            _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState10
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
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3 * _menhir_state * (
# 28 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat * core)
# 2209 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2238 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv15 * _menhir_state)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 2247 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv17 * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 2256 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv19 * _menhir_state)) * _menhir_state * (
# 27 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (core)
# 2265 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2289 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 26 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (denotation_pat)
# 2303 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 2327 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state * (
# 8 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2336 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 2355 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 29 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.denotation_chart)
# 2391 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
  

# 2422 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
