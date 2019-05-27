
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | UNDERSCORE
    | STRING of (
# 4 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
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
# 2 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (Bigint.t)
# 24 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
  )
    | ID of (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 29 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
  | MenhirState79
  | MenhirState71
  | MenhirState68
  | MenhirState65
  | MenhirState59
  | MenhirState52
  | MenhirState47
  | MenhirState43
  | MenhirState39
  | MenhirState37
  | MenhirState34
  | MenhirState31
  | MenhirState26
  | MenhirState18
  | MenhirState14
  | MenhirState10
  | MenhirState6
  | MenhirState4
  | MenhirState1
  | MenhirState0

let rec _menhir_goto_separated_nonempty_list_SEMICOLON_core_pat_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_core_pat_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv295) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_core_pat_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv293) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_SEMICOLON_core_pat_) : 'tv_separated_nonempty_list_SEMICOLON_core_pat_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__ = 
# 144 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x )
# 89 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_core_pat__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv294)) : 'freshtv296)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv299 * _menhir_state * 'tv_core_pat)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_core_pat_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv297 * _menhir_state * 'tv_core_pat)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_core_pat_) : 'tv_separated_nonempty_list_SEMICOLON_core_pat_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_core_pat)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_core_pat_ = 
# 243 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 106 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_core_pat_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv298)) : 'freshtv300)
    | _ ->
        _menhir_fail ()

and _menhir_goto_prim : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_prim -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv291) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_prim) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv289) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : 'tv_prim) : 'tv_prim) = _v in
    ((let _v : 'tv_core_pat = 
# 71 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Types.Core.PatternLit _1 )
# 125 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_core_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv290)) : 'freshtv292)

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_core_pat__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv287 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 136 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv283 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 146 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv281 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 153 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 158 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))), _, (xs : 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__)) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : 'tv_core_pat = let _3 = 
# 232 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 165 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        
# 65 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Types.Core.PatternTerm (_1, _3) )
# 170 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv282)) : 'freshtv284)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv285 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 180 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv286)) : 'freshtv288)

and _menhir_goto_core_pat : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_core_pat -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState59 | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv273 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv267 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DEFAULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | ID _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | INT _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | STRING _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | UNDERSCORE ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59) : 'freshtv268)
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv269 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_core_pat)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_core_pat_ = 
# 241 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 222 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_core_pat_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv270)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv271 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv272)) : 'freshtv274)
    | MenhirState68 | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv279 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_S_ARR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv275 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APP ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | CASE ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | ID _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | LAM ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | LEFT_OXFORD ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65) : 'freshtv276)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv277 * _menhir_state * 'tv_core_pat) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv278)) : 'freshtv280)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMICOLON_case_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_case_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv261) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_case_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv259) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_SEMICOLON_case_) : 'tv_separated_nonempty_list_SEMICOLON_case_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_case__ = 
# 144 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x )
# 283 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_case__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv260)) : 'freshtv262)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv265 * _menhir_state * 'tv_case)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_case_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv263 * _menhir_state * 'tv_case)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_case_) : 'tv_separated_nonempty_list_SEMICOLON_case_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_case)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_case_ = 
# 243 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 300 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_case_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv264)) : 'freshtv266)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_case__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMICOLON_case__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (((('freshtv257 * _menhir_state)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 313 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_case__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv253 * _menhir_state)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 323 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_case__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv251 * _menhir_state)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 330 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_case__) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (arg : (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 335 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))), _, (xs : 'tv_loption_separated_nonempty_list_SEMICOLON_case__)) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 344 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ) = let cases = 
# 232 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 348 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        
# 56 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Types.Core.Case (arg, Types.Core.Ty, cases) )
# 353 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv252)) : 'freshtv254)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv255 * _menhir_state)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 363 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_case__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv256)) : 'freshtv258)

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv249) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_core_pat = 
# 67 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Types.Core.PatternVar None )
# 378 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_core_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv250)

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 4 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 385 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv247) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 4 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 395 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    )) : (
# 4 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 399 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    )) = _v in
    ((let _v : 'tv_prim = 
# 78 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
           ( PrimString  _1     )
# 404 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_prim _menhir_env _menhir_stack _menhir_s _v) : 'freshtv248)

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 2 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (Bigint.t)
# 411 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv245) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 2 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (Bigint.t)
# 421 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    )) : (
# 2 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (Bigint.t)
# 425 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    )) = _v in
    ((let _v : 'tv_prim = 
# 77 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
           ( PrimInteger _1     )
# 430 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_prim _menhir_env _menhir_stack _menhir_s _v) : 'freshtv246)

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 437 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv239 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 449 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DEFAULT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | ID _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
        | INT _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
        | STRING _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
        | UNDERSCORE ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv237) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState52 in
            ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_core_pat__ = 
# 142 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 471 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_SEMICOLON_core_pat__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv238)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52) : 'freshtv240)
    | RIGHT_PAREN | RIGHT_S_ARR | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv241 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 483 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 488 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_core_pat = 
# 69 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Types.Core.PatternVar (Some _1) )
# 493 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_core_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv242)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv243 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 503 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv244)

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv235) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_core_pat = 
# 73 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Types.Core.PatternDefault )
# 518 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_core_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv236)

and _menhir_goto_separated_nonempty_list_SEMICOLON_core_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_core_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv229 * _menhir_state)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 531 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv225 * _menhir_state)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 541 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv223 * _menhir_state)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 548 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_3 : (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 553 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))), _, (_5 : 'tv_separated_nonempty_list_SEMICOLON_core_)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 562 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = 
# 48 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Types.Core.CoreApp (_3, _5) )
# 566 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv224)) : 'freshtv226)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv227 * _menhir_state)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 576 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv228)) : 'freshtv230)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv233 * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 585 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv231 * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 591 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_core_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 596 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))), _, (xs : 'tv_separated_nonempty_list_SEMICOLON_core_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_core_ = 
# 243 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 602 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_core_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv232)) : 'freshtv234)
    | _ ->
        _menhir_fail ()

and _menhir_goto_core : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 611 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv167 * _menhir_state)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 621 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv161 * _menhir_state)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 631 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv159 * _menhir_state)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 638 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_3 : (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 643 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 651 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = 
# 46 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Types.Core.CoreApp (_3, []) )
# 655 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv160)) : 'freshtv162)
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv163 * _menhir_state)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 663 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APP ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState39
            | CASE ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState39
            | ID _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
            | LAM ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState39
            | LEFT_OXFORD ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState39
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39) : 'freshtv164)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv165 * _menhir_state)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 689 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)) : 'freshtv168)
    | MenhirState43 | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv175 * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 698 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv169 * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 708 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APP ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | CASE ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | ID _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
            | LAM ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | LEFT_OXFORD ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43) : 'freshtv170)
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv171 * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 732 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 737 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_core_ = 
# 241 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 742 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_core_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv172)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv173 * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 752 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)) : 'freshtv176)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv183 * _menhir_state)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 761 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv179 * _menhir_state)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 771 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DEFAULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | ID _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
            | INT _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
            | STRING _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
            | UNDERSCORE ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | RIGHT_PAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv177) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState47 in
                ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_case__ = 
# 142 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 793 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
                 in
                _menhir_goto_loption_separated_nonempty_list_SEMICOLON_case__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv178)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47) : 'freshtv180)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv181 * _menhir_state)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 807 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)) : 'freshtv184)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv197 * _menhir_state * 'tv_core_pat)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 816 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv195 * _menhir_state * 'tv_core_pat)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 822 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_core_pat)), _, (_3 : (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 827 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_case = 
# 61 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
                                ( (_1, _3) )
# 833 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv193) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_case) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv191 * _menhir_state * 'tv_case) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv185 * _menhir_state * 'tv_case) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DEFAULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | ID _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | INT _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | STRING _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | UNDERSCORE ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68) : 'freshtv186)
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv187 * _menhir_state * 'tv_case) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_case)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_case_ = 
# 241 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 872 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_case_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv188)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv189 * _menhir_state * 'tv_case) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv190)) : 'freshtv192)) : 'freshtv194)) : 'freshtv196)) : 'freshtv198)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv205 * _menhir_state)) * _menhir_state * 'tv_separated_nonempty_list_DOT_ID_)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 887 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv201 * _menhir_state)) * _menhir_state * 'tv_separated_nonempty_list_DOT_ID_)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 897 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv199 * _menhir_state)) * _menhir_state * 'tv_separated_nonempty_list_DOT_ID_)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 904 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_3 : 'tv_separated_nonempty_list_DOT_ID_)), _, (_5 : (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 909 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 918 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = 
# 52 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Types.Core.Lam (_3, _5) )
# 922 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv200)) : 'freshtv202)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv203 * _menhir_state)) * _menhir_state * 'tv_separated_nonempty_list_DOT_ID_)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 932 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)) : 'freshtv206)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv213 * _menhir_state)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 941 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv209 * _menhir_state)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 951 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv207 * _menhir_state)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 958 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_3 : (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 963 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 971 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = 
# 54 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Types.Core.Lam ([], _3) )
# 975 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv208)) : 'freshtv210)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv211 * _menhir_state)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 985 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv212)) : 'freshtv214)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv221 * _menhir_state) * _menhir_state * (
# 21 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat)
# 994 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )))) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 998 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv219 * _menhir_state) * _menhir_state * (
# 21 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat)
# 1004 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )))) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 1008 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (_2 : (
# 21 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat)
# 1013 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))), _, (_5 : (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 1017 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _4 = () in
        let _3 = () in
        let _1 = () in
        let _v : (
# 23 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat * Types.Core.core)
# 1025 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ) = 
# 80 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
                                                    ( (_2, _5) )
# 1029 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv217) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 23 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat * Types.Core.core)
# 1037 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv215 * _menhir_state * (
# 23 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat * Types.Core.core)
# 1044 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFT_OXFORD ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | EOF ->
            _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79) : 'freshtv216)) : 'freshtv218)) : 'freshtv220)) : 'freshtv222)
    | _ ->
        _menhir_fail ()

and _menhir_reduce4 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1063 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1069 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ))) = _menhir_stack in
    let _v : (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 1074 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ) = 
# 50 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Types.Core.CoreVar _1 )
# 1078 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_SEMICOLON_scope_pat_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_scope_pat_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState4 | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv153) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_scope_pat_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv151) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_SEMICOLON_scope_pat_) : 'tv_separated_nonempty_list_SEMICOLON_scope_pat_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__ = 
# 144 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x )
# 1097 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_scope_pat__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv152)) : 'freshtv154)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv157 * _menhir_state * 'tv_scope_pat)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_scope_pat_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv155 * _menhir_state * 'tv_scope_pat)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_scope_pat_) : 'tv_separated_nonempty_list_SEMICOLON_scope_pat_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_scope_pat)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_scope_pat_ = 
# 243 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 1114 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_scope_pat_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv156)) : 'freshtv158)
    | _ ->
        _menhir_fail ()

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv147 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1132 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_OXFORD ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv143 * _menhir_state) * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1143 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv141 * _menhir_state) * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1150 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), (_2 : (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1155 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 1162 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = 
# 58 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Types.Core.Metavar _2 )
# 1166 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_core _menhir_env _menhir_stack _menhir_s _v) : 'freshtv142)) : 'freshtv144)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv145 * _menhir_state) * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1176 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)) : 'freshtv148)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv149 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv137 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | APP ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | CASE ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv135) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState31 in
            let (_v : (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1211 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DOT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
            | RIGHT_PAREN ->
                _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv133 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1228 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
                )) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)) : 'freshtv136)
        | LAM ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | LEFT_OXFORD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31) : 'freshtv138)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv139 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1251 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | CASE ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | ID _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | LAM ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LEFT_OXFORD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34) : 'freshtv130)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv131 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv125 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | APP ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | CASE ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | ID _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
        | LAM ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | LEFT_OXFORD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37) : 'freshtv126)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv127 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)

and _menhir_goto_scope_pat : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_scope_pat -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv123 * _menhir_state * 'tv_scope_pat) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117 * _menhir_state * 'tv_scope_pat) = Obj.magic _menhir_stack in
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
            _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14) : 'freshtv118)
    | RIGHT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119 * _menhir_state * 'tv_scope_pat) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_scope_pat)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_scope_pat_ = 
# 241 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 1359 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_scope_pat_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv120)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv121 * _menhir_state * 'tv_scope_pat) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)) : 'freshtv124)

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_scope_pat__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv107 * _menhir_state)) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv103 * _menhir_state)) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv101 * _menhir_state)) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (xs : 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (
# 21 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat)
# 1393 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = let _3 = 
# 232 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 1397 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            
# 31 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Types.Denotation.DPatternTm ("app", _3) )
# 1402 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv102)) : 'freshtv104)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv105 * _menhir_state)) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)) : 'freshtv108)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv115 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1417 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv111 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1427 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv109 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1434 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1439 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (
# 21 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat)
# 1446 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = let _3 = 
# 232 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 1450 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            
# 29 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Types.Denotation.DPatternTm (_1, _3) )
# 1455 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv110)) : 'freshtv112)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv113 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1465 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)) : 'freshtv116)
    | _ ->
        _menhir_fail ()

and _menhir_run6 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1475 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv99) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState6 in
        let (_v : (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1488 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
            let (_menhir_stack : 'freshtv97 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1503 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)) : 'freshtv100)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_pat : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 21 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat)
# 1520 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState4 | MenhirState10 | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv81 * _menhir_state * (
# 21 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat)
# 1530 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv79 * _menhir_state * (
# 21 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat)
# 1536 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 21 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat)
# 1541 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_scope_pat = 
# 42 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Types.Denotation.DenotationScopePat ([], _1) )
# 1546 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_scope_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv80)) : 'freshtv82)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv85 * _menhir_state * 'tv_loption_separated_nonempty_list_DOT_ID__)) * _menhir_state * (
# 21 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat)
# 1554 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv83 * _menhir_state * 'tv_loption_separated_nonempty_list_DOT_ID__)) * _menhir_state * (
# 21 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat)
# 1560 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (xs : 'tv_loption_separated_nonempty_list_DOT_ID__)), _, (_3 : (
# 21 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat)
# 1565 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_scope_pat = let _1 = 
# 232 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 1571 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        
# 40 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Types.Denotation.DenotationScopePat (_1, _3) )
# 1576 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_scope_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv84)) : 'freshtv86)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv95 * _menhir_state) * _menhir_state * (
# 21 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat)
# 1584 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_OXFORD ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv91 * _menhir_state) * _menhir_state * (
# 21 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat)
# 1594 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQ ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv87 * _menhir_state) * _menhir_state * (
# 21 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat)
# 1604 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | APP ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | CASE ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | ID _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
                | LAM ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | LEFT_OXFORD ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26) : 'freshtv88)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv89 * _menhir_state) * _menhir_state * (
# 21 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat)
# 1630 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)) : 'freshtv92)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv93 * _menhir_state) * _menhir_state * (
# 21 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat)
# 1641 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)) : 'freshtv96)
    | _ ->
        _menhir_fail ()

and _menhir_reduce29 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1651 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1657 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ))) = _menhir_stack in
    let _v : (
# 21 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat)
# 1662 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ) = 
# 36 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Types.Denotation.DVar (Some _1) )
# 1666 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1673 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
        _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | DOT ->
        _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_reduce18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_DOT_ID__ = 
# 142 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 1699 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv77) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_loption_separated_nonempty_list_DOT_ID__) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv75 * _menhir_state * 'tv_loption_separated_nonempty_list_DOT_ID__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71 * _menhir_state * 'tv_loption_separated_nonempty_list_DOT_ID__) = Obj.magic _menhir_stack in
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18) : 'freshtv72)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state * 'tv_loption_separated_nonempty_list_DOT_ID__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)) : 'freshtv76)) : 'freshtv78)

and _menhir_reduce24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_scope_pat__ = 
# 142 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 1740 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_SEMICOLON_scope_pat__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1747 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
        _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv69 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1767 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)

and _menhir_goto_list_dynamics_rule_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_dynamics_rule_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * _menhir_state * 'tv_list_dynamics_rule_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv59 * _menhir_state * 'tv_list_dynamics_rule_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv57 * _menhir_state * 'tv_list_dynamics_rule_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_list_dynamics_rule_)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 24 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.chart)
# 1792 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            ) = 
# 82 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
                                  ( DenotationChart _1 )
# 1796 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv55) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 24 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.chart)
# 1804 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv53) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 24 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.chart)
# 1812 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv51) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 24 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.chart)
# 1820 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) : (
# 24 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.chart)
# 1824 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv52)) : 'freshtv54)) : 'freshtv56)) : 'freshtv58)) : 'freshtv60)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv61 * _menhir_state * 'tv_list_dynamics_rule_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)) : 'freshtv64)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * _menhir_state * (
# 23 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat * Types.Core.core)
# 1839 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) * _menhir_state * 'tv_list_dynamics_rule_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv65 * _menhir_state * (
# 23 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat * Types.Core.core)
# 1845 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) * _menhir_state * 'tv_list_dynamics_rule_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : (
# 23 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat * Types.Core.core)
# 1850 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))), _, (xs : 'tv_list_dynamics_rule_)) = _menhir_stack in
        let _v : 'tv_list_dynamics_rule_ = 
# 213 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 1855 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
         in
        _menhir_goto_list_dynamics_rule_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv66)) : 'freshtv68)
    | _ ->
        _menhir_fail ()

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv49) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : (
# 21 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat)
# 1871 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
    ) = 
# 34 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
  ( Types.Denotation.DVar None )
# 1875 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
     in
    _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv50)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1882 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_PAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack)
    | RIGHT_OXFORD | RIGHT_PAREN | SEMICOLON ->
        _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv47 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1900 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43 * _menhir_state) = Obj.magic _menhir_stack in
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
            _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | DOT ->
            _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10) : 'freshtv44)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv45 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3 * _menhir_state * (
# 23 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat * Types.Core.core)
# 1947 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv4)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv5 * _menhir_state)) * _menhir_state * 'tv_separated_nonempty_list_DOT_ID_)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv6)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv7 * _menhir_state * 'tv_case)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv9 * _menhir_state * 'tv_core_pat)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv11 * _menhir_state * 'tv_core_pat)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv13 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 1976 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv15 * _menhir_state)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 1985 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv17 * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 1994 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv19 * _menhir_state)) * _menhir_state * (
# 22 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Core.core)
# 2003 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv21 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv23 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv27 * _menhir_state) * _menhir_state * (
# 21 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.pat)
# 2027 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        )))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state * 'tv_loption_separated_nonempty_list_DOT_ID__)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv31 * _menhir_state * 'tv_scope_pat)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv33 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv35 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2051 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state * (
# 3 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
       (string)
# 2060 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv41) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv42)

and _menhir_reduce16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_dynamics_rule_ = 
# 211 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 2079 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
# 24 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.mly"
      (Types.Denotation.chart)
# 2115 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
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
        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 269 "/Users/joel/.opam/default/lib/menhir/standard.mly"
  

# 2146 "/Users/joel/code/lvca-bucklescript/src/dynamicsParser.ml"
