
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | SEMICOLON
    | RIGHT_PAREN
    | RIGHT_BRACK
    | LEFT_PAREN
    | LEFT_BRACK
    | ID of (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
  )
    | EOL
    | EOF
    | DOT
    | COMMA
    | BAR
    | ASSIGN
  
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
  | MenhirState47
  | MenhirState44
  | MenhirState38
  | MenhirState37
  | MenhirState32
  | MenhirState27
  | MenhirState25
  | MenhirState19
  | MenhirState18
  | MenhirState17
  | MenhirState12
  | MenhirState11
  | MenhirState9
  | MenhirState7
  | MenhirState5
  | MenhirState4
  | MenhirState2
  | MenhirState0

let rec _menhir_goto_sortDef : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_sortDef -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv197 * _menhir_state * 'tv_sortDef) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47) : 'freshtv198)

and _menhir_goto_separated_nonempty_list_DOT_valence_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_DOT_valence_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv191 * _menhir_state * (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 82 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_DOT_valence_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv189 * _menhir_state * (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 90 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_DOT_valence_) : 'tv_separated_nonempty_list_DOT_valence_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 97 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_DOT_valence_ = 
# 231 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 103 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_separated_nonempty_list_DOT_valence_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv190)) : 'freshtv192)
    | MenhirState32 | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv195) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_DOT_valence_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv193) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_DOT_valence_) : 'tv_separated_nonempty_list_DOT_valence_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_DOT_valence__ = 
# 144 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 118 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_DOT_valence__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv194)) : 'freshtv196)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_BAR_operatorDef_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_BAR_operatorDef_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv183) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_BAR_operatorDef_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv181) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_BAR_operatorDef_) : 'tv_separated_nonempty_list_BAR_operatorDef_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_BAR_operatorDef__ = 
# 144 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 139 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv179) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_loption_separated_nonempty_list_BAR_operatorDef__) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv177) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_loption_separated_nonempty_list_BAR_operatorDef__) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv175) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((xs0 : 'tv_loption_separated_nonempty_list_BAR_operatorDef__) : 'tv_loption_separated_nonempty_list_BAR_operatorDef__) = _v in
        ((let _v : 'tv_operatorSingleLineBodyDef = let _1 =
          let xs = xs0 in
          
# 220 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 158 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
          
        in
        
# 63 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
                                     ( _1 )
# 164 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv173) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_operatorSingleLineBodyDef) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv171 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 174 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_operatorSingleLineBodyDef) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv169 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 182 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_3 : 'tv_operatorSingleLineBodyDef) : 'tv_operatorSingleLineBodyDef) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 189 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_sortDef = 
# 56 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
                                        ( (_1, _3) )
# 195 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_sortDef _menhir_env _menhir_stack _menhir_s _v) : 'freshtv170)) : 'freshtv172)) : 'freshtv174)) : 'freshtv176)) : 'freshtv178)) : 'freshtv180)) : 'freshtv182)) : 'freshtv184)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv187 * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 203 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_BAR_operatorDef_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv185 * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 211 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_BAR_operatorDef_) : 'tv_separated_nonempty_list_BAR_operatorDef_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 218 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_BAR_operatorDef_ = 
# 231 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 224 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_separated_nonempty_list_BAR_operatorDef_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv186)) : 'freshtv188)
    | _ ->
        _menhir_fail ()

and _menhir_goto_operatorMultilineBodyDef : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_operatorMultilineBodyDef -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv163 * _menhir_state)) * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 238 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_operatorMultilineBodyDef) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv161 * _menhir_state)) * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 246 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_4 : 'tv_operatorMultilineBodyDef) : 'tv_operatorMultilineBodyDef) = _v in
        ((let ((_menhir_stack, _menhir_s), _, (_3 : (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 253 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_operatorMultilineBodyDef = 
# 59 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
                                                 ( _3 :: _4 )
# 260 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_operatorMultilineBodyDef _menhir_env _menhir_stack _menhir_s _v) : 'freshtv162)) : 'freshtv164)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv167 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 268 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_operatorMultilineBodyDef) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv165 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 276 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_3 : 'tv_operatorMultilineBodyDef) : 'tv_operatorMultilineBodyDef) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 283 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_sortDef = 
# 55 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
                                        ( (_1, _3) )
# 289 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_sortDef _menhir_env _menhir_stack _menhir_s _v) : 'freshtv166)) : 'freshtv168)
    | _ ->
        _menhir_fail ()

and _menhir_goto_fixedValence : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_fixedValence -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv155 * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 303 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_fixedValence) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv153 * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 311 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_3 : 'tv_fixedValence) : 'tv_fixedValence) = _v in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 318 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))), _) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_fixedValence = 
# 29 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
  ( match _3 with | Types.FixedValence (binds, result) -> Types.FixedValence (_1 :: binds, result) )
# 324 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_fixedValence _menhir_env _menhir_stack _menhir_s _v) : 'freshtv154)) : 'freshtv156)
    | MenhirState32 | MenhirState4 | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv159) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_fixedValence) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv157) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_fixedValence) : 'tv_fixedValence) = _v in
        ((let _v : (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 339 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ) = 
# 25 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
                                   ( _1                       )
# 343 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_valence _menhir_env _menhir_stack _menhir_s _v) : 'freshtv158)) : 'freshtv160)
    | _ ->
        _menhir_fail ()

and _menhir_goto_valence : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 352 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv151 * _menhir_state * (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 360 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
    )) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv145 * _menhir_state * (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 370 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
        | LEFT_PAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17) : 'freshtv146)
    | RIGHT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv147 * _menhir_state * (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 388 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 393 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_DOT_valence_ = 
# 229 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 398 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_separated_nonempty_list_DOT_valence_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv148)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv149 * _menhir_state * (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 408 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)) : 'freshtv152)

and _menhir_goto_operatorDef : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 416 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv135 * _menhir_state)) * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 426 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOL ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | ID _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv133 * _menhir_state)) * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 438 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_3 : (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 443 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_operatorMultilineBodyDef = 
# 60 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
                                                 ( [ _3 ] )
# 450 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
             in
            _menhir_goto_operatorMultilineBodyDef _menhir_env _menhir_stack _menhir_s _v) : 'freshtv134)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38) : 'freshtv136)
    | MenhirState44 | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv143 * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 462 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv137 * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 472 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44) : 'freshtv138)
        | ID _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv139 * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 488 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 493 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_BAR_operatorDef_ = 
# 229 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 498 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
             in
            _menhir_goto_separated_nonempty_list_BAR_operatorDef_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv140)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv141 * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 508 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)) : 'freshtv144)
    | _ ->
        _menhir_fail ()

and _menhir_goto_sort : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 518 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv115 * _menhir_state) * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 528 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
        | LEFT_PAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv113 * _menhir_state) * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 542 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState7 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv111 * _menhir_state) * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 550 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 556 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 563 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ) = 
# 34 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
                                ( _2             )
# 567 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
             in
            _menhir_goto_sort _menhir_env _menhir_stack _menhir_s _v) : 'freshtv112)) : 'freshtv114)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv116)
    | MenhirState18 | MenhirState12 | MenhirState9 | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv119 * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 579 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 583 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | LEFT_PAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | DOT | RIGHT_BRACK | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv117 * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 597 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 601 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 606 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))), _, (_2 : (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 610 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) = _menhir_stack in
            let _v : (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 615 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ) = 
# 36 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
                                ( Types.SortAp(_1, _2) )
# 619 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
             in
            _menhir_goto_sort _menhir_env _menhir_stack _menhir_s _v) : 'freshtv118)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9) : 'freshtv120)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv125 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 631 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 635 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | LEFT_PAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | RIGHT_BRACK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv123 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 649 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 653 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState12 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv121 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 661 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 665 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 671 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))), _, (_3 : (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 675 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 682 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ) = 
# 24 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
                                   ( VariableValence (_1, _3) )
# 686 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
             in
            _menhir_goto_valence _menhir_env _menhir_stack _menhir_s _v) : 'freshtv122)) : 'freshtv124)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12) : 'freshtv126)
    | MenhirState32 | MenhirState4 | MenhirState19 | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv131 * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 698 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv127 * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 708 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState18 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
            | LEFT_PAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19) : 'freshtv128)
        | ID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | LEFT_PAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv129 * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 732 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 737 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) = _menhir_stack in
            let _v : 'tv_fixedValence = 
# 31 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
  ( FixedValence ([], _1) )
# 742 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
             in
            _menhir_goto_fixedValence _menhir_env _menhir_stack _menhir_s _v) : 'freshtv130)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18) : 'freshtv132)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_loption_separated_nonempty_list_DOT_valence__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_DOT_valence__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv109) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_loption_separated_nonempty_list_DOT_valence__) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv107) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((xs0 : 'tv_loption_separated_nonempty_list_DOT_valence__) : 'tv_loption_separated_nonempty_list_DOT_valence__) = _v in
    ((let _v : 'tv_valenceList = let _1 =
      let xs = xs0 in
      
# 220 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 772 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
      
    in
    
# 46 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
                                          ( _1 )
# 778 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv105) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_valenceList) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv95 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 791 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) * _menhir_state * 'tv_valenceList) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv91 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 801 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) * _menhir_state * 'tv_valenceList) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv89 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 808 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) * _menhir_state * 'tv_valenceList) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 813 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))), _, (_3 : 'tv_valenceList)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 820 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ) = 
# 52 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
  ( OperatorDef(_1, Arity([], _3)) )
# 824 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
             in
            _menhir_goto_operatorDef _menhir_env _menhir_stack _menhir_s _v) : 'freshtv90)) : 'freshtv92)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv93 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 834 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) * _menhir_state * 'tv_valenceList) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)) : 'freshtv96)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv103 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 843 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) * _menhir_state * 'tv_nameList))) * _menhir_state * 'tv_valenceList) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv99 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 853 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) * _menhir_state * 'tv_nameList))) * _menhir_state * 'tv_valenceList) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv97 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 860 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) * _menhir_state * 'tv_nameList))) * _menhir_state * 'tv_valenceList) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_1 : (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 865 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))), _, (_3 : 'tv_nameList)), _, (_6 : 'tv_valenceList)) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _v : (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 874 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ) = 
# 50 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
  ( OperatorDef(_1, Arity(_3, _6)) )
# 878 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
             in
            _menhir_goto_operatorDef _menhir_env _menhir_stack _menhir_s _v) : 'freshtv98)) : 'freshtv100)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv101 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 888 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) * _menhir_state * 'tv_nameList))) * _menhir_state * 'tv_valenceList) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)) : 'freshtv104)
    | _ ->
        _menhir_fail ()) : 'freshtv106)) : 'freshtv108)) : 'freshtv110)

and _menhir_reduce25 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 898 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 904 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
    ))) = _menhir_stack in
    let _v : (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 909 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
    ) = 
# 35 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
                                ( SortName _1    )
# 913 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
     in
    _menhir_goto_sort _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 920 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack)

and _menhir_goto_separated_nonempty_list_COMMA_ID_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_ID_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 935 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_ID_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 943 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_ID_) : 'tv_separated_nonempty_list_COMMA_ID_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 950 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_COMMA_ID_ = 
# 231 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 956 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv82)) : 'freshtv84)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv87) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_ID_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_ID_) : 'tv_separated_nonempty_list_COMMA_ID_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_ID__ = 
# 144 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 971 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv86)) : 'freshtv88)
    | _ ->
        _menhir_fail ()

and _menhir_reduce10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_DOT_valence__ = 
# 142 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 982 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_DOT_valence__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | LEFT_PAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1004 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_BRACK ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv77 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1016 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | LEFT_PAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11) : 'freshtv78)
    | DOT | ID _ | LEFT_PAREN | RIGHT_PAREN ->
        _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv79 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1038 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)

and _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_ID__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv75) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_loption_separated_nonempty_list_COMMA_ID__) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv73) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((xs0 : 'tv_loption_separated_nonempty_list_COMMA_ID__) : 'tv_loption_separated_nonempty_list_COMMA_ID__) = _v in
    ((let _v : 'tv_nameList = let _1 =
      let xs = xs0 in
      
# 220 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 1058 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
      
    in
    
# 44 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
                                    ( _1 )
# 1064 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv71) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_nameList) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv69 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1075 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
    ))) * _menhir_state * 'tv_nameList) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHT_BRACK ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv65 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1085 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) * _menhir_state * 'tv_nameList) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv61 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1095 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) * _menhir_state * 'tv_nameList)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
            | LEFT_PAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | RIGHT_PAREN ->
                _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32) : 'freshtv62)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv63 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1117 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
            ))) * _menhir_state * 'tv_nameList)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)) : 'freshtv66)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv67 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1128 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) * _menhir_state * 'tv_nameList) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)) : 'freshtv70)) : 'freshtv72)) : 'freshtv74)) : 'freshtv76)

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1136 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1148 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27) : 'freshtv56)
    | RIGHT_BRACK ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1164 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1169 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_ID_ = 
# 229 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 1174 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv58)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1184 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1192 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_BRACK ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1204 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
        | RIGHT_BRACK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv47) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState25 in
            ((let _v : 'tv_loption_separated_nonempty_list_COMMA_ID__ = 
# 142 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 1218 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv48)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25) : 'freshtv50)
    | LEFT_PAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1230 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
        | LEFT_PAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | RIGHT_PAREN ->
            _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4) : 'freshtv52)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1252 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37) : 'freshtv44)
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
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state * 'tv_sortDef) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv9 * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 1296 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv11 * _menhir_state)) * _menhir_state * (
# 18 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.operatorDef)
# 1305 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv13 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv15 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1319 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) * _menhir_state * 'tv_nameList))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv17 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1328 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv19 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1337 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv21 * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 1346 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 1355 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state * (
# 20 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.valence)
# 1364 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv27 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1373 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 1377 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1386 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv31 * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 1395 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 1399 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv33 * _menhir_state) * _menhir_state * (
# 16 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.sort)
# 1408 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv35 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1422 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv39 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1431 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv41) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv42)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1443 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1455 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOL ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2) : 'freshtv4)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5 * _menhir_state * (
# 1 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
       (string)
# 1475 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
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

and languageDef : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 15 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.mly"
      (Types.language)
# 1495 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
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
    | ID _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 233 "/home/joel/.opam/4.06.0/lib/menhir/standard.mly"
  

# 1524 "/media/joel/nixos/home/joel/code/lvca-bucklescript/src/languageParser.ml"
