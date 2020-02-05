%token <int> NAT
%token <string> REGEX
%token <string> TERMINAL_ID
%token <string> NONTERMINAL_ID
%token <string> STRING
%token LEFT_PAREN
%token RIGHT_PAREN
%token DOT
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_ANGLE
%token RIGHT_ANGLE
%token EOF
%token ASSIGN
%token COLON
%token DOLLAR
%token BAR
%token ARROW
%token SEMICOLON
%token LEFT_FIXITY
%token RIGHT_FIXITY
%token UNDERSCORE
%token COMMA
%token FORALL

%{ open ConcreteSyntaxDescription

let concretize_vars : Belt.Set.String.t -> Types.sort -> Types.sort
  = fun var_set ->
    let open Types in
    let rec go = function
      | SortAp (name, args)
      -> SortAp (name, Belt.Array.map args go)
      | SortVar name
      -> if Belt.Set.String.has var_set name
         then SortVar name
         else SortAp (name, [||])
    in go
%}

%start quantifiers__test
%start terminal_rule__test
%start capture_number
%start nonterminal_token__test
%start operator_match__test
%start nonterminal_rule__test
%start nonterminal_type__test
%start language
%type <Belt.Set.String.t> quantifiers
%type <Belt.Set.String.t> quantifiers__test
%type <ConcreteSyntaxDescription.pre_terminal_rule> terminal_rule
%type <ConcreteSyntaxDescription.pre_terminal_rule> terminal_rule__test
%type <ConcreteSyntaxDescription.capture_number> capture_number
%type <ConcreteSyntaxDescription.nonterminal_token> nonterminal_token
%type <ConcreteSyntaxDescription.nonterminal_token> nonterminal_token__test
%type <ConcreteSyntaxDescription.operator_match> operator_match
%type <ConcreteSyntaxDescription.operator_match> operator_match__test
%type <ConcreteSyntaxDescription.operator_match list list> operator_match_list
%type <ConcreteSyntaxDescription.nonterminal_rule> nonterminal_rule
%type <ConcreteSyntaxDescription.nonterminal_rule> nonterminal_rule__test
%type <ConcreteSyntaxDescription.nonterminal_type> nonterminal_type
%type <ConcreteSyntaxDescription.nonterminal_type> nonterminal_type__test
%type <ConcreteSyntaxDescription.operator_match_pattern> operator_match_pattern
%type <ConcreteSyntaxDescription.pre_terminal_rule list * ConcreteSyntaxDescription.nonterminal_rule list> language
%%

language: terminal_rule+ nonterminal_rule+ EOF { ($1, $2) }

terminal_rule:
  | TERMINAL_ID ASSIGN REGEX
  { PreTerminalRule ($1, Left $3) }
  | TERMINAL_ID ASSIGN STRING
  { PreTerminalRule ($1, Right $3) }

terminal_rule__test: terminal_rule EOF { $1 }

capture_number: DOLLAR NAT { $2 }

nonterminal_rule__test: nonterminal_rule EOF { $1 }

quantifiers:
  FORALL nonempty_list(NONTERMINAL_ID) DOT
  { $2
    |. Belt.List.toArray
    |. Belt.Set.String.fromArray
  }

quantifiers__test: quantifiers EOF { $1 }

/* TODO: duplicated (sort / atomic_sort) */
sort:
  /* TODO: this is ugly -- should be called SORT_ID or ID */
  | NONTERMINAL_ID nonempty_list(atomic_sort)
  { Types.SortAp ($1, Belt.List.toArray $2) }
  | atomic_sort
  { $1 }

atomic_sort:
  | LEFT_PAREN sort RIGHT_PAREN
  { $2 }
  | NONTERMINAL_ID
  /* TODO: this is ugly -- should be called SORT_ID or ID */
  { Types.SortVar $1 }

nonterminal_type__test: nonterminal_type EOF { $1 }

nonterminal_type:
  | quantifiers? separated_nonempty_list(ARROW, sort)
  { let var_set = match $1 with
      | None -> Belt.Set.String.empty
      | Some var_set -> var_set
    in
    let arg_sorts, result_sort = $2
      |. Belt.List.map (concretize_vars var_set)
      |. Util.unsnoc
    in
    NonterminalType (arg_sorts, result_sort)
  }

nonterminal_rule:
  | NONTERMINAL_ID ASSIGN BAR? operator_match_list
  { NonterminalRule
    { nonterminal_name = $1
    ; nonterminal_type = NonterminalType ([], SortAp ($1, [||]))
    ; operator_rules = $4
    }
  }
  | NONTERMINAL_ID COLON nonterminal_type ASSIGN BAR? operator_match_list
  { NonterminalRule
    { nonterminal_name = $1
    ; nonterminal_type = $3
    ; operator_rules = $6
    }
  }

(* The list of operator matches making up a nonterminal. Each operator match is
 * separated by '|' to indicate the same precedence level, or '>' to indicate
 * different precedence levels.
 *)
operator_match_list:
  | operator_match
    { [[ $1 ]] }
  | operator_match BAR     operator_match_list
    { match $3 with
      | []      -> [[ $1 ]]
      | x :: xs -> ($1 :: x) :: xs
    }
  | operator_match RIGHT_ANGLE operator_match_list
    { [ $1 ] :: $3 }

fixity:
  | LEFT_FIXITY  { Infixl }
  | RIGHT_FIXITY { Infixr }

operator_match:
  | nonempty_list(nonterminal_token)
    LEFT_BRACE operator_match_pattern RIGHT_BRACE option(fixity)
  { let fixity = match $5 with
    | None   -> Nofix
    | Some f -> f
    in OperatorMatch { tokens = $1; operator_match_pattern = $3; fixity } }

(* TODO: should this id allow uppercase? *)
operator_match_pattern:
  | capture_number
  { SingleCapturePattern $1 }
  | NONTERMINAL_ID
    LEFT_PAREN separated_list(SEMICOLON, term_scope_pattern) RIGHT_PAREN
  { OperatorPattern ($1, $3) }

term_scope_pattern:
  separated_nonempty_list(DOT, operator_match_pattern)
  { let capture_nums, body = Util.unsnoc $1 in

    let capture_nums' = capture_nums
      |. Belt.List.map (function
        | SingleCapturePattern n -> PatternCapture n
        | OperatorPattern
          ("var", [NumberedScopePattern ([], SingleCapturePattern n)])
        -> VarCapture n
        | OperatorPattern _ -> failwith "TODO: message 1"
      )
    in

    NumberedScopePattern (capture_nums', body)
  }

operator_match__test: operator_match EOF { $1 }

box_formatting_options:
  /* TODO: using NONTERMINAL_ID here is a bit of a hack */
  LEFT_ANGLE NONTERMINAL_ID separated_list(COMMA, NAT) RIGHT_ANGLE
  { let box_type = match $2 with
      | "h" -> HBox
      | "v" -> VBox
      | "hov" -> HovBox
      | "b" -> BBox
      | "hv" -> HvBox
      | _ -> failwith "TODO: error"
    in
    box_type, $3
  }

nonterminal_token:
  | LEFT_BRACKET box_formatting_options?
  { OpenBox $2 }
  | RIGHT_BRACKET   { CloseBox           }
  | TERMINAL_ID     { TerminalName    $1 }
  | NONTERMINAL_ID  { NonterminalName $1 }
  (* remove? *)
  | UNDERSCORE NAT? { Underscore (Belt.Option.getWithDefault $2 1) }

nonterminal_token__test: nonterminal_token EOF { $1 }
