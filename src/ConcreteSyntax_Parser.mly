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
%token SEMICOLON
%token UNDERSCORE
%token COMMA

%{open ConcreteSyntaxDescription

  (*
let concretize_vars : Core_kernel.String.Set.t -> Types.sort -> Types.sort
  = fun var_set ->
    let open Types in
    let rec go = function
      | SortAp (name, args)
      -> SortAp (name, Core_kernel.Array.map args ~f:go)
      | SortVar name
      -> if Core_kernel.String.Set.mem var_set name
         then SortVar name
         else SortAp (name, [||])
    in go
    *)
%}

%start terminal_rule__test
%start capture_number
%start nonterminal_token__test
%start operator_match__test
%start nonterminal_rule__test
%start language
%type <(string * Types.sort) list * Types.sort> args
%type <ConcreteSyntaxDescription.pre_terminal_rule> terminal_rule
%type <ConcreteSyntaxDescription.pre_terminal_rule> terminal_rule__test
%type <ConcreteSyntaxDescription.capture_number> capture_number
%type <ConcreteSyntaxDescription.nonterminal_token> nonterminal_token
%type <ConcreteSyntaxDescription.nonterminal_token> nonterminal_token__test
%type <ConcreteSyntaxDescription.operator_match> operator_match
%type <ConcreteSyntaxDescription.operator_match> operator_match__test
%type <ConcreteSyntaxDescription.nonterminal_rule> nonterminal_rule
%type <ConcreteSyntaxDescription.nonterminal_rule> nonterminal_rule__test
%type <ConcreteSyntaxDescription.operator_match_pattern> operator_match_pattern
%type <ConcreteSyntaxDescription.pre_terminal_rule list * ConcreteSyntaxDescription.nonterminal_rule list> language
%%

language: terminal_rule+ nonterminal_rule+ EOF { ($1, $2) }

terminal_rule:
  | TERMINAL_ID ASSIGN REGEX
  { PreTerminalRule ($1, First $3) }
  | TERMINAL_ID ASSIGN STRING
  { PreTerminalRule ($1, Second $3) }

terminal_rule__test: terminal_rule EOF { $1 }

capture_number: DOLLAR NAT { $2 }

nonterminal_rule__test: nonterminal_rule EOF { $1 }

/* TODO: duplicated (sort / atomic_sort) */
sort:
  /* TODO: this is ugly -- should be called SORT_ID or ID */
  | NONTERMINAL_ID nonempty_list(atomic_sort)
  { Types.SortAp ($1, Core_kernel.Array.of_list $2) }
  | atomic_sort
  { $1 }

atomic_sort:
  | LEFT_PAREN sort RIGHT_PAREN
  { $2 }
  | NONTERMINAL_ID
  /* TODO: this is ugly -- should be called SORT_ID or ID */
  { Types.SortVar $1 }

arg: LEFT_PAREN NONTERMINAL_ID COLON sort RIGHT_PAREN { $2, $4 }

args: list(arg) COLON sort { $1, $3 }

nonterminal_rule:
  | NONTERMINAL_ID args? ASSIGN BAR? separated_nonempty_list(BAR, operator_match)
  (* XXX must concretize vars *)
  { let args, result_type = match $2 with
      | None -> [], None
      | Some (args_ty, result_ty) -> args_ty, Some result_ty
    in
    NonterminalRule
    { nonterminal_name = $1
    ; args
    ; result_type
    ; operator_rules = $5
    }
  }

operator_match:
  | nonempty_list(nonterminal_token)
    LEFT_BRACE operator_match_pattern RIGHT_BRACE
  { OperatorMatch { tokens = $1; operator_match_pattern = $3 } }

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
      |> Core_kernel.List.map ~f:(function
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
      | "hv" -> HvBox
      | name -> failwith ("Unknown box type: " ^ name)
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
  | UNDERSCORE NAT? { Underscore (Core_kernel.Option.value $2 ~default:1) }

nonterminal_token__test: nonterminal_token EOF { $1 }
