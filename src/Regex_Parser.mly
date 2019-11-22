%token <string> CHARS
%token <string> CHARACTER_SET
%token <string> CHARACTER_CLASS
%token <string> ESCAPED
%token LEFT_PAREN
%token RIGHT_PAREN
%token DOT
%token BAR
%token STAR
%token PLUS
%token QUESTION
%token EOF

%start regex
%start regex__test
%start prec0_re
%type <Regex.re_class> re_class
%type <Regex.t> regex
%type <Regex.t> regex__test
%type <Regex.t> prec0_re
%%

re_class: CHARACTER_CLASS { match $1 with
  | {|\w|} -> PosClass Word
  | {|\s|} -> PosClass Whitespace
  | {|\d|} -> PosClass Digit
  | {|\b|} -> PosClass Boundary
  | {|\W|} -> NegClass Word
  | {|\S|} -> NegClass Whitespace
  | {|\D|} -> NegClass Digit
  | {|\B|} -> NegClass Boundary
  | _ -> failwith "unexpected character class"
  }

prec0_re:
  | prec0_re BAR prec0_re { ReChoice ($1, $3) }
  | prec1_re { $1 }

prec1_re:
  | nonempty_list(prec2_re) { match $1 with
    | [ re ] -> re
    | res -> ReConcat res
  }

prec2_re:
  | CHARACTER_SET { ReSet $1 }
  | CHARS { ReString $1 }
  | re_class { ReClass $1 }
  (* Remove the escape character, leaving only the unescaped content *)
  | ESCAPED { ReString (String.sub $1 1 1) }
  | prec3_re STAR { ReStar $1 }
  | prec3_re PLUS { RePlus $1 }
  | prec3_re QUESTION { ReOption $1 }
  | prec3_re { $1 }
  | DOT { ReAny }

prec3_re: LEFT_PAREN prec0_re RIGHT_PAREN { $2 }

regex: prec0_re { $1 }

regex__test: regex EOF { $1 }
