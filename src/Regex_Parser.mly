%token <char> CHAR
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_PAREN
%token RIGHT_PAREN
%token BACKSLASH
%token DOT
%token BAR
%token STAR
%token PLUS
%token QUESTION
%token DASH
%token EOF

%start regex
%type <Regex.re_set> character_set
%type <Regex.set_member> character_set_elem
%type <Regex.re_class> re_class
%type <Regex.t> regex
%type <Regex.t> prec0_re
%type <Regex.t> prec1_re
%type <Regex.t> prec2_re
%type <Regex.t> prec3_re
%%

re_class: BACKSLASH CHAR { match $2 with
  | 'w' -> PosClass Word
  | 's' -> PosClass Whitespace
  | 'd' -> PosClass Digit
  | 'b' -> PosClass Boundary
  | 'W' -> NegClass Word
  | 'S' -> NegClass Whitespace
  | 'D' -> NegClass Digit
  | 'B' -> NegClass Boundary
  | c -> failwith (Printf.sprintf "unexpected character class %c" c)
  }

character_set_elem:
  | char
  { SingleCharacter $1 }
  | char DASH char
  { Range ($1, $3) }

character_set: list(character_set_elem) { $1 }

prec0_re: separated_nonempty_list(BAR, prec1_re)
  { match $1 with
    | [ re ] -> re
    | res -> Regex.ReChoice res
  }

prec1_re: nonempty_list(prec2_re)
  { match $1 with
    | [ re ] -> re
    | res -> Regex.ReConcat res
  }

(* TODO: same escapes valid inside and out of char sets? *)
char:
  | CHAR { $1 }
  | BACKSLASH LEFT_BRACKET { '[' }
  | BACKSLASH RIGHT_BRACKET { ']' }
  | BACKSLASH LEFT_PAREN { '(' }
  | BACKSLASH RIGHT_PAREN { ')' }
  | BACKSLASH BACKSLASH { '\\' }
  | BACKSLASH DOT { '.' }
  | BACKSLASH BAR { '|' }
  | BACKSLASH STAR { '*' }
  | BACKSLASH PLUS { '+' }
  | BACKSLASH QUESTION { '?' }

string: nonempty_list(char) { Core_kernel.String.of_char_list $1 }

prec2_re:
  | LEFT_BRACKET character_set RIGHT_BRACKET { ReSet $2 }
  | string { ReString $1 }
  | re_class { ReClass $1 }
  (* Remove the escape character, leaving only the unescaped content *)
  | prec2_re STAR { ReStar $1 }
  | prec2_re PLUS { RePlus $1 }
  | prec2_re QUESTION { ReOption $1 }
  | prec3_re { $1 }
  | DOT { Regex.ReAny }

prec3_re: LEFT_PAREN prec0_re RIGHT_PAREN { $2 }

regex: prec0_re EOF { $1 }
