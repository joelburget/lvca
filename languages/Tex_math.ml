(** This doesn't even come close to understanding all of TeX. It limits itself to a
    reasonable subset of mathematical TeX. *)
open Base

open Lvca_syntax
module List_model = Lvca_core.List_model.List

module Lang =
[%lvca.abstract_syntax_module
{|
string : *  // module Primitive.String
char : *  // module Primitive.Char
list : * -> *  // module List_model

tex :=
  | Control_seq(string)
  | Token(char)
  // | Literal(string)
  | Grouped(list tex)
  | Space()
|}]

include Lang

module Supported = struct
  let accent =
    [ "\\Overrightarrow"
    ; "\\acute"
    ; "\\bar"
    ; "\\breve"
    ; "\\check"
    ; "\\ddot"
    ; "\\dot"
    ; "\\grave"
    ; "\\hat"
    ; "\\mathring"
    ; "\\overgroup"
    ; "\\overleftarrow"
    ; "\\overleftharpoon"
    ; "\\overleftrightarrow"
    ; "\\overlinesegment"
    ; "\\overrightarrow"
    ; "\\overrightharpoon"
    ; "\\tilde"
    ; "\\vec"
    ; "\\widecheck"
    ; "\\widehat"
    ; "\\widetilde"
    ]
  ;;

  let accent_under =
    [ "\\underleftarrow"
    ; "\\underrightarrow"
    ; "\\underleftrightarrow"
    ; "\\undergroup"
    ; "\\underlinesegment"
    ; "\\utilde"
    ]
  ;;

  let arrow =
    [ "\\xleftarrow"
    ; "\\xrightarrow"
    ; "\\xLeftarrow"
    ; "\\xRightarrow"
    ; "\\xleftrightarrow"
    ; "\\xLeftrightarrow"
    ; "\\xhookleftarrow"
    ; "\\xhookrightarrow"
    ; "\\xmapsto"
    ; "\\xrightharpoondown"
    ; "\\xrightharpoonup"
    ; "\\xleftharpoondown"
    ; "\\xleftharpoonup"
    ; "\\xrightleftharpoons"
    ; "\\xleftrightharpoons"
    ; "\\xlongequal"
    ; "\\xtwoheadrightarrow"
    ; "\\xtwoheadleftarrow"
    ; "\\xtofrom"
    ]
  ;;

  let color = [ "\\textcolor"; "\\color"; "\\colorbox"; "\\fcolorbox" ]
  let cr = [ {|\\|} ]

  let delim_sizing =
    [ "\\bigl"
    ; "\\Bigl"
    ; "\\biggl"
    ; "\\Biggl"
    ; "\\bigr"
    ; "\\Bigr"
    ; "\\biggr"
    ; "\\Biggr"
    ; "\\bigm"
    ; "\\Bigm"
    ; "\\biggm"
    ; "\\Biggm"
    ; "\\big"
    ; "\\Big"
    ; "\\bigg"
    ; "\\Bigg"
    ]
  ;;

  let delimiters =
    [ "("
    ; "\\lparen"
    ; ")"
    ; "\\rparen"
    ; "["
    ; "\\lbrack"
    ; "]"
    ; "\\rbrack"
    ; "\\{"
    ; "\\lbrace"
    ; "\\}"
    ; "\\rbrace"
    ; "\\lfloor"
    ; "\\rfloor"
      (* ; "\u230a" *)
      (* ; "\u230b" *)
    ; "\\lceil"
    ; "\\rceil"
      (* ; "\u2308" *)
      (* ; "\u2309" *)
    ; "<"
    ; ">"
    ; "\\langle" (* ; "\u27e8" *)
    ; "\\rangle" (* ; "\u27e9" *)
    ; "\\lt"
    ; "\\gt"
    ; "\\lvert"
    ; "\\rvert"
    ; "\\lVert"
    ; "\\rVert"
    ; "\\lgroup"
    ; "\\rgroup"
      (* ; "\u27ee" *)
      (* ; "\u27ef" *)
    ; "\\lmoustache"
    ; "\\rmoustache"
      (* ; "\u23b0" *)
      (* ; "\u23b1" *)
    ; "/"
    ; "\\backslash"
    ; "|"
    ; "\\vert"
    ; "\\|"
    ; "\\Vert"
    ; "\\uparrow"
    ; "\\Uparrow"
    ; "\\downarrow"
    ; "\\Downarrow"
    ; "\\updownarrow"
    ; "\\Updownarrow"
    ; "."
    ]
  ;;

  let direction = [ "\\left"; "\\middle"; "\\right" ]

  let font =
    [ "\\Bbb"
    ; "\\bf"
    ; "\\bm"
    ; "\\bold"
    ; "\\boldsymbol"
    ; "\\cal"
    ; "\\frak"
    ; "\\it"
    ; "\\mathbb"
    ; "\\mathbf"
    ; "\\mathcal"
    ; "\\mathfrak"
    ; "\\mathit"
    ; "\\mathnormal"
    ; "\\mathrm"
    ; "\\mathscr"
    ; "\\mathsf"
    ; "\\mathtt"
    ; "\\rm"
    ; "\\sf"
    ; "\\tt"
    ]
  ;;

  let genfrac =
    [ "\\above"
    ; "\\atop"
    ; "\\binom"
    ; "\\brace"
    ; "\\brack"
    ; "\\cfrac"
    ; "\\choose"
    ; "\\dbinom"
    ; "\\dfrac"
    ; "\\frac"
    ; "\\genfrac"
    ; "\\over"
    ; "\\tbinom"
    ; "\\tfrac"
    ]
  ;;

  let hbox = [ "\\hbox" ]
  let horiz_brace = [ "\\overbrace"; "\\underbrace" ]
  let kern = [ "\\kern"; "\\mkern"; "\\hskip"; "\\mskip" ]
  let lap = [ "\\mathllap"; "\\mathrlap"; "\\mathclap" ]

  let mclass =
    [ "\\mathbin"
    ; "\\mathclose"
    ; "\\mathinner"
    ; "\\mathopen"
    ; "\\mathord"
    ; "\\mathpunct"
    ; "\\mathrel"
    ; "\\overset"
    ; "\\stackrel"
    ; "\\underset"
    ]
  ;;

  let op =
    [ "\\Pr"
    ; "\\arccos"
    ; "\\arcctg"
    ; "\\arcsin"
    ; "\\arctan"
    ; "\\arctg"
    ; "\\arg"
    ; "\\bigcap"
    ; "\\bigcup"
    ; "\\bigodot"
    ; "\\bigoplus"
    ; "\\bigotimes"
    ; "\\bigsqcup"
    ; "\\biguplus"
    ; "\\bigvee"
    ; "\\bigwedge"
    ; "\\ch"
    ; "\\coprod"
    ; "\\cos"
    ; "\\cosec"
    ; "\\cosh"
    ; "\\cot"
    ; "\\cotg"
    ; "\\coth"
    ; "\\csc"
    ; "\\ctg"
    ; "\\cth"
    ; "\\deg"
    ; "\\det"
    ; "\\dim"
    ; "\\exp"
    ; "\\gcd"
    ; "\\hom"
    ; "\\iiint"
    ; "\\iint"
    ; "\\inf"
    ; "\\int"
    ; "\\intop"
    ; "\\ker"
    ; "\\lg"
    ; "\\lim"
    ; "\\ln"
    ; "\\log"
    ; "\\mathop"
    ; "\\max"
    ; "\\min"
    ; "\\oiiint"
    ; "\\oiint"
    ; "\\oint"
    ; "\\prod"
    ; "\\sec"
    ; "\\sh"
    ; "\\sin"
    ; "\\sinh"
    ; "\\smallint"
    ; "\\sum"
    ; "\\sup"
    ; "\\tan"
    ; "\\tanh"
    ; "\\tg"
    ; "\\th"
      (*
    ; "\\u220F"
    ; "\\u2210"
    ; "\\u2211"
    ; "\\u222b"
    ; "\\u222c"
    ; "\\u222d"
    ; "\\u222e"
    ; "\\u222f"
    ; "\\u2230"
    ; "\\u22c0"
    ; "\\u22c1"
    ; "\\u22c2"
    ; "\\u22c3"
    ; "\\u2a00"
    ; "\\u2a01"
    ; "\\u2a02"
    ; "\\u2a04"
    ; "\\u2a06"
         *)
    ]
  ;;

  let operatorname = [ "\\operatorname"; "\\operatorname*"; "\\operatornamewithlimits" ]

  let binop =
    [ "*"
    ; "+"
    ; "-"
    ; "/"
    ; "\\And"
    ; "\\Cap"
    ; "\\Cup"
    ; "\\amalg"
    ; "\\ast"
    ; "\\barwedge"
    ; "\\bigcirc"
    ; "\\bmod"
    ; "\\boxdot"
    ; "\\boxminus"
    ; "\\boxplus"
    ; "\\boxtimes"
    ; "\\bullet"
    ; "\\cap"
    ; "\\cdot"
    ; "\\cdotp"
    ; "\\centerdot"
    ; "\\circ"
    ; "\\circledast"
    ; "\\circledcirc"
    ; "\\circleddash"
    ; "\\cup"
    ; "\\curlyvee"
    ; "\\curlywedge"
    ; "\\div"
    ; "\\divideontimes"
    ; "\\dotplus"
    ; "\\doublebarwedge"
    ; "\\doublecap"
    ; "\\doublecup"
    ; "\\gtrdot"
    ; "\\intercal"
    ; "\\land"
    ; "\\ldotp"
    ; "\\leftthreetimes"
    ; "\\lessdot"
    ; "\\lhd"
    ; "\\lor"
    ; "\\ltimes"
    ; "\\mod"
    ; "\\mp"
    ; "\\odot"
    ; "\\ominus"
    ; "\\oplus"
    ; "\\oslash"
    ; "\\otimes"
    ; "\\plusmn"
    ; "\\pm"
    ; "\\pmod"
    ; "\\pod"
    ; "\\rhd"
    ; "\\rightthreetimes"
    ; "\\rtimes"
    ; "\\setminus"
    ; "\\smallsetminus"
    ; "\\sqcap"
    ; "\\sqcup"
    ; "\\times"
    ; "\\unlhd"
    ; "\\unrhd"
    ; "\\uplus"
    ; "\\vee"
    ; "\\veebar"
    ; "\\wedge"
    ; "\\wr"
    ]
  ;;

  let relations =
    [ ":"
    ; "<"
    ; "="
    ; ">"
    ; "\\Bumpeq"
    ; "\\Colonapprox"
    ; "\\Coloneq"
    ; "\\Coloneqq"
    ; "\\Colonsim"
    ; "\\Doteq"
    ; "\\Eqcolon"
    ; "\\Eqcolon"
    ; "\\Join"
    ; "\\Join"
    ; "\\Subset"
    ; "\\Supset"
    ; "\\Vdash"
    ; "\\Vvdash"
    ; "\\approx"
    ; "\\approxcolon"
    ; "\\approxcoloncolon"
    ; "\\approxeq"
    ; "\\asymp"
    ; "\\backepsilon"
    ; "\\backsim"
    ; "\\backsimeq"
    ; "\\between"
    ; "\\bowtie"
    ; "\\bumpeq"
    ; "\\circeq"
    ; "\\colonapprox"
    ; "\\coloncolon"
    ; "\\coloncolonapprox"
    ; "\\coloncolonequals"
    ; "\\coloncolonminus"
    ; "\\coloncolonsim"
    ; "\\coloneq"
    ; "\\coloneqq"
    ; "\\colonequals"
    ; "\\colonminus"
    ; "\\colonsim"
    ; "\\cong"
    ; "\\curlyeqprec"
    ; "\\curlyeqsucc"
    ; "\\curlyeqsucc"
    ; "\\dashv"
    ; "\\dblcolon"
    ; "\\doteq"
    ; "\\doteqdot"
    ; "\\eqcirc"
    ; "\\eqcolon"
    ; "\\eqqcolon"
    ; "\\eqsim"
    ; "\\eqslantgtr"
    ; "\\eqslatless"
    ; "\\equalscolon"
    ; "\\equalscoloncolon"
    ; "\\equiv"
    ; "\\fallingdotseq"
    ; "\\frown"
    ; "\\ge"
    ; "\\geq"
    ; "\\geqq"
    ; "\\geqslant"
    ; "\\gg"
    ; "\\ggg"
    ; "\\gggtr"
    ; "\\gt"
    ; "\\gtrapprox"
    ; "\\gtreqless"
    ; "\\gtreqqless"
    ; "\\gtrless"
    ; "\\gtrsim"
    ; "\\imageof"
    ; "\\le"
    ; "\\leq"
    ; "\\leqq"
    ; "\\leqslant"
    ; "\\lessapprox"
    ; "\\lesseqgtr"
    ; "\\lesseqqgtr"
    ; "\\lessgtr"
    ; "\\lesssim"
    ; "\\ll"
    ; "\\lll"
    ; "\\llless"
    ; "\\lt"
    ; "\\mid"
    ; "\\minuscolon"
    ; "\\minuscoloncolon"
    ; "\\models"
    ; "\\multimap"
    ; "\\origof"
    ; "\\owns"
    ; "\\parallel"
    ; "\\perp"
    ; "\\pitchfork"
    ; "\\prec"
    ; "\\precapprox"
    ; "\\preccurlyeq"
    ; "\\preceq"
    ; "\\precsim"
    ; "\\propto"
    ; "\\ratio"
    ; "\\ratio"
    ; "\\risingdotseq"
    ; "\\shortmid"
    ; "\\shortparallel"
    ; "\\sim"
    ; "\\simcolon"
    ; "\\simcolon"
    ; "\\simcoloncolon"
    ; "\\simeq"
    ; "\\smallfrown"
    ; "\\smallsmile"
    ; "\\smile"
    ; "\\sqsubset"
    ; "\\sqsubseteq"
    ; "\\sqsupset"
    ; "\\sqsupseteq"
    ; "\\sub"
    ; "\\sube"
    ; "\\subset"
    ; "\\subseteq"
    ; "\\subseteqq"
    ; "\\succ"
    ; "\\succapprox"
    ; "\\succcurlyeq"
    ; "\\succeq"
    ; "\\succsim"
    ; "\\supe"
    ; "\\supset"
    ; "\\supseteq"
    ; "\\supseteqq"
    ; "\\thickapprox"
    ; "\\thicksim"
    ; "\\tranglerighteq"
    ; "\\triangleeq"
    ; "\\trianglelefteq"
    ; "\\vDash"
    ; "\\varpropto"
    ; "\\vartriangle"
    ; "\\vartriangleleft"
    ; "\\vartriangleright"
    ; "\\vcentcolon"
    ; "\\vcentcolon"
    ; "\\vdash"
    ]
  ;;

  let arrows =
    [ "\\Darr"
    ; "\\Downarrow"
    ; "\\Harr"
    ; "\\Larr"
    ; "\\Leftarrow"
    ; "\\Leftrightarrow"
    ; "\\Lleftarrow"
    ; "\\Longleftarrow"
    ; "\\Longleftrightarrow"
    ; "\\Longrightarrow"
    ; "\\Lrarr"
    ; "\\Lsh"
    ; "\\Rarr"
    ; "\\Rightarrow"
    ; "\\Rrightarrow"
    ; "\\Rsh"
    ; "\\Uarr"
    ; "\\Uparrow"
    ; "\\Updownarrow"
    ; "\\circlearrowleft"
    ; "\\circlearrowright"
    ; "\\curvearrowleft"
    ; "\\curvearrowright"
    ; "\\dArr"
    ; "\\darr"
    ; "\\dashleftarrow"
    ; "\\dashrightarrow"
    ; "\\downarrow"
    ; "\\downdownarrows"
    ; "\\downharpoonleft"
    ; "\\downharpoonright"
    ; "\\gets"
    ; "\\hArr"
    ; "\\harr"
    ; "\\hookleftarrow"
    ; "\\hookrightarrow"
    ; "\\iff"
    ; "\\impliedby"
    ; "\\implies"
    ; "\\lArr"
    ; "\\larr"
    ; "\\leadsto"
    ; "\\leftarrow"
    ; "\\leftarrowtail"
    ; "\\leftharpoondown"
    ; "\\leftharpoonup"
    ; "\\leftleftarrows"
    ; "\\leftrightarrow"
    ; "\\leftrightarrows"
    ; "\\leftrightharpoons"
    ; "\\leftrightsquigarrow"
    ; "\\longleftarrow"
    ; "\\longleftrightarrow"
    ; "\\longmapsto"
    ; "\\longrightarrow"
    ; "\\looparrowleft"
    ; "\\looparrowright"
    ; "\\lrArr"
    ; "\\lrarr"
    ; "\\mapsto"
    ; "\\nLeftarrow"
    ; "\\nLeftrightarrow"
    ; "\\nRightarrow"
    ; "\\nearrow"
    ; "\\nleftarrow"
    ; "\\nleftrightarrow"
    ; "\\nrightarrow"
    ; "\\nwarrow"
    ; "\\rArr"
    ; "\\rarr"
    ; "\\restriction"
    ; "\\rightarrow"
    ; "\\rightarrowtail"
    ; "\\rightharpoondown"
    ; "\\rightharpoonup"
    ; "\\rightleftarrows"
    ; "\\rightleftharpoons"
    ; "\\rightrightarrows"
    ; "\\rightsquigarrow"
    ; "\\searrow"
    ; "\\swarrow"
    ; "\\to"
    ; "\\twoheadleftarrow"
    ; "\\twoheadrightarrow"
    ; "\\uArr"
    ; "\\uarr"
    ; "\\uparrow"
    ; "\\updownarrow"
    ; "\\upharpoonleft"
    ; "\\upharpoonright"
    ; "\\upuparrows"
    ]
  ;;

  let extensible_arrows =
    [ "\\xLeftarrow"
    ; "\\xLeftrightarrow"
    ; "\\xRightarrow"
    ; "\\xhookleftarrow"
    ; "\\xhookrightarrow"
    ; "\\xleftarrow"
    ; "\\xleftharpoondown"
    ; "\\xleftharpoonup"
    ; "\\xleftrightarrow"
    ; "\\xleftrightharpoons"
    ; "\\xlongequal"
    ; "\\xmapsto"
    ; "\\xrightarrow"
    ; "\\xrightharpoondown"
    ; "\\xrightharpoonup"
    ; "\\xrightleftharpoons"
    ; "\\xtofrom"
    ; "\\xtwoheadleftarrow"
    ; "\\xtwoheadrightarrow"
    ]
  ;;

  let braket = [ "\\bra"; "\\ket"; "\\Bra"; "\\Ket"; "\\braket" ]

  let punctuation =
    [ "--"
    ; "---"
    ; "\""
    ; "\\#"
    ; "\\$"
    ; "\\%"
    ; "\\&"
    ; "\\Box"
    ; "\\Dagger"
    ; "\\Diamond"
    ; "\\KaTex"
    ; "\\LaTex"
    ; "\\P"
    ; "\\S"
    ; "\\TeX"
    ; "\\_"
    ; "\\angle"
    ; "\\backprime"
    ; "\\bigstar"
    ; "\\bigtriangledown"
    ; "\\bigtriangleup"
    ; "\\blacklozenge"
    ; "\\blacksquare"
    ; "\\blacktriangle"
    ; "\\blacktriangledown"
    ; "\\blacktriangleleft"
    ; "\\blacktriangleright"
    ; "\\bot"
    ; "\\cdots"
    ; "\\checkmark"
    ; "\\circledR"
    ; "\\circledS"
    ; "\\clubs"
    ; "\\clubsuit"
    ; "\\colon"
    ; "\\copyright"
    ; "\\dag"
    ; "\\dagger"
    ; "\\ddag"
    ; "\\ddagger"
    ; "\\ddots"
    ; "\\degree"
    ; "\\diagdown"
    ; "\\diagup"
    ; "\\diamond"
    ; "\\diamonds"
    ; "\\diamondsuit"
    ; "\\dots"
    ; "\\dotsb"
    ; "\\dotsc"
    ; "\\dotsi"
    ; "\\dotsm"
    ; "\\dotso"
    ; "\\flat"
    ; "\\hearts"
    ; "\\heartsuit"
    ; "\\infin"
    ; "\\infty"
    ; "\\ldots"
    ; "\\lozenge"
    ; "\\lq"
    ; "\\maltese"
    ; "\\mathellipsis"
    ; "\\mathsterling"
    ; "\\measuredangle"
    ; "\\mho"
    ; "\\minuso"
    ; "\\nabla"
    ; "\\natural"
    ; "\\pounds"
    ; "\\prime"
    ; "\\rq"
    ; "\\sdot"
    ; "\\sect"
    ; "\\sharp"
    ; "\\spades"
    ; "\\spadesuit"
    ; "\\sphericalangle"
    ; "\\square"
    ; "\\star"
    ; "\\surd"
    ; "\\textasciicircum"
    ; "\\textasciitilde"
    ; "\\textbackslash"
    ; "\\textbar"
    ; "\\textbardbl"
    ; "\\textbraceleft"
    ; "\\textbraceright"
    ; "\\textcircled"
    ; "\\textdagger"
    ; "\\textdaggerdbl"
    ; "\\textdegree"
    ; "\\textdollar"
    ; "\\textellipsis"
    ; "\\textemdash"
    ; "\\textendash"
    ; "\\textgreater"
    ; "\\textless"
    ; "\\textquotedblleft"
    ; "\\textquotedblright"
    ; "\\textquoteleft"
    ; "\\textquoteright"
    ; "\\textregistered"
    ; "\\textsterling"
    ; "\\textunderscore"
    ; "\\top"
    ; "\\triangle"
    ; "\\triangledown"
    ; "\\triangleleft"
    ; "\\triangleright"
    ; "\\vdots"
    ; "\\yen"
    ; "`"
    ]
  ;;

  let overline = [ "\\overline" ]
  let phantom = [ "\\phantom"; "\\hphantom"; "\\vphantom" ]
  let raisebox = [ "\\raisebox" ]
  let rule = [ "\\rule" ]

  let sizing =
    [ "\\tiny"
    ; "\\sixptsize"
    ; "\\scriptsize"
    ; "\\footnotesize"
    ; "\\small"
    ; "\\normalsize"
    ; "\\large"
    ; "\\Large"
    ; "\\LARGE"
    ; "\\huge"
    ; "\\Huge"
    ]
  ;;

  let smash = [ "\\smash" ]
  let sqrt = [ "\\sqrt" ]

  let styling =
    [ "\\displaystyle"; "\\textstyle"; "\\scriptstyle"; "\\scriptscriptstyle" ]
  ;;

  let subsup = [ "_"; "^" ]

  let text =
    [ "\\text"
    ; "\\textrm"
    ; "\\textsf"
    ; "\\texttt"
    ; "\\textnormal"
    ; "\\textbf"
    ; "\\textmd"
    ; "\\textit"
    ; "\\textup"
    ]
  ;;

  let underline = [ "\\underline" ]
  let vcenter = [ "\\vcenter" ]
  let verb = [ "\\verb" ]

  let letters =
    [ "\\Alpha"
    ; "\\Beta"
    ; "\\Gamma"
    ; "\\Delta"
    ; "\\Epsilon"
    ; "\\Zeta"
    ; "\\Eta"
    ; "\\Theta"
    ; "\\Iota"
    ; "\\Kappa"
    ; "\\Lambda"
    ; "\\Mu"
    ; "\\Nu"
    ; "\\Xi"
    ; "\\Omicron"
    ; "\\Pi"
    ; "\\Rho"
    ; "\\Sigma"
    ; "\\Tau"
    ; "\\Upsilon"
    ; "\\Phi"
    ; "\\Chi"
    ; "\\Psi"
    ; "\\Omega"
    ; "\\varGamma"
    ; "\\varDelta"
    ; "\\varTheta"
    ; "\\varLambda"
    ; "\\varXi"
    ; "\\varPi"
    ; "\\varSigma"
    ; "\\varUpsilon"
    ; "\\varPhi"
    ; "\\varPsi"
    ; "\\varOmega"
    ; "\\alpha"
    ; "\\beta"
    ; "\\gamma"
    ; "\\delta"
    ; "\\epsilon"
    ; "\\zeta"
    ; "\\eta"
    ; "\\theta"
    ; "\\iota"
    ; "\\kappa"
    ; "\\lambda"
    ; "\\mu"
    ; "\\nu"
    ; "\\xi"
    ; "\\omicron"
    ; "\\pi"
    ; "\\rho"
    ; "\\sigma"
    ; "\\tau"
    ; "\\upsilon"
    ; "\\phi"
    ; "\\chi"
    ; "\\psi"
    ; "\\omega"
    ; "\\varepsilon"
    ; "\\varkappa"
    ; "\\vartheta"
    ; "\\thetasym"
    ; "\\varpi"
    ; "\\varrho"
    ; "\\varsigma"
    ; "\\varphi"
    ; "\\digamma"
    ; "\\imath"
    ; "\\nabla"
    ; "\\Im"
    ; "\\Reals"
    ; "\\OE"
    ; "\\jmath"
    ; "\\partial"
    ; "\\image"
    ; "\\wp"
    ; "\\o"
    ; "\\aleph"
    ; "\\Game"
    ; "\\Bbbk"
    ; "\\weierp"
    ; "\\O"
    ; "\\alef"
    ; "\\Finv"
    ; "\\N"
    ; "\\Z"
    ; "\\ss"
    ; "\\alefsym"
    ; "\\cnums"
    ; "\\natnums"
    ; "\\aa"
    ; "\\i"
    ; "\\beth"
    ; "\\Complex"
    ; "\\R"
    ; "\\AA"
    ; "\\j"
    ; "\\gimel"
    ; "\\ell"
    ; "\\Re"
    ; "\\ae"
    ; "\\daleth"
    ; "\\hbar"
    ; "\\real"
    ; "\\AE"
    ; "\\eth"
    ; "\\hslash"
    ; "\\reals"
    ; "\\oe"
    ]
  ;;

  let space =
    [ "\\,"
    ; "\\thinspace"
    ; "\\>"
    ; "\\:"
    ; "\\medspace"
    ; "\\;"
    ; "\\thickspace"
    ; "\\enspace"
    ; "\\quad"
    ; "\\qquad"
    ; "~"
    ; "\\nobreakspace"
    ; "\\space"
    ; "\\kern"
    ; "\\mkern"
    ; "\\mskip"
    ; "\\hskip"
    ; "\\hspace"
    ; "\\hspace*"
    ; "\\!"
    ; "\\negthinspace"
    ; "\\negmedspace"
    ; "\\negthickspace"
    ; "\\mathstrut"
    ]
  ;;

  let foundations =
    [ "\\forall"
    ; "\\complement"
    ; "\\therefore"
    ; "\\emptyset"
    ; "\\exists"
    ; "\\subset"
    ; "\\because"
    ; "\\empty"
    ; "\\exist"
    ; "\\supset"
    ; "\\mapsto"
    ; "\\varnothing"
    ; "\\nexists"
    ; "\\mid"
    ; "\\to"
    ; "\\implies"
    ; "\\in"
    ; "\\land"
    ; "\\gets"
    ; "\\impliedby"
    ; "\\isin"
    ; "\\lor"
    ; "\\leftrightarrow"
    ; "\\iff"
    ; "\\notin"
    ; "\\ni"
    ; "\\notni"
    ; "\\neg"
    ; "\\lnot"
    ]
  ;;

  let misc = [ "\\begingroup"; "\\endgroup" ]

  let all =
    Lvca_util.String.Set.of_list
      (accent
      @ accent_under
      @ arrow
      @ color
      @ cr
      @ delim_sizing
      @ delimiters
      @ direction
      @ font
      @ genfrac
      @ hbox
      @ horiz_brace
      @ kern
      @ lap
      @ mclass
      @ op
      @ overline
      @ phantom
      @ raisebox
      @ rule
      @ sizing
      @ smash
      @ sqrt
      @ styling
      @ subsup
      @ text
      @ underline
      @ vcenter
      @ verb
      @ letters
      @ space
      @ foundations
      @ operatorname
      @ binop
      @ relations
      @ arrows
      @ extensible_arrows
      @ braket
      @ punctuation
      @ misc)
  ;;

  let is_valid_control_seq str = Set.mem all str
end

let rec check tm =
  match tm with
  | Tex.Control_seq (_, (_, str)) ->
    if Supported.is_valid_control_seq str
    then None
    else Some (Fmt.str "%s is not a known control sequence" str)
  | Grouped (_, list) -> list |> Lvca_core.List_model.to_list |> List.find_map ~f:check
  | Token _ | Space _ -> None
;;

(* TODO: remove? *)
let trim_spaces tex_list =
  let go = List.drop_while ~f:(function Tex.Plain.Space -> true | _ -> false) in
  List.(tex_list |> go |> rev |> go |> rev)
;;

let starts_with ~f str =
  if String.length str = 0
  then false
  else (
    let c = String.get str 0 in
    f c)
;;

let ends_with ~f str =
  if String.length str = 0
  then false
  else (
    let c = String.get str (String.length str - 1) in
    f c)
;;

let is_ascii c = Char.to_int c < 128
let rec to_list = function List_model.Plain.Nil -> [] | Cons (x, xs) -> x :: to_list xs

(* characters with special meaning: & % $ # _ { } ~ ^ \ *)
let is_special_char c = String.mem {|&%$#_{}~^\|} c

let is_token_char c =
  let code = Char.to_int c in
  code >= 31 && code < 127 && not (is_special_char c)
;;

let is_control_seq_single_char = String.mem {|@ \,;:!-=><+'`|()[]{}#$%&_|}

let rec pp : Lang.Tex.Plain.t Fmt.t =
 fun ppf -> function
  | Token c ->
    Fmt.char ppf c
    (*
  | Literal str ->
    let str = if starts_with ~f:Char.is_alpha str then " " ^ str else str in
    Fmt.string ppf str
                    *)
  | Control_seq str ->
    let f c = Char.is_alphanum c || not (is_ascii c) in
    let str = if starts_with ~f str then " " ^ str else str in
    Fmt.string ppf str
  | Grouped texs ->
    (match texs with
    | Cons (Grouped texs, Nil) -> pp ppf (Grouped texs)
    | _ -> Fmt.pf ppf "{%a}" Fmt.(list pp) (to_list texs))
  | Space -> Fmt.string ppf " "
;;

let parse : Lvca_provenance.Opt_range.t Lang.Tex.t list Lvca_parsing.t =
  let open Lvca_parsing in
  let open Lang.Tex in
  let control_seq =
    No_ws.char '\\'
    >>= fun _ ->
    choice
      ~failure_msg:"control sequence"
      [ (many1 (No_ws.satisfy Char.is_alpha)
        >>~ fun range value ->
        Control_seq (range, (range, String.of_char_list ('\\' :: value))))
      ; (No_ws.satisfy is_control_seq_single_char
        >>~ fun range value ->
        Control_seq (range, (range, String.of_char_list [ '\\'; value ])))
      ]
  in
  let subsuptick =
    [ '_'; '^'; '\''; '~' ]
    |> List.map ~f:(fun c ->
           No_ws.char c >>~ fun range c -> Control_seq (range, (range, String.of_char c)))
    |> choice ~failure_msg:"looking for `_`, `^`, `'`, or `~`"
  in
  let space = whitespace1 >>~ fun range _ -> Space range in
  let token =
    No_ws.satisfy is_token_char >>~ fun range value -> Token (range, (range, value))
  in
  let atom =
    fix (fun expr ->
        let grouped =
          No_ws.braces (many1 expr)
          >>~ fun range value ->
          Grouped (range, Lvca_core.List_model.of_list ~empty_info:range value)
        in
        choice
          ~failure_msg:
            "looking for a control sequence, `_`, `^`, `'`, group, space, or token"
          [ control_seq <?> "control sequence"
          ; subsuptick <?> "`_`, `^`, `'`"
          ; grouped <?> "group"
          ; space <?> "space"
          ; token <?> "token"
          ])
    <?> "atom"
  in
  many1 atom <?> "Tex math expression"
;;

let literal i str =
  str |> String.to_list |> List.map ~f:(fun c -> Lang.Tex.Token (i, (i, c)))
;;

let%test_module _ =
  (module struct
    let margin = Stdlib.Format.(pp_get_margin std_formatter ())
    let () = Stdlib.Format.(pp_set_margin std_formatter 200)

    let parse_print str =
      match Lvca_parsing.(parse_string parse) str with
      | Ok tms -> Fmt.pr "%a\n" Fmt.(list ~sep:nop pp) (List.map tms ~f:Lang.Tex.to_plain)
      | Error msg -> Fmt.pr "%s\n" msg
    ;;

    let%expect_test _ =
      parse_print "1";
      parse_print "1 + 1";
      parse_print " ";
      parse_print "a";
      parse_print {|\alpha|};
      parse_print {|\sqrt{1}|};
      parse_print {|\sqrt 1|};
      parse_print {|\frac{1}{2}|};
      parse_print {|\frac 1 2|};
      parse_print {|x_1|};
      parse_print {|x_{1 + 1}|};
      parse_print {|x^1|};
      parse_print {|x^{1 + 1}|};
      parse_print {|x^\begingroup1 + 1\endgroup|};
      parse_print {|x_1^2|};
      parse_print {|x^1_2|};
      parse_print {|x'''|};
      parse_print {|x \over y|};
      parse_print
        {|\f\relax{x} = \int_{-\infty}^\infty
    \f\hat\xi\,e^{2 \pi i \xi x}
    \,d\xi|};
      [%expect
        {|
    1
    1 + 1

    a
    \alpha
    \sqrt{1}
    \sqrt 1
    \frac{1}{2}
    \frac 1 2
    x_1
    x_{1 + 1}
    x^1
    x^{1 + 1}
    x^\begingroup1 + 1\endgroup
    x_1^2
    x^1_2
    x'''
    x \over y
    \f\relax{x} = \int_{-\infty}^\infty \f\hat\xi\,e^{2 \pi i
    \xi
    x} \,d\xi
    |}]
    ;;

    let () = Stdlib.Format.(pp_set_margin std_formatter margin)
  end)
;;

let%test_module "control sequences" =
  (module struct
    let go str =
      match Lvca_parsing.(parse_string parse) str with
      | Ok [ tm ] ->
        (match check tm with None -> () | Some msg -> Fmt.pr "%S -> %s\n" str msg)
      | Ok _tms -> Fmt.pr "TODO: %S parsed to multiple control sequences\n" str
      | Error msg -> Fmt.pr "%s -> %s\n" str msg
    ;;

    let%expect_test _ =
      Supported.all |> Set.to_list |> List.iter ~f:go;
      [%expect
        {|
        TODO: "--" parsed to multiple control sequences
        TODO: "---" parsed to multiple control sequences
        TODO: "\\hspace*" parsed to multiple control sequences
        TODO: "\\operatorname*" parsed to multiple control sequences |}]
    ;;

    let%expect_test _ =
      go {|\misspelled|};
      go {|\*|};
      [%expect
        {|
        "\\misspelled" -> \misspelled is not a known control sequence
        \* -> Tex math expression > atom: looking for a control sequence, `_`, `^`, `'`, group, space, or token |}]
    ;;

    let%expect_test _ =
      let go str =
        match Lvca_parsing.(parse_string parse) str with
        | Ok [ tm ] ->
          let str' = Fmt.str "%a" pp (Lang.Tex.to_plain tm) in
          if String.(str' = str) then () else Fmt.pr "%s -> %s\n" str str'
        | Ok _tms -> Fmt.pr "TODO: %S parsed to multiple control sequences\n" str
        | Error msg -> Fmt.pr "%s -> %s\n" str msg
      in
      Supported.all |> Set.to_list |> List.iter ~f:go;
      [%expect
        {|
        TODO: "--" parsed to multiple control sequences
        TODO: "---" parsed to multiple control sequences
        TODO: "\\hspace*" parsed to multiple control sequences
        TODO: "\\operatorname*" parsed to multiple control sequences |}]
    ;;
  end)
;;
