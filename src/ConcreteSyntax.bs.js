// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Util = require("./Util.bs.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Jison = require("./Jison.bs.js");
var Types = require("./Types.bs.js");
var Bigint = require("bs-zarith/src/Bigint.js");
var Printf = require("bs-platform/lib/js/printf.js");
var $$String = require("bs-platform/lib/js/string.js");
var Js_dict = require("bs-platform/lib/js/js_dict.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Belt_Result = require("bs-platform/lib/js/belt_Result.js");
var Belt_MapString = require("bs-platform/lib/js/belt_MapString.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

var AA = Util.ArrayApplicative(/* module */[]);

function equivalent(t1, t2) {
  if (Caml_obj.caml_equal(t1[/* sort */0], t2[/* sort */0]) && Caml_obj.caml_equal(t1[/* node_type */1], t2[/* node_type */1])) {
    return Belt_Array.every(Belt_Array.zipBy(t1[/* children */4], t2[/* children */4], equivalent$prime), (function (b) {
                  return b;
                }));
  } else {
    return false;
  }
}

function equivalent$prime(child1, child2) {
  if (child1.tag) {
    if (child2.tag) {
      return equivalent(child1[0], child2[0]);
    } else {
      return false;
    }
  } else if (child2.tag) {
    return false;
  } else {
    return child1[0] === child2[0];
  }
}

function find_operator_match(matches, opname) {
  var maybeMatch = Util.find((function (param) {
          return param[0][/* term_pattern */1][0] === opname;
        }), matches);
  if (maybeMatch !== undefined) {
    return maybeMatch;
  } else {
    return Pervasives.failwith("TODO: default match");
  }
}

function find_subtm(param, param$1, param$2) {
  var _slot_num = 0;
  var token_ix = param;
  var _scopes = param$1;
  var _term_pattern = param$2;
  while(true) {
    var term_pattern = _term_pattern;
    var scopes = _scopes;
    var slot_num = _slot_num;
    if (term_pattern) {
      if (scopes) {
        var match = term_pattern[0];
        var match$1 = scopes[0];
        var match$2 = Util.find((function (param) {
                return param[1] === token_ix;
              }), Belt_List.zip(match$1[0], match[0]));
        if (match$2 !== undefined) {
          return /* FoundBinder */Block.__(1, [match$2[0]]);
        } else if (token_ix === match[1]) {
          return /* FoundTerm */Block.__(0, [
                    slot_num,
                    match$1[1]
                  ]);
        } else {
          _term_pattern = term_pattern[1];
          _scopes = scopes[1];
          _slot_num = slot_num + 1 | 0;
          continue ;
        }
      } else {
        return Pervasives.failwith("invariant violation: mismatched scopes / term patterns");
      }
    } else {
      return /* NotFound */0;
    }
  };
}

var BadRules = Caml_exceptions.create("ConcreteSyntax.BadRules");

var CantEmitTokenRegex = Caml_exceptions.create("ConcreteSyntax.CantEmitTokenRegex");

function regex_is_literal(param) {
  if (param) {
    var match = param[0];
    if (match.tag || param[1]) {
      return undefined;
    } else {
      return match[0];
    }
  }
  
}

function regex_piece_to_string(param) {
  switch (param.tag | 0) {
    case 0 : 
        return param[0].replace((/\+/g), "\\+").replace((/\*/g), "\\*").replace((/\?/g), "\\?").replace((/\-/g), "\\-");
    case 1 : 
        return "[" + (param[0] + "]");
    case 2 : 
        return regex_piece_to_string(param[0]) + "*";
    case 3 : 
        return regex_piece_to_string(param[0]) + "+";
    case 4 : 
        return regex_piece_to_string(param[0]) + "?";
    
  }
}

function regex_to_string(re_parts) {
  return $$String.concat("", List.map(regex_piece_to_string, re_parts));
}

function mk_tree(sort, node_type, children) {
  return /* record */[
          /* sort */sort,
          /* node_type */node_type,
          /* leading_trivia */"",
          /* trailing_trivia */"",
          /* children */children
        ];
}

function of_ast(lang, rules, current_sort, tm) {
  var terminal_rules = rules[/* terminal_rules */0];
  var sorts = lang[0];
  var exit = 0;
  var exit$1 = 0;
  var sort_name = current_sort[0];
  var exit$2 = 0;
  switch (tm.tag | 0) {
    case 0 : 
        var scopes = tm[1];
        var op_name = tm[0];
        var match = Belt_MapString.getExn(rules[/* sort_rules */1], sort_name);
        var match$1 = find_operator_match(match[0][/* operator_rules */1], op_name);
        var match$2 = match$1[0];
        var numbered_scope_patterns = match$2[/* term_pattern */1][1];
        var children = Belt_Array.mapWithIndex(Belt_List.toArray(match$2[/* tokens */0]), (function (token_ix, token) {
                var token_ix$prime = token_ix + 1 | 0;
                var match = find_subtm(token_ix$prime, scopes, numbered_scope_patterns);
                if (typeof match === "number") {
                  if (token.tag) {
                    throw [
                          BadRules,
                          "subterm not found, nonterminal name: " + token[0]
                        ];
                  } else {
                    var name = token[0];
                    var terminal_rule = Belt_MapString.getExn(terminal_rules, name);
                    var match$1 = regex_is_literal(terminal_rule);
                    if (match$1 !== undefined) {
                      return /* Left */Block.__(0, [match$1]);
                    } else {
                      throw [
                            CantEmitTokenRegex,
                            name,
                            terminal_rule
                          ];
                    }
                  }
                } else if (match.tag) {
                  var binder_name = match[0];
                  if (token.tag) {
                    throw [
                          BadRules,
                          "binder (" + (binder_name + (") found, nonterminal name: " + token[0]))
                        ];
                  } else {
                    return /* Left */Block.__(0, [binder_name]);
                  }
                } else {
                  var subtm = match[1];
                  if (token.tag) {
                    var match$2 = Belt_MapString.getExn(sorts, token[0]);
                    var some_operator = Util.find((function (param) {
                            return param[0] === op_name;
                          }), match$2[1]);
                    var valences;
                    if (some_operator !== undefined) {
                      valences = some_operator[1][1];
                    } else {
                      throw [
                            Caml_builtin_exceptions.assert_failure,
                            /* tuple */[
                              "ConcreteSyntax.ml",
                              148,
                              23
                            ]
                          ];
                    }
                    var valence = Belt_List.getExn(valences, match[0]);
                    var subtree = of_ast(lang, rules, valence[1], subtm);
                    return /* Right */Block.__(1, [subtree]);
                  } else {
                    var subtree$1 = of_ast(lang, rules, current_sort, subtm);
                    return /* Right */Block.__(1, [subtree$1]);
                  }
                }
              }));
        return mk_tree(current_sort, /* Operator */Block.__(0, [op_name]), children);
    case 1 : 
        exit$1 = 2;
        break;
    case 2 : 
    case 3 : 
        exit$2 = 3;
        break;
    
  }
  if (exit$2 === 3) {
    switch (sort_name) {
      case "integer" : 
          if (current_sort[1].length !== 0) {
            exit$1 = 2;
          } else if (tm.tag === 2) {
            exit = 1;
          } else {
            var match$3 = tm[0];
            if (match$3.tag) {
              exit = 1;
            } else {
              var str = Bigint.to_string(match$3[0]);
              return mk_tree(current_sort, /* Primitive */Block.__(1, [/* Integer */0]), /* array */[/* Left */Block.__(0, [str])]);
            }
          }
          break;
      case "sequence" : 
          var match$4 = current_sort[1];
          if (match$4.length !== 1) {
            exit$1 = 2;
          } else {
            var sort = match$4[0];
            if (tm.tag === 2) {
              var children$1 = Belt_List.toArray(List.map((function (tm) {
                          return /* Right */Block.__(1, [of_ast(lang, rules, sort, tm)]);
                        }), tm[0]));
              return mk_tree(current_sort, /* Sequence */1, children$1);
            } else {
              exit = 1;
            }
          }
          break;
      case "string" : 
          if (current_sort[1].length !== 0) {
            exit$1 = 2;
          } else if (tm.tag === 2) {
            exit = 1;
          } else {
            var match$5 = tm[0];
            if (match$5.tag) {
              return mk_tree(current_sort, /* Primitive */Block.__(1, [/* String */1]), /* array */[/* Left */Block.__(0, [match$5[0]])]);
            } else {
              exit = 1;
            }
          }
          break;
      default:
        exit$1 = 2;
    }
  }
  if (exit$1 === 2) {
    switch (tm.tag | 0) {
      case 1 : 
          return mk_tree(current_sort, /* Var */0, /* array */[/* Left */Block.__(0, [tm[0]])]);
      case 2 : 
      case 3 : 
          exit = 1;
          break;
      
    }
  }
  if (exit === 1) {
    throw [
          Caml_builtin_exceptions.match_failure,
          /* tuple */[
            "ConcreteSyntax.ml",
            125,
            4
          ]
        ];
  }
  
}

function to_string(param) {
  var children_str = $$String.concat("", Belt_List.fromArray($$Array.map((function (param) {
                  if (param.tag) {
                    return to_string(param[0]);
                  } else {
                    return param[0];
                  }
                }), param[/* children */4])));
  return param[/* leading_trivia */2] + (children_str + param[/* trailing_trivia */3]);
}

function to_ast(lang, param) {
  var children = param[/* children */4];
  var node_type = param[/* node_type */1];
  var exit = 0;
  if (typeof node_type === "number") {
    if (node_type === 0) {
      if (children.length !== 1) {
        exit = 1;
      } else {
        var match = children[0];
        if (match.tag) {
          exit = 1;
        } else {
          return /* Ok */Block.__(0, [/* Var */Block.__(1, [match[0]])]);
        }
      }
    } else {
      return Belt_Result.map(Curry._2(AA[/* traverse_array_result */2], (function (param) {
                        if (param.tag) {
                          return to_ast(lang, param[0]);
                        } else {
                          return /* Error */Block.__(1, ["TODO: message"]);
                        }
                      }), children), (function (children$prime) {
                    return /* Sequence */Block.__(2, [Belt_List.fromArray(children$prime)]);
                  }));
    }
  } else if (node_type.tag) {
    if (children.length !== 1) {
      exit = 1;
    } else {
      var match$1 = children[0];
      if (match$1.tag) {
        exit = 1;
      } else {
        var str = match$1[0];
        if (node_type[0]) {
          return /* Ok */Block.__(0, [/* Primitive */Block.__(3, [/* PrimString */Block.__(1, [str])])]);
        } else {
          try {
            return /* Ok */Block.__(0, [/* Primitive */Block.__(3, [/* PrimInteger */Block.__(0, [Bigint.of_string(str)])])]);
          }
          catch (exn){
            return /* Error */Block.__(1, ["failed to read integer literal"]);
          }
        }
      }
    }
  } else {
    var op_name = node_type[0];
    var children$prime = Curry._1(AA[/* sequence_array_result */1], Belt_Array.keepMap(children, (function (param) {
                if (param.tag) {
                  return scope_to_ast(lang, param[0]);
                }
                
              })));
    return Belt_Result.map(children$prime, (function (children$prime$prime) {
                  return /* Operator */Block.__(0, [
                            op_name,
                            Belt_List.fromArray(children$prime$prime)
                          ]);
                }));
  }
  if (exit === 1) {
    throw [
          Caml_builtin_exceptions.match_failure,
          /* tuple */[
            "ConcreteSyntax.ml",
            217,
            4
          ]
        ];
  }
  
}

function scope_to_ast(lang, tree) {
  var match = Belt_List.fromArray(Belt_Array.reverse(tree[/* children */4]));
  if (match) {
    var match$1 = to_ast(lang, /* record */[
          /* sort */tree[/* sort */0],
          /* node_type */tree[/* node_type */1],
          /* leading_trivia */tree[/* leading_trivia */2],
          /* trailing_trivia */tree[/* trailing_trivia */3],
          /* children : array */[match[0]]
        ]);
    if (match$1.tag) {
      return /* Error */Block.__(1, [match$1[0]]);
    } else {
      var body$prime = match$1[0];
      return Belt_Result.map(Util.traverse_list_result((function (param) {
                        if (param.tag) {
                          return /* Error */Block.__(1, ["expected binder name, got TODO"]);
                        } else {
                          return /* Ok */Block.__(0, [param[0]]);
                        }
                      }), match[1]), (function (binders$prime) {
                    return /* Scope */[
                            List.rev(binders$prime),
                            body$prime
                          ];
                  }));
    }
  } else {
    return /* Error */Block.__(1, ["scope_to_ast called on no children"]);
  }
}

function to_grammar(param) {
  var rules = Belt_List.toArray(Belt_List.add(Belt_List.add(Belt_List.map(Belt_MapString.toList(param[/* terminal_rules */0]), (function (param) {
                      return /* tuple */[
                              regex_to_string(param[1]),
                              "return '" + (param[0] + "'")
                            ];
                    })), /* tuple */[
                "$",
                "return 'EOF'"
              ]), /* tuple */[
            "\\s+",
            "/* skip whitespace */"
          ]));
  var lex = {
    rules: rules
  };
  var nonterminal_tok_num = function (param) {
    if (param.tag) {
      return 1;
    } else {
      return 0;
    }
  };
  var print_tokens = function (toks) {
    return $$String.concat(" ", List.map(Types.ConcreteSyntaxDescription[/* token_name */1], toks));
  };
  var mk_variable = function (sort_name, param) {
    if (param !== undefined) {
      var match = param;
      return /* array */[
              print_tokens(match[/* tokens */0]),
              Curry._2(Printf.sprintf(/* Format */[
                        /* String_literal */Block.__(11, [
                            "\n          $$ = /* record */[\n            /* SortAp */['",
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* String_literal */Block.__(11, [
                                    "',[]],\n            /* Var */0,\n            '',\n            '',\n            /* array */[(function(tag,x){x.tag=tag;return x;})(/* TerminalName */0, [$",
                                    /* Int */Block.__(4, [
                                        /* Int_i */3,
                                        /* No_padding */0,
                                        /* No_precision */0,
                                        /* String_literal */Block.__(11, [
                                            "])]\n          ]\n        ",
                                            /* End_of_format */0
                                          ])
                                      ])
                                  ])
                              ])
                          ]),
                        "\n          $$ = /* record */[\n            /* SortAp */['%s',[]],\n            /* Var */0,\n            '',\n            '',\n            /* array */[(function(tag,x){x.tag=tag;return x;})(/* TerminalName */0, [$%i])]\n          ]\n        "
                      ]), sort_name, match[/* var_capture */1])
            ];
    } else {
      return /* array */[];
    }
  };
  var mk_sort_rule = function (param) {
    var match = param[1][0];
    var sort_name = param[0];
    return /* tuple */[
            sort_name,
            Belt_List.toArray(/* :: */[
                  mk_variable(sort_name, match[/* variable */2]),
                  List.map((function (param) {
                          var sort_name$1 = sort_name;
                          var param$1 = param;
                          var match = param$1[0];
                          var tokens = match[/* tokens */0];
                          return /* array */[
                                  print_tokens(tokens),
                                  Curry._3(Printf.sprintf(/* Format */[
                                            /* String_literal */Block.__(11, [
                                                "\n          $$ = /* record */[\n            /* SortAp */['",
                                                /* String */Block.__(2, [
                                                    /* No_padding */0,
                                                    /* String_literal */Block.__(11, [
                                                        "', []],\n            /* Operator */(function(tag,x){x.tag=tag;return x;})(0, ['",
                                                        /* String */Block.__(2, [
                                                            /* No_padding */0,
                                                            /* String_literal */Block.__(11, [
                                                                "']),\n            '',\n            '',\n            /* array */[",
                                                                /* String */Block.__(2, [
                                                                    /* No_padding */0,
                                                                    /* String_literal */Block.__(11, [
                                                                        "]\n          ]\n        ",
                                                                        /* End_of_format */0
                                                                      ])
                                                                  ])
                                                              ])
                                                          ])
                                                      ])
                                                  ])
                                              ]),
                                            "\n          $$ = /* record */[\n            /* SortAp */['%s', []],\n            /* Operator */(function(tag,x){x.tag=tag;return x;})(0, ['%s']),\n            '',\n            '',\n            /* array */[%s]\n          ]\n        "
                                          ]), sort_name$1, match[/* term_pattern */1][0], $$String.concat(", ", List.mapi((function (i, tok) {
                                                  return Curry._2(Printf.sprintf(/* Format */[
                                                                  /* String_literal */Block.__(11, [
                                                                      "(function(tag,x){x.tag=tag;return x;})(",
                                                                      /* Int */Block.__(4, [
                                                                          /* Int_i */3,
                                                                          /* No_padding */0,
                                                                          /* No_precision */0,
                                                                          /* String_literal */Block.__(11, [
                                                                              ", [$",
                                                                              /* Int */Block.__(4, [
                                                                                  /* Int_i */3,
                                                                                  /* No_padding */0,
                                                                                  /* No_precision */0,
                                                                                  /* String_literal */Block.__(11, [
                                                                                      "])",
                                                                                      /* End_of_format */0
                                                                                    ])
                                                                                ])
                                                                            ])
                                                                        ])
                                                                    ]),
                                                                  "(function(tag,x){x.tag=tag;return x;})(%i, [$%i])"
                                                                ]), nonterminal_tok_num(tok), i + 1 | 0);
                                                }), tokens)))
                                ];
                        }), match[/* operator_rules */1])
                ])
          ];
  };
  var operators = /* array */[/* array */[
      "left",
      "ADD",
      "SUB"
    ]];
  var bnf = Js_dict.fromList(Belt_List.add(Belt_List.map(Belt_MapString.toList(param[/* sort_rules */1]), mk_sort_rule), /* tuple */[
            "start",
            (
        [["arith EOF", "/*console.log($1);*/ return $1"]]
      )
          ]));
  return {
          lex: lex,
          operators: operators,
          bnf: bnf
        };
}

function jison_parse(parser, str) {
  return Curry._2((function(parser, str) { return parser.parse(str); }), parser, str);
}

function parse(desc, str) {
  var grammar = to_grammar(desc);
  var parser = Jison.to_parser(grammar);
  return /* Ok */Block.__(0, [jison_parse(parser, str)]);
}

exports.equivalent = equivalent;
exports.mk_tree = mk_tree;
exports.of_ast = of_ast;
exports.to_string = to_string;
exports.parse = parse;
exports.to_ast = to_ast;
exports.regex_piece_to_string = regex_piece_to_string;
/* AA Not a pure module */
