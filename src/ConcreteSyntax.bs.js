// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Util = require("./Util.bs.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Bigint = require("bs-zarith/src/Bigint.js");
var Printf = require("bs-platform/lib/js/printf.js");
var $$String = require("bs-platform/lib/js/string.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var LrParsing = require("./LrParsing.bs.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Belt_MapInt = require("bs-platform/lib/js/belt_MapInt.js");
var Belt_Result = require("bs-platform/lib/js/belt_Result.js");
var Belt_MapString = require("bs-platform/lib/js/belt_MapString.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("bs-platform/lib/js/caml_js_exceptions.js");
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
    return Caml_obj.caml_equal(child1[0], child2[0]);
  }
}

function find_operator_match(matches, opname) {
  var maybe_match = Util.find((function (param) {
          var term_pattern = param[0][/* term_pattern */1];
          if (term_pattern.tag) {
            return false;
          } else {
            return term_pattern[0] === opname;
          }
        }), Belt_List.flatten(matches));
  if (maybe_match !== undefined) {
    return maybe_match;
  } else {
    return Pervasives.failwith("TODO: default match");
  }
}

var BadSortTerm = Caml_exceptions.create("ConcreteSyntax.BadSortTerm");

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
        return param[0].replace((/\+/g), "\\+").replace((/\*/g), "\\*").replace((/\?/g), "\\?").replace((/\-/g), "\\-").replace((/\(/g), "\\(").replace((/\)/g), "\\)");
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

function mk_left(content) {
  return /* Left */Block.__(0, [/* record */[
              /* content */content,
              /* leading_trivia */"",
              /* trailing_trivia */""
            ]]);
}

function of_ast(lang, rules, current_sort, tm) {
  var terminal_rules = rules[/* terminal_rules */0];
  var sorts = lang[0];
  var exit = 0;
  var sort_name = current_sort[0];
  var exit$1 = 0;
  switch (tm.tag | 0) {
    case 0 : 
        var scopes = tm[1];
        var op_name = tm[0];
        var match = Belt_MapString.getExn(rules[/* sort_rules */1], sort_name);
        var match$1 = find_operator_match(match[0][/* operator_rules */1], op_name);
        var match$2 = match$1[0];
        var term_pattern = match$2[/* term_pattern */1];
        var find_subtm$prime = function (ix) {
          if (term_pattern.tag) {
            return /* FoundCapture */1;
          } else {
            var param = ix;
            var param$1 = scopes;
            var param$2 = term_pattern[1];
            var _slot_num = 0;
            var token_ix = param;
            var _scopes = param$1;
            var _term_pattern = param$2;
            while(true) {
              var term_pattern$1 = _term_pattern;
              var scopes$1 = _scopes;
              var slot_num = _slot_num;
              if (term_pattern$1) {
                if (scopes$1) {
                  var match = term_pattern$1[0];
                  var match$1 = scopes$1[0];
                  var binder_matches = Util.find((function (param) {
                          return param[1] === token_ix;
                        }), Belt_List.zip(match$1[0], match[0]));
                  if (binder_matches !== undefined) {
                    return /* FoundBinder */Block.__(1, [binder_matches[0]]);
                  } else if (token_ix === match[1]) {
                    return /* FoundTerm */Block.__(0, [
                              slot_num,
                              match$1[1]
                            ]);
                  } else {
                    _term_pattern = term_pattern$1[1];
                    _scopes = scopes$1[1];
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
        };
        var children = Belt_Array.map(Belt_List.toArray(Belt_List.keep(Belt_List.mapWithIndex(match$2[/* tokens */0], (function (token_ix, token) {
                            return /* tuple */[
                                    token_ix,
                                    token
                                  ];
                          })), (function (param) {
                        if (typeof param[1] === "number") {
                          return false;
                        } else {
                          return true;
                        }
                      }))), (function (param) {
                var token = param[1];
                var token_ix$prime = param[0] + 1 | 0;
                var match = find_subtm$prime(token_ix$prime);
                var exit = 0;
                if (typeof match === "number") {
                  if (match === 0) {
                    if (typeof token === "number") {
                      exit = 1;
                    } else if (token.tag) {
                      throw [
                            BadRules,
                            "subterm not found, nonterminal name: " + token[0]
                          ];
                    } else {
                      var name = token[0];
                      var terminal_rule = Belt_MapString.getExn(terminal_rules, name);
                      var match$1 = regex_is_literal(terminal_rule);
                      if (match$1 !== undefined) {
                        return mk_left(match$1);
                      } else {
                        throw [
                              CantEmitTokenRegex,
                              name,
                              terminal_rule
                            ];
                      }
                    }
                  } else if (typeof token === "number") {
                    exit = 1;
                  } else if (token.tag) {
                    throw [
                          Caml_builtin_exceptions.assert_failure,
                          /* tuple */[
                            "ConcreteSyntax.ml",
                            195,
                            11
                          ]
                        ];
                  } else {
                    throw [
                          BadRules,
                          "capture found, terminal name: " + token[0]
                        ];
                  }
                } else if (match.tag) {
                  var binder_name = match[0];
                  if (typeof token === "number") {
                    exit = 1;
                  } else if (token.tag) {
                    throw [
                          BadRules,
                          "binder (" + (binder_name + (") found, nonterminal name: " + token[0]))
                        ];
                  } else {
                    return mk_left(binder_name);
                  }
                } else {
                  var subtm = match[1];
                  if (typeof token === "number") {
                    exit = 1;
                  } else if (token.tag) {
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
                              208,
                              23
                            ]
                          ];
                    }
                    var valence = Belt_List.getExn(valences, match[0]);
                    return /* Right */Block.__(1, [of_ast(lang, rules, valence[1], subtm)]);
                  } else {
                    return /* Right */Block.__(1, [of_ast(lang, rules, current_sort, subtm)]);
                  }
                }
                if (exit === 1) {
                  throw [
                        Caml_builtin_exceptions.assert_failure,
                        /* tuple */[
                          "ConcreteSyntax.ml",
                          242,
                          27
                        ]
                      ];
                }
                
              }));
        return mk_tree(current_sort, /* Operator */Block.__(0, [op_name]), children);
    case 1 : 
        exit = 1;
        break;
    case 2 : 
    case 3 : 
        exit$1 = 2;
        break;
    
  }
  if (exit$1 === 2) {
    switch (sort_name) {
      case "integer" : 
          if (current_sort[1].length !== 0) {
            exit = 1;
          } else if (tm.tag === 2) {
            throw [
                  BadSortTerm,
                  current_sort,
                  tm
                ];
          } else {
            var match$3 = tm[0];
            if (match$3.tag) {
              throw [
                    BadSortTerm,
                    current_sort,
                    tm
                  ];
            } else {
              var str = Bigint.to_string(match$3[0]);
              return mk_tree(current_sort, /* Primitive */Block.__(1, [/* Integer */0]), /* array */[mk_left(str)]);
            }
          }
          break;
      case "sequence" : 
          var match$4 = current_sort[1];
          if (match$4.length !== 1) {
            exit = 1;
          } else {
            var sort = match$4[0];
            if (tm.tag === 2) {
              var children$1 = Belt_List.toArray(List.map((function (tm) {
                          return /* Right */Block.__(1, [of_ast(lang, rules, sort, tm)]);
                        }), tm[0]));
              return mk_tree(current_sort, /* Sequence */1, children$1);
            } else {
              throw [
                    BadSortTerm,
                    current_sort,
                    tm
                  ];
            }
          }
          break;
      case "string" : 
          if (current_sort[1].length !== 0) {
            exit = 1;
          } else if (tm.tag === 2) {
            throw [
                  BadSortTerm,
                  current_sort,
                  tm
                ];
          } else {
            var match$5 = tm[0];
            if (match$5.tag) {
              return mk_tree(current_sort, /* Primitive */Block.__(1, [/* String */1]), /* array */[mk_left(match$5[0])]);
            } else {
              throw [
                    BadSortTerm,
                    current_sort,
                    tm
                  ];
            }
          }
          break;
      default:
        exit = 1;
    }
  }
  if (exit === 1) {
    switch (tm.tag | 0) {
      case 1 : 
          return mk_tree(current_sort, /* Var */0, /* array */[mk_left(tm[0])]);
      case 2 : 
      case 3 : 
          throw [
                BadSortTerm,
                current_sort,
                tm
              ];
      
    }
  }
  
}

function to_string(param) {
  var children_str = $$String.concat("", Belt_List.fromArray($$Array.map((function (param) {
                  if (param.tag) {
                    return to_string(param[0]);
                  } else {
                    var match = param[0];
                    return match[/* leading_trivia */1] + (match[/* content */0] + match[/* trailing_trivia */2]);
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
          return /* Ok */Block.__(0, [/* Var */Block.__(1, [match[0][/* content */0]])]);
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
        var str = match$1[0][/* content */0];
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
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "ConcreteSyntax.ml",
            313,
            7
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
                          return /* Ok */Block.__(0, [param[0][/* content */0]]);
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

var NonMatchingFixities = Caml_exceptions.create("ConcreteSyntax.NonMatchingFixities");

var MixedFixities = Caml_exceptions.create("ConcreteSyntax.MixedFixities");

function to_grammar(param) {
  var sort_rules = param[/* sort_rules */1];
  var terminal_rules = param[/* terminal_rules */0];
  Belt_MapString.keysToArray(terminal_rules);
  Belt_MapString.keysToArray(sort_rules);
  var terminal_names = Belt_MapString.fromArray(Belt_Array.mapWithIndex(Belt_MapString.keysToArray(terminal_rules), (function (i, name) {
              return /* tuple */[
                      name,
                      i
                    ];
            })));
  var nonterminal_names = Belt_MapString.fromArray(Belt_Array.mapWithIndex(Belt_MapString.keysToArray(sort_rules), (function (i, name) {
              return /* tuple */[
                      name,
                      i
                    ];
            })));
  return /* record */[
          /* nonterminals */Belt_MapInt.fromArray(Belt_Array.mapWithIndex(Belt_MapString.valuesToArray(sort_rules), (function (i, param) {
                      var productions = Belt_List.flatten(Belt_List.map(param[0][/* operator_rules */1], (function (operator_level) {
                                  return Belt_List.map(operator_level, (function (param) {
                                                return Belt_List.map(param[0][/* tokens */0], (function (param) {
                                                              if (typeof param === "number") {
                                                                return Pervasives.failwith("TODO");
                                                              } else if (param.tag) {
                                                                return /* Nonterminal */Block.__(1, [Belt_MapString.getExn(nonterminal_names, param[0])]);
                                                              } else {
                                                                return /* Terminal */Block.__(0, [Belt_MapString.getExn(terminal_names, param[0])]);
                                                              }
                                                            }));
                                              }));
                                })));
                      return /* tuple */[
                              i,
                              /* record */[/* productions */productions]
                            ];
                    }))),
          /* num_terminals */Belt_MapString.size(terminal_rules),
          /* terminal_names */terminal_names,
          /* nonterminal_names */nonterminal_names
        ];
}

var symbol_info = Pervasives.failwith("TODO");

function tree_of_parse_result(str, root) {
  var str_pos = /* record */[/* contents */0];
  var str_len = str.length;
  var get_trivia = function (start_pos, end_pos) {
    var leading_trivia = str.slice(str_pos[0], start_pos);
    str_pos[0] = end_pos;
    var $$continue = true;
    while($$continue) {
      var match = str.charAt(str_pos[0]);
      var match$1;
      switch (match) {
        case "\n" : 
            match$1 = /* tuple */[
              false,
              true
            ];
            break;
        case " " : 
            match$1 = /* tuple */[
              true,
              false
            ];
            break;
        default:
          match$1 = /* tuple */[
            false,
            false
          ];
      }
      $$continue = str_pos[0] < str_len && match$1[0];
      if ($$continue) {
        str_pos[0] = str_pos[0] + 1 | 0;
      }
      
    };
    var trailing_trivia = str.slice(end_pos, str_pos[0]);
    return /* tuple */[
            leading_trivia,
            trailing_trivia
          ];
  };
  var go_nt = function (param) {
    var match = Curry._1(symbol_info, param[/* symbol */0]);
    var match$1 = get_trivia(param[/* start_pos */2], param[/* end_pos */3]);
    var tokens = Pervasives.failwith("TODO");
    return /* record */[
            /* sort */match[1],
            /* node_type */match[0],
            /* leading_trivia */match$1[0],
            /* trailing_trivia */match$1[1],
            /* children */Belt_List.toArray(Belt_List.map(Belt_List.zip(param[/* children */1], tokens), (function (param) {
                        var parse_result = param[0];
                        var tmp = param[1];
                        if (typeof tmp === "number") {
                          return Pervasives.failwith("TODO");
                        } else if (tmp.tag) {
                          return /* Right */Block.__(1, [go_nt(parse_result)]);
                        } else {
                          return /* Left */Block.__(0, [go_t(parse_result)]);
                        }
                      })))
          ];
  };
  var go_t = function (param) {
    var end_pos = param[/* end_pos */3];
    var start_pos = param[/* start_pos */2];
    var match = get_trivia(start_pos, end_pos);
    var content = str.slice(start_pos, end_pos);
    return /* record */[
            /* content */content,
            /* leading_trivia */match[0],
            /* trailing_trivia */match[1]
          ];
  };
  return go_nt(root);
}

function lexer_of_desc(param) {
  return Belt_List.fromArray(Belt_MapString.toArray(Belt_MapString.map(param[/* terminal_rules */0], regex_to_string)));
}

function parse(desc, str) {
  try {
    var grammar = to_grammar(desc);
    var Lr0$prime = LrParsing.Lr0(/* module */[/* grammar */grammar]);
    var lexer = lexer_of_desc(desc);
    var match = Curry._2(Lr0$prime[/* lex_and_parse */31], lexer, str);
    if (match.tag) {
      var match$1 = match[0];
      if (match$1.tag) {
        var match$2 = match$1[0];
        return /* Error */Block.__(1, [Curry._2(Printf.sprintf(/* Format */[
                            /* String_literal */Block.__(11, [
                                "parser error at character ",
                                /* Scan_get_counter */Block.__(21, [
                                    /* Char_counter */1,
                                    /* String_literal */Block.__(11, [
                                        ":\n",
                                        /* String */Block.__(2, [
                                            /* No_padding */0,
                                            /* End_of_format */0
                                          ])
                                      ])
                                  ])
                              ]),
                            "parser error at character %n:\n%s"
                          ]), match$2[0], match$2[1])]);
      } else {
        var match$3 = match$1[0];
        var end_pos = match$3[/* end_pos */1];
        var start_pos = match$3[/* start_pos */0];
        return /* Error */Block.__(1, [Curry._4(Printf.sprintf(/* Format */[
                            /* String_literal */Block.__(11, [
                                "lexical error at characters ",
                                /* Scan_get_counter */Block.__(21, [
                                    /* Char_counter */1,
                                    /* String_literal */Block.__(11, [
                                        " - ",
                                        /* Scan_get_counter */Block.__(21, [
                                            /* Char_counter */1,
                                            /* String_literal */Block.__(11, [
                                                " (",
                                                /* String */Block.__(2, [
                                                    /* No_padding */0,
                                                    /* String_literal */Block.__(11, [
                                                        "):\n",
                                                        /* String */Block.__(2, [
                                                            /* No_padding */0,
                                                            /* End_of_format */0
                                                          ])
                                                      ])
                                                  ])
                                              ])
                                          ])
                                      ])
                                  ])
                              ]),
                            "lexical error at characters %n - %n (%s):\n%s"
                          ]), start_pos, end_pos, str.slice(start_pos, end_pos), match$3[/* message */2])]);
      }
    } else {
      return /* Ok */Block.__(0, [tree_of_parse_result(str, match[0])]);
    }
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === NonMatchingFixities) {
      return /* Error */Block.__(1, ["In sort " + (exn[1] + (": all fixities in a precedence level must be the same fixity (this is a limitation of Bison-style parsers (Jison in particular). The operators identified by [" + ($$String.concat(", ", exn[2]) + "] must all share the same fixity.")))]);
    } else if (exn[0] === MixedFixities) {
      return /* Error */Block.__(1, ["Found a mix of fixities -- all must be uniform " + (Pervasives.string_of_bool(exn[1]) + (" " + String(exn[2])))]);
    } else {
      throw exn;
    }
  }
}

exports.equivalent = equivalent;
exports.mk_tree = mk_tree;
exports.of_ast = of_ast;
exports.to_string = to_string;
exports.parse = parse;
exports.to_ast = to_ast;
exports.regex_piece_to_string = regex_piece_to_string;
/* AA Not a pure module */
