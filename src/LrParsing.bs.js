// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Util = require("./Util.bs.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Belt_Id = require("bs-platform/lib/js/belt_Id.js");
var Belt_Map = require("bs-platform/lib/js/belt_Map.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Bitstring = require("./Bitstring.bs.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Belt_MapInt = require("bs-platform/lib/js/belt_MapInt.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Belt_SetInt = require("bs-platform/lib/js/belt_SetInt.js");
var Belt_MutableSet = require("bs-platform/lib/js/belt_MutableSet.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var Belt_MutableMapInt = require("bs-platform/lib/js/belt_MutableMapInt.js");
var Belt_MutableSetInt = require("bs-platform/lib/js/belt_MutableSetInt.js");
var Caml_js_exceptions = require("bs-platform/lib/js/caml_js_exceptions.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function cmp(param, param$1) {
  var c = Caml_obj.caml_compare(param[0], param$1[0]);
  if (c !== 0) {
    return c;
  } else {
    return Caml_obj.caml_compare(param[1], param$1[1]);
  }
}

var SymbolCmp = Belt_Id.MakeComparable(/* module */[/* cmp */cmp]);

function view_item(item) {
  return /* record */[
          /* production_num */item & 16777215,
          /* position */((item & -16777216) >>> 24)
        ];
}

function mk_item$prime(production_num, position) {
  return (position << 24) | production_num;
}

function mk_item(param) {
  return mk_item$prime(param[/* production_num */0], param[/* position */1]);
}

var ComparableSet = Belt_Id.MakeComparable(/* module */[/* cmp */Belt_SetInt.cmp]);

var ParseFinished = Caml_exceptions.create("LrParsing.ParseFinished");

var ParseFailed = Caml_exceptions.create("LrParsing.ParseFailed");

var PopFailed = Caml_exceptions.create("LrParsing.PopFailed");

function pop_exn(arr) {
  var match = arr.pop();
  if (match !== undefined) {
    return match;
  } else {
    throw PopFailed;
  }
}

function Lr0(G) {
  var production_map = Belt_MutableMapInt.make(/* () */0);
  var production_nonterminal_map = Belt_MutableMapInt.make(/* () */0);
  var nonterminal_production_map = Belt_MutableMapInt.make(/* () */0);
  var production_cnt = /* record */[/* contents */0];
  Belt_MapInt.forEach(G[/* grammar */0][/* nonterminals */0], (function (nt_num, param) {
          Belt_MutableMapInt.set(nonterminal_production_map, nt_num, Belt_MutableSetInt.make(/* () */0));
          return Belt_List.forEach(param[/* productions */0], (function (production) {
                        var production_num = production_cnt[0];
                        production_cnt[0] = production_num + 1 | 0;
                        Belt_MutableMapInt.set(production_map, production_num, production);
                        Belt_MutableMapInt.set(production_nonterminal_map, production_num, nt_num);
                        var prod_set = Belt_MutableMapInt.getExn(nonterminal_production_map, nt_num);
                        return Belt_MutableSetInt.add(prod_set, production_num);
                      }));
        }));
  var number_of_nonterminals = Belt_MapInt.size(G[/* grammar */0][/* nonterminals */0]);
  var get_nonterminal_num = function (param) {
    return Belt_MutableMapInt.getExn(production_nonterminal_map, param);
  };
  var get_nonterminal = function (pn) {
    return Belt_MapInt.getExn(G[/* grammar */0][/* nonterminals */0], Belt_MutableMapInt.getExn(production_nonterminal_map, pn));
  };
  var closure = function (initial_items) {
    var added = Bitstring.alloc(number_of_nonterminals, false);
    var nonkernel_items = Belt_MutableSetInt.make(/* () */0);
    var nt_stack = Belt_MutableSetInt.make(/* () */0);
    Belt_SetInt.forEach(initial_items, (function (item) {
            var match = view_item(item);
            var production = Belt_MutableMapInt.getExn(production_map, match[/* production_num */0]);
            var match$1 = Belt_List.get(production, match[/* position */1]);
            if (match$1 !== undefined) {
              var match$2 = match$1;
              if (match$2.tag) {
                return Belt_MutableSetInt.add(nt_stack, match$2[0]);
              } else {
                return /* () */0;
              }
            } else {
              return /* () */0;
            }
          }));
    while(!Belt_MutableSetInt.isEmpty(nt_stack)) {
      var match = Belt_MutableSetInt.minimum(nt_stack);
      var nonterminal_num = match !== undefined ? match : Pervasives.failwith("invariant violation: the set is not empty!");
      Belt_MutableSetInt.remove(nt_stack, nonterminal_num);
      if (!Bitstring.getExn(added, nonterminal_num)) {
        Bitstring.setExn(added, nonterminal_num, true);
        var production_set = Belt_MutableMapInt.getExn(nonterminal_production_map, nonterminal_num);
        Belt_MutableSetInt.forEach(production_set, (function (production_num) {
                return Belt_MutableSetInt.add(nonkernel_items, mk_item$prime(production_num, 0));
              }));
        var match$1 = Belt_MapInt.getExn(G[/* grammar */0][/* nonterminals */0], nonterminal_num);
        Belt_List.forEach(match$1[/* productions */0], (function (production) {
                if (production) {
                  var match = production[0];
                  if (match.tag) {
                    return Belt_MutableSetInt.add(nt_stack, match[0]);
                  } else {
                    return /* () */0;
                  }
                } else {
                  return Pervasives.failwith("Empty production");
                }
              }));
      }
      
    };
    return /* record */[
            /* kernel_items */initial_items,
            /* nonkernel_items */Belt_SetInt.fromArray(Belt_MutableSetInt.toArray(nonkernel_items))
          ];
  };
  var simplify_config_set = function (param) {
    return Belt_SetInt.union(param[/* kernel_items */0], param[/* nonkernel_items */1]);
  };
  var closure$prime = function (items) {
    return simplify_config_set(closure(items));
  };
  var goto_kernel = function (item_set, symbol) {
    var result = Belt_MutableSetInt.make(/* () */0);
    Belt_SetInt.forEach(item_set, (function (item) {
            var match = view_item(item);
            var position = match[/* position */1];
            var production_num = match[/* production_num */0];
            var production = Belt_MutableMapInt.getExn(production_map, production_num);
            var match$1 = Belt_List.get(production, position);
            if (match$1 !== undefined && Caml_obj.caml_equal(symbol, match$1)) {
              return Belt_MutableSetInt.add(result, mk_item$prime(production_num, position + 1 | 0));
            } else {
              return /* () */0;
            }
          }));
    return Belt_SetInt.fromArray(Belt_MutableSetInt.toArray(result));
  };
  var $$goto = function (item_set, symbol) {
    return closure(goto_kernel(item_set, symbol));
  };
  var augmented_start = Belt_SetInt.fromArray(/* array */[mk_item(/* record */[
              /* production_num */0,
              /* position */0
            ])]);
  var ca = simplify_config_set(closure(augmented_start));
  var c = Belt_MutableSet.fromArray(/* array */[ca], ComparableSet);
  var $$continue = /* record */[/* contents */true];
  while($$continue[0]) {
    $$continue[0] = false;
    Belt_MutableSet.forEach(c, (function (i) {
            var grammar_symbols = Belt_List.concat(Belt_List.makeBy(G[/* grammar */0][/* num_terminals */1], (function (n) {
                        return /* Terminal */Block.__(0, [n]);
                      })), Belt_List.makeBy(number_of_nonterminals, (function (n) {
                        return /* Nonterminal */Block.__(1, [n]);
                      })));
            return Belt_List.forEach(grammar_symbols, (function (x) {
                          var goto_i_x = simplify_config_set(closure(goto_kernel(i, x)));
                          if (!Belt_SetInt.isEmpty(goto_i_x) && !Belt_MutableSet.has(c, goto_i_x)) {
                            Belt_MutableSet.add(c, goto_i_x);
                            $$continue[0] = true;
                            return /* () */0;
                          } else {
                            return 0;
                          }
                        }));
          }));
  };
  var items$prime = Belt_MapInt.fromArray(Belt_Array.mapWithIndex(Belt_MutableSet.toArray(c), (function (i, item_set) {
              return /* tuple */[
                      i,
                      item_set
                    ];
            })));
  var state_to_item_set = function (param) {
    return Belt_MapInt.getExn(items$prime, param);
  };
  var item_set_to_state = function (item_set) {
    return Belt_Option.getExn(Belt_MapInt.findFirstBy(items$prime, (function (k, item_set$prime) {
                        return Caml_obj.caml_equal(item_set$prime, item_set);
                      })))[0];
  };
  var in_first_cache = Belt_Map.make(SymbolCmp);
  var stack = Belt_MutableSetInt.make(/* () */0);
  var in_first$prime = function (t_num, sym) {
    var match = Belt_Map.get(in_first_cache, /* tuple */[
          t_num,
          sym
        ]);
    if (match !== undefined) {
      return match;
    } else if (sym.tag) {
      var match$1 = Belt_MapInt.getExn(G[/* grammar */0][/* nonterminals */0], sym[0]);
      var productions = match$1[/* productions */0];
      var match$2 = Util.fold_right((function (param) {
              var match = param[1];
              var found_it = Belt_List.some(productions, (function (param) {
                      if (param) {
                        var nt = param[0];
                        if (nt.tag) {
                          var nt_num = nt[0];
                          if (Belt_MutableSetInt.has(stack, nt_num)) {
                            return false;
                          } else {
                            Belt_MutableSetInt.add(stack, nt_num);
                            var result = in_first$prime(t_num, nt);
                            Belt_MutableSetInt.remove(stack, nt_num);
                            return result;
                          }
                        } else {
                          return nt[0] === t_num;
                        }
                      } else {
                        return false;
                      }
                    }));
              return /* tuple */[
                      match[0] || found_it,
                      match[1] && false
                    ];
            }), productions, /* tuple */[
            false,
            true
          ]);
      var result = match$2[0];
      Belt_Map.set(in_first_cache, /* tuple */[
            t_num,
            sym
          ], result);
      return result;
    } else {
      return sym[0] === t_num;
    }
  };
  var in_first_str = function (t_num, str) {
    if (str) {
      return in_first$prime(t_num, str[0]);
    } else {
      return false;
    }
  };
  var FoundInFollow = Caml_exceptions.create("LrParsing.Lr0(G).FoundInFollow");
  var in_follow$prime$prime = function (nts_visited, t_num, nt_num, _param) {
    while(true) {
      var param = _param;
      if (param) {
        var match = param[0];
        if (match.tag) {
          var rest = param[1];
          if (match[0] === nt_num && in_first_str(t_num, rest)) {
            return true;
          } else {
            _param = rest;
            continue ;
          }
        } else {
          _param = param[1];
          continue ;
        }
      } else {
        return false;
      }
    };
  };
  var in_follow$prime = function (nts_visited, t_num, nt_num) {
    if (Belt_SetInt.has(nts_visited, nt_num)) {
      return false;
    } else if (nt_num === 0) {
      return t_num === 0;
    } else {
      try {
        var nts_visited$prime = Belt_SetInt.add(nts_visited, nt_num);
        Belt_MutableMapInt.forEach(production_map, (function (prod_num, production) {
                if (in_follow$prime$prime(nts_visited$prime, t_num, nt_num, production)) {
                  throw FoundInFollow;
                }
                var nt_num$prime = Belt_MutableMapInt.getExn(production_nonterminal_map, prod_num);
                var match = Util.unsnoc(production);
                var match$1 = match[1];
                if (match$1.tag && match$1[0] === nt_num && in_follow$prime(nts_visited$prime, t_num, nt_num$prime)) {
                  throw FoundInFollow;
                } else {
                  return 0;
                }
              }));
        return false;
      }
      catch (exn){
        if (exn === FoundInFollow) {
          return true;
        } else {
          throw exn;
        }
      }
    }
  };
  var partial_arg = Belt_SetInt.fromArray(/* array */[]);
  var in_follow = function (param, param$1) {
    return in_follow$prime(partial_arg, param, param$1);
  };
  var goto_table = function (state, nt) {
    var item_set = Belt_MapInt.getExn(items$prime, state);
    return item_set_to_state(simplify_config_set(closure(goto_kernel(item_set, nt))));
  };
  var action_table = function (state, terminal_num) {
    var item_set = Belt_MapInt.getExn(items$prime, state);
    var item_set_l = Belt_SetInt.toList(item_set);
    var shift_action = Util.find_by(item_set_l, (function (item) {
            var match = view_item(item);
            var symbols = Belt_MutableMapInt.getExn(production_map, match[/* production_num */0]);
            var match$1 = Belt_List.get(symbols, match[/* position */1]);
            if (match$1 !== undefined) {
              var next_symbol = match$1;
              if (next_symbol.tag) {
                return undefined;
              } else {
                return /* Shift */Block.__(0, [goto_table(state, next_symbol)]);
              }
            }
            
          }));
    var reduce_action = Util.find_by(item_set_l, (function (item) {
            var match = view_item(item);
            var production_num = match[/* production_num */0];
            var nt_num = Belt_MutableMapInt.getExn(production_nonterminal_map, production_num);
            var production = Belt_MutableMapInt.getExn(production_map, production_num);
            if (match[/* position */1] === Belt_List.length(production) && Curry._2(in_follow, terminal_num, nt_num)) {
              return /* Reduce */Block.__(1, [nt_num]);
            }
            
          }));
    var accept_action = terminal_num === 0 && Belt_SetInt.has(item_set, 16777216) ? /* Accept */0 : undefined;
    var exit = 0;
    if (shift_action !== undefined) {
      if (reduce_action !== undefined || accept_action !== undefined) {
        exit = 1;
      } else {
        return shift_action;
      }
    } else if (reduce_action !== undefined) {
      if (accept_action !== undefined) {
        exit = 1;
      } else {
        return reduce_action;
      }
    } else if (accept_action !== undefined) {
      return accept_action;
    } else {
      return /* Error */1;
    }
    if (exit === 1) {
      throw [
            Caml_builtin_exceptions.match_failure,
            /* tuple */[
              "LrParsing.ml",
              434,
              4
            ]
          ];
    }
    
  };
  var slr_tables = /* tuple */[
    action_table,
    goto_table
  ];
  var parse = function (toks) {
    var stack = /* :: */[
      0,
      /* [] */0
    ];
    try {
      while(true) {
        var match = stack;
        if (match) {
          var a = /* record */[/* contents */pop_exn(toks)];
          var a$prime = Curry._1(Pervasives.failwith("TODO"), a);
          var match$1 = action_table(match[0], a$prime);
          if (typeof match$1 === "number") {
            if (match$1 === 0) {
              throw ParseFinished;
            } else {
              throw [
                    ParseFailed,
                    Pervasives.failwith("TODO")
                  ];
            }
          } else if (match$1.tag) {
            var match$2 = stack;
            if (match$2) {
              stack = /* :: */[
                goto_table(match$2[0], a$prime),
                match$2[1]
              ];
            } else {
              Pervasives.failwith("invariant violation: reduction with empty stack");
            }
          } else {
            stack = /* :: */[
              match$1[0],
              stack
            ];
            a[0] = pop_exn(toks);
          }
        } else {
          throw [
                Caml_builtin_exceptions.match_failure,
                /* tuple */[
                  "LrParsing.ml",
                  448,
                  14
                ]
              ];
        }
      };
      return Pervasives.failwith("can't make it here");
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn === ParseFinished) {
        return Pervasives.failwith("TODO");
      } else if (exn[0] === ParseFailed) {
        return /* Error */Block.__(1, [exn[1]]);
      } else {
        throw exn;
      }
    }
  };
  return /* module */[
          /* production_map */production_map,
          /* production_nonterminal_map */production_nonterminal_map,
          /* nonterminal_production_map */nonterminal_production_map,
          /* production_cnt */production_cnt,
          /* number_of_nonterminals */number_of_nonterminals,
          /* get_nonterminal_num */get_nonterminal_num,
          /* get_nonterminal */get_nonterminal,
          /* closure */closure,
          /* simplify_config_set */simplify_config_set,
          /* closure' */closure$prime,
          /* goto_kernel */goto_kernel,
          /* goto */$$goto,
          /* items */c,
          /* items' */items$prime,
          /* state_to_item_set */state_to_item_set,
          /* item_set_to_state */item_set_to_state,
          /* in_first_cache */in_first_cache,
          /* in_first */in_first$prime,
          /* in_first_str */in_first_str,
          /* end_marker */0,
          /* FoundInFollow */FoundInFollow,
          /* in_follow'' */in_follow$prime$prime,
          /* in_follow' */in_follow$prime,
          /* in_follow */in_follow,
          /* goto_table */goto_table,
          /* action_table */action_table,
          /* slr_tables */slr_tables,
          /* parse */parse
        ];
}

function lalr_tables(grammar) {
  Pervasives.failwith("TODO");
  Pervasives.failwith("TODO");
  var action_table = Pervasives.failwith("TODO");
  var goto_table = Pervasives.failwith("TODO");
  return /* tuple */[
          action_table,
          goto_table
        ];
}

var A = 0;

var L = 0;

var M = 0;

var MM = 0;

var SI = 0;

var SS = 0;

var MSI = 0;

var Result = 0;

exports.A = A;
exports.L = L;
exports.M = M;
exports.MM = MM;
exports.SI = SI;
exports.SS = SS;
exports.MSI = MSI;
exports.Result = Result;
exports.SymbolCmp = SymbolCmp;
exports.view_item = view_item;
exports.mk_item$prime = mk_item$prime;
exports.mk_item = mk_item;
exports.ComparableSet = ComparableSet;
exports.ParseFinished = ParseFinished;
exports.ParseFailed = ParseFailed;
exports.PopFailed = PopFailed;
exports.pop_exn = pop_exn;
exports.Lr0 = Lr0;
exports.lalr_tables = lalr_tables;
/* SymbolCmp Not a pure module */
