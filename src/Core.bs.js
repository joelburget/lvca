// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var Util = require("./Util.bs.js");
var Block = require("bs-platform/lib/js/block.js");
var Types = require("./Types.bs.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Belt_Result = require("bs-platform/lib/js/belt_Result.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Belt_MapString = require("bs-platform/lib/js/belt_MapString.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("bs-platform/lib/js/caml_js_exceptions.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

var AstConversionErr = Caml_exceptions.create("Core.AstConversionErr");

function to_ast(core) {
  switch (core.tag | 0) {
    case 0 : 
        return /* Operator */Block.__(0, [
                  core[0],
                  Belt_List.map(core[1], scope_to_ast)
                ]);
    case 1 : 
        return /* Var */Block.__(1, [core[0]]);
    case 2 : 
        return /* Sequence */Block.__(2, [Belt_List.map(core[0], to_ast)]);
    case 3 : 
        return /* Primitive */Block.__(3, [core[0]]);
    case 4 : 
        throw [
              AstConversionErr,
              core
            ];
    default:
      throw [
            AstConversionErr,
            core
          ];
  }
}

function scope_to_ast(param) {
  return /* Scope */[
          param[0],
          to_ast(param[1])
        ];
}

function match_branch(v, pat) {
  var exit = 0;
  switch (v.tag | 0) {
    case 0 : 
        var vals = v[1];
        if (typeof pat === "number") {
          exit = 1;
        } else {
          switch (pat.tag | 0) {
            case 0 : 
                var pats = pat[1];
                var sub_results = Belt_List.zipBy(vals, pats, match_binding_branch);
                if (v[0] === pat[0] && Belt_List.length(vals) === Belt_List.length(pats) && Belt_List.every(sub_results, Belt_Option.isSome)) {
                  return Caml_option.some(Belt_List.reduce(Belt_List.map(sub_results, Belt_Option.getExn), Belt_MapString.empty, Util.union));
                } else {
                  return undefined;
                }
            case 1 : 
                exit = 1;
                break;
            case 2 : 
            case 3 : 
                return undefined;
            
          }
        }
        break;
    case 2 : 
        var s1 = v[0];
        if (typeof pat === "number") {
          exit = 1;
        } else {
          switch (pat.tag | 0) {
            case 1 : 
                exit = 1;
                break;
            case 2 : 
                var s2 = pat[0];
                var sub_results$1 = Belt_List.zipBy(s1, s2, match_branch);
                if (Belt_List.length(s1) === Belt_List.length(s2) && Belt_List.every(sub_results$1, Belt_Option.isSome)) {
                  return Caml_option.some(Belt_List.reduce(Belt_List.map(sub_results$1, Belt_Option.getExn), Belt_MapString.empty, Util.union));
                } else {
                  return undefined;
                }
            case 0 : 
            case 3 : 
                return undefined;
            
          }
        }
        break;
    case 3 : 
        if (typeof pat === "number") {
          exit = 1;
        } else {
          switch (pat.tag | 0) {
            case 1 : 
                exit = 1;
                break;
            case 0 : 
            case 2 : 
                return undefined;
            case 3 : 
                if (Types.prim_eq(v[0], pat[0])) {
                  return Caml_option.some(Belt_MapString.empty);
                } else {
                  return undefined;
                }
            
          }
        }
        break;
    default:
      exit = 1;
  }
  if (exit === 1) {
    if (typeof pat === "number") {
      return Caml_option.some(Belt_MapString.empty);
    } else if (pat.tag === 1) {
      var match = pat[0];
      if (match !== undefined) {
        return Caml_option.some(Belt_MapString.fromArray(/* array */[/* tuple */[
                          match,
                          v
                        ]]));
      } else {
        return Caml_option.some(Belt_MapString.empty);
      }
    } else {
      return undefined;
    }
  }
  
}

function match_binding_branch(param, param$1) {
  return match_branch(param[1], param$1[1]);
}

function find_core_match(v, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var match = param[0];
      var match$1 = match_branch(v, match[0]);
      if (match$1 !== undefined) {
        return /* tuple */[
                match[1][1],
                Caml_option.valFromOption(match$1)
              ];
      } else {
        _param = param[1];
        continue ;
      }
    } else {
      return undefined;
    }
  };
}

function matches(tm, pat) {
  var exit = 0;
  if (tm.tag) {
    exit = 1;
  } else {
    var subtms = tm[1];
    if (pat.tag) {
      exit = 1;
    } else {
      var subpats = pat[1];
      if (tm[0] === pat[0] && Belt_List.length(subtms) === Belt_List.length(subpats)) {
        return Util.fold_right((function (param) {
                      var b_opt = param[1];
                      var match = param[0];
                      var match$1 = matches_scope(match[0], match[1]);
                      if (match$1 !== undefined && b_opt !== undefined) {
                        var match$2 = b_opt;
                        var match$3 = match$1;
                        return /* tuple */[
                                Pervasives.$at(match$3[0], match$2[0]),
                                Util.union(match$3[1], match$2[1])
                              ];
                      }
                      
                    }), Belt_List.zip(subtms, subpats), /* tuple */[
                    /* [] */0,
                    Belt_MapString.empty
                  ]);
      } else {
        return undefined;
      }
    }
  }
  if (exit === 1) {
    if (pat.tag) {
      var match = pat[0];
      if (match !== undefined) {
        return /* tuple */[
                /* [] */0,
                Belt_MapString.fromArray(/* array */[/* tuple */[
                        match,
                        tm
                      ]])
              ];
      } else {
        return /* tuple */[
                /* [] */0,
                Belt_MapString.empty
              ];
      }
    } else {
      return undefined;
    }
  }
  
}

function matches_scope(param, param$1) {
  var patBinders = param$1[0];
  var binders = param[0];
  if (Belt_List.length(patBinders) === Belt_List.length(binders)) {
    return Belt_Option.map(matches(param[1], param$1[1]), (function (param) {
                  return /* tuple */[
                          Pervasives.$at(Belt_List.zipBy(patBinders, binders, (function (pattern_name, term_name) {
                                      return /* record */[
                                              /* pattern_name */pattern_name,
                                              /* term_name */term_name
                                            ];
                                    })), param[0]),
                          param[1]
                        ];
                }));
  }
  
}

function find_match(param, term) {
  return Util.get_first((function (param) {
                var core = param[1];
                return Belt_Option.map(matches(term, param[0]), (function (param) {
                              return /* tuple */[
                                      param[0],
                                      param[1],
                                      core
                                    ];
                            }));
              }), param[0]);
}

var TranslationError = Caml_exceptions.create("Core.TranslationError");

function fill_in_core(args, v) {
  var assignments = args[/* assignments */3];
  switch (v.tag | 0) {
    case 0 : 
        return /* Operator */Block.__(0, [
                  v[0],
                  Belt_List.map(v[1], (function (param) {
                          return fill_in_core_scope(args, param);
                        }))
                ]);
    case 2 : 
        return /* Sequence */Block.__(2, [Belt_List.map(v[0], (function (param) {
                          return fill_in_core(args, param);
                        }))]);
    case 1 : 
    case 3 : 
        return v;
    case 4 : 
        return /* Lambda */Block.__(4, [fill_in_core_scope(args, v[0])]);
    case 5 : 
        return /* CoreApp */Block.__(5, [
                  fill_in_core(args, v[0]),
                  Belt_List.map(v[1], (function (param) {
                          return fill_in_core(args, param);
                        }))
                ]);
    case 6 : 
        return /* Case */Block.__(6, [
                  fill_in_core(args, v[0]),
                  v[1],
                  Belt_List.map(v[2], (function (param) {
                          return /* tuple */[
                                  param[0],
                                  fill_in_core_scope(args, param[1])
                                ];
                        }))
                ]);
    case 7 : 
        var name = v[0];
        var match = Belt_MapString.get(assignments, name);
        if (match !== undefined) {
          return term_to_core(/* [] */0, match);
        } else {
          throw [
                TranslationError,
                /* tuple */[
                  "Metavariable " + (name + " not found"),
                  undefined
                ]
              ];
        }
    case 8 : 
        var name$1 = v[0];
        var match$1 = Belt_MapString.get(assignments, name$1);
        if (match$1 !== undefined) {
          var match$2 = term_denotation(args[/* dynamics */0], args[/* vars */1], match$1);
          if (match$2.tag) {
            throw [
                  TranslationError,
                  match$2[0]
                ];
          } else {
            return match$2[0];
          }
        } else {
          throw [
                TranslationError,
                /* tuple */[
                  "Metavariable " + (name$1 + " not found"),
                  undefined
                ]
              ];
        }
    
  }
}

function fill_in_core_scope(args, param) {
  var names = param[0];
  return /* CoreScope */[
          names,
          fill_in_core(/* record */[
                /* dynamics */args[/* dynamics */0],
                /* vars */Pervasives.$at(names, args[/* vars */1]),
                /* assocs */args[/* assocs */2],
                /* assignments */args[/* assignments */3]
              ], param[1])
        ];
}

function term_to_core(env, tm) {
  switch (tm.tag | 0) {
    case 0 : 
        return /* Operator */Block.__(0, [
                  tm[0],
                  Belt_List.map(tm[1], (function (param) {
                          var env$1 = env;
                          var param$1 = param;
                          return /* CoreScope */[
                                  param$1[0],
                                  term_to_core(env$1, param$1[1])
                                ];
                        }))
                ]);
    case 1 : 
        throw [
              Caml_builtin_exceptions.match_failure,
              /* tuple */[
                "Core.ml",
                226,
                26
              ]
            ];
    case 2 : 
        return /* Sequence */Block.__(2, [Belt_List.map(tm[0], (function (param) {
                          return term_to_core(env, param);
                        }))]);
    case 3 : 
        return /* Primitive */Block.__(3, [tm[0]]);
    
  }
}

function term_denotation(dynamics, vars, tm) {
  if (tm.tag === 1) {
    var i = tm[0];
    var match = Belt_List.get(vars, i);
    if (match !== undefined) {
      return /* Ok */Block.__(0, [/* Var */Block.__(1, [match])]);
    } else {
      return /* Error */Block.__(1, [/* tuple */[
                  "couldn't find variable " + (String(i) + (" in variable context of size " + String(Belt_List.length(vars)))),
                  tm
                ]]);
    }
  } else {
    var match$1 = find_match(dynamics, tm);
    if (match$1 !== undefined) {
      var match$2 = match$1;
      try {
        return /* Ok */Block.__(0, [fill_in_core(/* record */[
                        /* dynamics */dynamics,
                        /* vars */vars,
                        /* assocs */match$2[0],
                        /* assignments */match$2[1]
                      ], match$2[2])]);
      }
      catch (raw_exn){
        var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn[0] === TranslationError) {
          return /* Error */Block.__(1, [exn[1]]);
        } else {
          throw exn;
        }
      }
    } else {
      return /* Error */Block.__(1, [/* tuple */[
                  "no match found",
                  tm
                ]]);
    }
  }
}

function $$eval(core) {
  var go = function (ctx, tm) {
    switch (tm.tag | 0) {
      case 1 : 
          var v = tm[0];
          var match = Belt_MapString.get(ctx, v);
          if (match !== undefined) {
            return /* Ok */Block.__(0, [match]);
          } else {
            return /* Error */Block.__(1, ["Unbound variable " + v]);
          }
      case 4 : 
          return /* Error */Block.__(1, ["Found a term we can't evaluate"]);
      case 5 : 
          var match$1 = tm[0];
          if (match$1.tag === 4) {
            var args = tm[1];
            var match$2 = match$1[0];
            var body = match$2[1];
            var argNames = match$2[0];
            if (Belt_List.length(argNames) !== Belt_List.length(args)) {
              return /* Error */Block.__(1, ["mismatched application lengths"]);
            } else {
              return Belt_Result.flatMap(Util.sequence_list_result(Belt_List.map(args, (function (param) {
                                    return go(ctx, param);
                                  }))), (function (arg_vals) {
                            var new_args = Belt_MapString.fromArray(Belt_List.toArray(Belt_List.zip(argNames, arg_vals)));
                            return go(Util.union(ctx, new_args), body);
                          }));
            }
          } else {
            return /* Error */Block.__(1, ["Found a term we can't evaluate"]);
          }
      case 6 : 
          var branches = tm[2];
          return Belt_Result.flatMap(go(ctx, tm[0]), (function (v) {
                        var match = find_core_match(v, branches);
                        if (match !== undefined) {
                          var match$1 = match;
                          return go(Util.union(ctx, match$1[1]), match$1[0]);
                        } else {
                          return /* Error */Block.__(1, ["no match found in case"]);
                        }
                      }));
      case 7 : 
      case 8 : 
          return /* Error */Block.__(1, ["Found a metavar!"]);
      default:
        return /* Ok */Block.__(0, [tm]);
    }
  };
  return go(Belt_MapString.empty, core);
}

exports.to_ast = to_ast;
exports.$$eval = $$eval;
exports.term_denotation = term_denotation;
/* Types Not a pure module */
