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
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function val_to_ast(core_val) {
  switch (core_val.tag | 0) {
    case 0 : 
        return /* Operator */Block.__(0, [
                  core_val[0],
                  Belt_List.map(core_val[1], (function (value) {
                          return /* Scope */[
                                  /* [] */0,
                                  val_to_ast(value)
                                ];
                        }))
                ]);
    case 1 : 
        return /* Primitive */Block.__(3, [core_val[0]]);
    case 2 : 
        return /* Operator */Block.__(0, [
                  "lam",
                  /* :: */[
                    /* Scope */[
                      core_val[0],
                      to_ast(core_val[1])
                    ],
                    /* [] */0
                  ]
                ]);
    
  }
}

function to_ast(core) {
  switch (core.tag | 0) {
    case 0 : 
        return /* Var */Block.__(1, [core[0]]);
    case 1 : 
        return /* Operator */Block.__(0, [
                  "CoreVal",
                  /* :: */[
                    /* Scope */[
                      /* [] */0,
                      val_to_ast(core[0])
                    ],
                    /* [] */0
                  ]
                ]);
    default:
      throw [
            Caml_builtin_exceptions.match_failure,
            /* tuple */[
              "Core.ml",
              58,
              42
            ]
          ];
  }
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
                var sub_results = Belt_List.zipBy(vals, pats, match_branch);
                if (v[0] === pat[0] && Belt_List.length(vals) === Belt_List.length(pats) && Belt_List.every(sub_results, Belt_Option.isSome)) {
                  return Caml_option.some(Belt_List.reduce(Belt_List.map(sub_results, Belt_Option.getExn), Belt_MapString.empty, Util.union));
                } else {
                  return undefined;
                }
            case 1 : 
                exit = 1;
                break;
            case 2 : 
                return undefined;
            
          }
        }
        break;
    case 1 : 
        if (typeof pat === "number") {
          exit = 1;
        } else {
          switch (pat.tag | 0) {
            case 0 : 
                return undefined;
            case 1 : 
                exit = 1;
                break;
            case 2 : 
                if (Types.prim_eq(v[0], pat[0])) {
                  return Caml_option.some(Belt_MapString.empty);
                } else {
                  return undefined;
                }
            
          }
        }
        break;
    case 2 : 
        exit = 1;
        break;
    
  }
  if (exit === 1) {
    if (typeof pat === "number") {
      return Caml_option.some(Belt_MapString.empty);
    } else {
      switch (pat.tag | 0) {
        case 1 : 
            var match = pat[0];
            if (match !== undefined) {
              return Caml_option.some(Belt_MapString.fromArray(/* array */[/* tuple */[
                                match,
                                v
                              ]]));
            } else {
              return Caml_option.some(Belt_MapString.empty);
            }
        case 0 : 
        case 2 : 
            return undefined;
        
      }
    }
  }
  
}

function find_core_match(v, _pats) {
  while(true) {
    var pats = _pats;
    if (pats) {
      var match = pats[0];
      var match$1 = match_branch(v, match[0]);
      if (match$1 !== undefined) {
        return /* tuple */[
                match[1],
                Caml_option.valFromOption(match$1)
              ];
      } else {
        _pats = pats[1];
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
                          Pervasives.$at(Belt_List.zip(patBinders, binders), param[0]),
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

function fill_in_core(dynamics, mr, c) {
  var assignments = mr[1];
  switch (c.tag | 0) {
    case 0 : 
        var match = Belt_MapString.get(assignments, c[0]);
        if (match !== undefined) {
          return Belt_Result.map(term_is_core_val(/* [] */0, match), (function (cv) {
                        return /* CoreVal */Block.__(1, [cv]);
                      }));
        } else {
          return /* Ok */Block.__(0, [c]);
        }
    case 1 : 
        return Belt_Result.map(fill_in_val(dynamics, mr, c[0]), (function (v$prime) {
                      return /* CoreVal */Block.__(1, [v$prime]);
                    }));
    case 2 : 
        var match$1 = fill_in_core(dynamics, mr, c[0]);
        var match$2 = Util.sequence_list_result(Belt_List.map(c[1], (function (param) {
                    return fill_in_core(dynamics, mr, param);
                  })));
        if (match$1.tag) {
          return /* Error */Block.__(1, [match$1[0]]);
        } else if (match$2.tag) {
          return /* Error */Block.__(1, [match$2[0]]);
        } else {
          return /* Ok */Block.__(0, [/* CoreApp */Block.__(2, [
                        match$1[0],
                        match$2[0]
                      ])]);
        }
    case 3 : 
        var mBranches = Util.sequence_list_result(Belt_List.map(c[2], (function (param) {
                    var pat = param[0];
                    return Belt_Result.map(fill_in_core(dynamics, mr, param[1]), (function (core$prime) {
                                  return /* tuple */[
                                          pat,
                                          core$prime
                                        ];
                                }));
                  })));
        var match$3 = fill_in_core(dynamics, mr, c[0]);
        if (match$3.tag) {
          return /* Error */Block.__(1, [match$3[0]]);
        } else if (mBranches.tag) {
          return /* Error */Block.__(1, [mBranches[0]]);
        } else {
          return /* Ok */Block.__(0, [/* Case */Block.__(3, [
                        match$3[0],
                        c[1],
                        mBranches[0]
                      ])]);
        }
    case 4 : 
        var match$4 = Belt_MapString.get(assignments, c[0]);
        if (match$4 !== undefined) {
          return term_to_core(dynamics, match$4);
        } else {
          return /* Error */Block.__(1, [/* tuple */[
                      "TODO 3",
                      undefined
                    ]]);
        }
    
  }
}

function fill_in_val(dynamics, mr, v) {
  switch (v.tag | 0) {
    case 0 : 
        var tag = v[0];
        return Belt_Result.map(Util.traverse_list_result((function (param) {
                          return fill_in_val(dynamics, mr, param);
                        }), v[1]), (function (vals$prime) {
                      return /* ValTm */Block.__(0, [
                                tag,
                                vals$prime
                              ]);
                    }));
    case 1 : 
        return /* Ok */Block.__(0, [v]);
    case 2 : 
        var binders = v[0];
        return Belt_Result.map(fill_in_core(dynamics, mr, v[1]), (function (core$prime) {
                      return /* ValLam */Block.__(2, [
                                binders,
                                core$prime
                              ]);
                    }));
    
  }
}

function term_is_core_val(env, tm) {
  switch (tm.tag | 0) {
    case 0 : 
        var tag = tm[0];
        var exit = 0;
        if (tag === "lam") {
          var match = tm[1];
          if (match && !match[1]) {
            var match$1 = match[0];
            var names = match$1[0];
            var env$prime = Belt_List.concat(names, env);
            return Belt_Result.map(term_is_core(env$prime, match$1[1]), (function (body$prime) {
                          return /* ValLam */Block.__(2, [
                                    names,
                                    body$prime
                                  ]);
                        }));
          } else {
            exit = 1;
          }
        } else {
          exit = 1;
        }
        if (exit === 1) {
          return Belt_Result.map(Util.traverse_list_result((function (param) {
                            var env$1 = env;
                            var param$1 = param;
                            if (param$1[0]) {
                              return /* Error */Block.__(1, [/* tuple */[
                                          "Unexpected binding TODO",
                                          undefined
                                        ]]);
                            } else {
                              return term_is_core_val(env$1, param$1[1]);
                            }
                          }), tm[1]), (function (subtms$prime) {
                        return /* ValTm */Block.__(0, [
                                  tag,
                                  subtms$prime
                                ]);
                      }));
        }
        break;
    case 1 : 
        return /* Error */Block.__(1, [/* tuple */[
                    "TODO 4",
                    tm
                  ]]);
    case 2 : 
        return /* Error */Block.__(1, [/* tuple */[
                    "TODO 5",
                    tm
                  ]]);
    case 3 : 
        return /* Ok */Block.__(0, [/* ValPrim */Block.__(1, [tm[0]])]);
    
  }
}

function term_is_core(env, tm) {
  if (tm.tag === 1) {
    var match = Belt_List.get(env, tm[0]);
    if (match !== undefined) {
      return /* Ok */Block.__(0, [/* CoreVar */Block.__(0, [match])]);
    } else {
      return /* Error */Block.__(1, [/* tuple */[
                  "failed to look up variable",
                  tm
                ]]);
    }
  } else {
    throw [
          Caml_builtin_exceptions.match_failure,
          /* tuple */[
            "Core.ml",
            208,
            4
          ]
        ];
  }
}

function term_to_core(dynamics, tm) {
  var match = find_match(dynamics, tm);
  if (match !== undefined) {
    var match$1 = match;
    return fill_in_core(dynamics, /* tuple */[
                match$1[0],
                match$1[1]
              ], match$1[2]);
  } else {
    return /* Error */Block.__(1, [/* tuple */[
                "no match found",
                tm
              ]]);
  }
}

function $$eval(core) {
  var go = function (ctx, param) {
    switch (param.tag | 0) {
      case 0 : 
          var v = param[0];
          var match = Belt_MapString.get(ctx, v);
          if (match !== undefined) {
            return /* Ok */Block.__(0, [match]);
          } else {
            return /* Error */Block.__(1, ["Unbound variable " + v]);
          }
      case 1 : 
          return /* Ok */Block.__(0, [param[0]]);
      case 2 : 
          var match$1 = param[0];
          if (match$1.tag === 1) {
            var match$2 = match$1[0];
            switch (match$2.tag | 0) {
              case 0 : 
              case 1 : 
                  return /* Error */Block.__(1, ["TODO 7"]);
              case 2 : 
                  var args = param[1];
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
              
            }
          } else {
            return /* Error */Block.__(1, ["TODO 7"]);
          }
      case 3 : 
          var branches = param[2];
          return Belt_Result.flatMap(go(ctx, param[0]), (function (v) {
                        var match = find_core_match(v, branches);
                        if (match !== undefined) {
                          var match$1 = match;
                          return go(Util.union(ctx, match$1[1]), match$1[0]);
                        } else {
                          return /* Error */Block.__(1, ["no match found in case"]);
                        }
                      }));
      case 4 : 
          return /* Error */Block.__(1, ["Found a metavar!"]);
      
    }
  };
  return go(Belt_MapString.empty, core);
}

exports.val_to_ast = val_to_ast;
exports.$$eval = $$eval;
exports.term_to_core = term_to_core;
/* Types Not a pure module */