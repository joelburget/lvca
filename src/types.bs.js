// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Belt_MapString = require("bs-platform/lib/js/belt_MapString.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

var Abt = /* module */[];

var Ast = /* module */[];

function matchBranch(v, pat, core) {
  var exit = 0;
  if (v.tag || typeof pat === "number" || pat.tag) {
    exit = 1;
  } else {
    return Pervasives.failwith("TODO");
  }
  if (exit === 1) {
    throw [
          Caml_builtin_exceptions.match_failure,
          /* tuple */[
            "types.ml",
            97,
            37
          ]
        ];
  }
  
}

function $$eval(core) {
  var _ctx = Belt_MapString.empty;
  var _core = core;
  while(true) {
    var core$1 = _core;
    var ctx = _ctx;
    switch (core$1.tag | 0) {
      case 0 : 
          var v = core$1[0][0];
          var match = Belt_MapString.get(ctx, v);
          if (match !== undefined) {
            return /* Ok */Block.__(0, [match]);
          } else {
            return /* Error */Block.__(1, ["Unbound variable " + v]);
          }
      case 1 : 
          return /* Ok */Block.__(0, [core$1[0]]);
      case 2 : 
          var match$1 = core$1[0];
          if (match$1.tag === 3) {
            var argNames = match$1[0];
            if (Belt_List.length(argNames) !== Belt_List.length(core$1[1])) {
              return /* Error */Block.__(1, ["mismatched application lengths"]);
            } else {
              var ctx$prime = Belt_MapString.merge(ctx, Belt_MapString.fromArray(Belt_List.toArray(Belt_List.zip(argNames, /* [] */0))), (function (_k, v1, v2) {
                      if (v2 !== undefined) {
                        return v2;
                      } else if (v1 !== undefined) {
                        return v1;
                      } else {
                        return undefined;
                      }
                    }));
              _core = match$1[1];
              _ctx = ctx$prime;
              continue ;
            }
          } else {
            return /* Error */Block.__(1, ["TODO"]);
          }
      case 3 : 
      case 4 : 
          return /* Error */Block.__(1, ["TODO"]);
      case 5 : 
          return /* Error */Block.__(1, ["Found a metavar!"]);
      
    }
  };
}

var Core = /* module */[
  /* M */0,
  /* matchBranch */matchBranch,
  /* eval */$$eval
];

var Denotation = /* module */[];

function intersperse(list, el) {
  if (list) {
    var match = list[1];
    if (match) {
      return /* :: */[
              list[0],
              /* :: */[
                el,
                intersperse(/* :: */[
                      match[0],
                      match[1]
                    ], el)
              ]
            ];
    } else {
      return list;
    }
  } else {
    return list;
  }
}

function intersperse_after(list, el) {
  if (list) {
    var match = list[1];
    var list_el = list[0];
    if (match) {
      return /* :: */[
              list_el,
              /* :: */[
                el,
                intersperse_after(/* :: */[
                      match[0],
                      match[1]
                    ], el)
              ]
            ];
    } else {
      return /* :: */[
              list_el,
              /* :: */[
                el,
                /* [] */0
              ]
            ];
    }
  } else {
    return /* [] */0;
  }
}

exports.Abt = Abt;
exports.Ast = Ast;
exports.Core = Core;
exports.Denotation = Denotation;
exports.intersperse = intersperse;
exports.intersperse_after = intersperse_after;
/* No side effect */
