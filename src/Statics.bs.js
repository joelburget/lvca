// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");

function of_de_bruijn(param) {
  switch (param.tag | 0) {
    case 0 : 
        return /* Operator */Block.__(0, [
                  param[0],
                  Belt_List.map(param[1], scope_of_de_bruijn)
                ]);
    case 1 : 
        return /* Bound */Block.__(1, [param[0]]);
    case 2 : 
        return /* Sequence */Block.__(3, [Belt_List.map(param[0], of_de_bruijn)]);
    case 3 : 
        return /* Primitive */Block.__(4, [param[0]]);
    
  }
}

function scope_of_de_bruijn(param) {
  return /* Scope */[
          param[0],
          of_de_bruijn(param[1])
        ];
}

var M = 0;

var BL = 0;

exports.M = M;
exports.BL = BL;
exports.of_de_bruijn = of_de_bruijn;
exports.scope_of_de_bruijn = scope_of_de_bruijn;
/* No side effect */