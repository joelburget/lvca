// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");

var ScopeEncountered = Caml_exceptions.create("NonBinding.ScopeEncountered");

function from_de_bruijn$prime(param) {
  switch (param.tag | 0) {
    case 0 : 
        return /* Operator */Block.__(0, [
                  param[0],
                  Belt_List.map(param[1], from_de_bruijn_scope)
                ]);
    case 1 : 
        throw ScopeEncountered;
    case 2 : 
        return /* Sequence */Block.__(1, [Belt_List.map(param[0], from_de_bruijn$prime)]);
    case 3 : 
        return /* Primitive */Block.__(2, [param[0]]);
    
  }
}

function from_de_bruijn_scope(param) {
  if (param[0]) {
    throw ScopeEncountered;
  }
  return from_de_bruijn$prime(param[1]);
}

function from_de_bruijn(tm) {
  try {
    return from_de_bruijn$prime(tm);
  }
  catch (exn){
    if (exn === ScopeEncountered) {
      return undefined;
    } else {
      throw exn;
    }
  }
}

function to_de_bruijn(tm) {
  switch (tm.tag | 0) {
    case 0 : 
        return /* Operator */Block.__(0, [
                  tm[0],
                  Belt_List.map(tm[1], (function (tm) {
                          return /* Scope */[
                                  /* [] */0,
                                  to_de_bruijn(tm)
                                ];
                        }))
                ]);
    case 1 : 
        return /* Sequence */Block.__(2, [Belt_List.map(tm[0], to_de_bruijn)]);
    case 2 : 
        return /* Primitive */Block.__(3, [tm[0]]);
    
  }
}

function from_nominal$prime(param) {
  switch (param.tag | 0) {
    case 0 : 
        return /* Operator */Block.__(0, [
                  param[0],
                  Belt_List.map(param[1], from_nominal_scope)
                ]);
    case 1 : 
        throw ScopeEncountered;
    case 2 : 
        return /* Sequence */Block.__(1, [Belt_List.map(param[0], from_nominal$prime)]);
    case 3 : 
        return /* Primitive */Block.__(2, [param[0]]);
    
  }
}

function from_nominal_scope(param) {
  if (param[0]) {
    throw ScopeEncountered;
  }
  return from_nominal$prime(param[1]);
}

function from_nominal(tm) {
  try {
    return from_nominal$prime(tm);
  }
  catch (exn){
    if (exn === ScopeEncountered) {
      return undefined;
    } else {
      throw exn;
    }
  }
}

function to_nominal(tm) {
  switch (tm.tag | 0) {
    case 0 : 
        return /* Operator */Block.__(0, [
                  tm[0],
                  Belt_List.map(tm[1], (function (tm) {
                          return /* Scope */[
                                  /* [] */0,
                                  to_nominal(tm)
                                ];
                        }))
                ]);
    case 1 : 
        return /* Sequence */Block.__(2, [Belt_List.map(tm[0], to_nominal)]);
    case 2 : 
        return /* Primitive */Block.__(3, [tm[0]]);
    
  }
}

var BL = 0;

exports.BL = BL;
exports.ScopeEncountered = ScopeEncountered;
exports.from_de_bruijn$prime = from_de_bruijn$prime;
exports.from_de_bruijn_scope = from_de_bruijn_scope;
exports.from_de_bruijn = from_de_bruijn;
exports.to_de_bruijn = to_de_bruijn;
exports.from_nominal$prime = from_nominal$prime;
exports.from_nominal_scope = from_nominal_scope;
exports.from_nominal = from_nominal;
exports.to_nominal = to_nominal;
/* No side effect */
