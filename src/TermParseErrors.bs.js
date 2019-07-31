// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function message(s) {
  switch (s) {
    case 0 : 
        return "Unexpected token. Expected term.\n";
    case 2 : 
        return "Unexpected token after left bracket. Expected (semicolon-separated) list of terms.\n";
    case 4 : 
        return "Unexpected token after identifier. Expected left paren or semicolon.\n";
    case 5 : 
        return "Unexpected token after opening paren. Expected a variable name or term.\n";
    case 6 : 
        return "Unexpected token after identifier. Expected `.`, `(`, or `;`.\n";
    case 7 : 
        return "Missing body after binding. Example of a binding and use: `lam(x. x)`.\n";
    case 12 : 
        return "Unexpected token. Missing right paren?\n";
    case 13 : 
        return "Unexpected symbol after semicolon.\n";
    case 17 : 
        return "Unexpected token after term. Expected `]` or `;`.\n";
    case 18 : 
        return "Unexpected token after semicolon. Expected list of terms.\n";
    case 1 : 
    case 3 : 
    case 8 : 
    case 9 : 
    case 10 : 
    case 11 : 
    case 14 : 
    case 15 : 
    case 16 : 
    case 19 : 
    case 20 : 
    case 21 : 
    case 22 : 
    case 23 : 
        throw Caml_builtin_exceptions.not_found;
    case 24 : 
        return "Unexpected token. Expected end of file.\n";
    default:
      throw Caml_builtin_exceptions.not_found;
  }
}

exports.message = message;
/* No side effect */
