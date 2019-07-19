// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Printf = require("bs-platform/lib/js/printf.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");

var $$SyntaxError = Caml_exceptions.create("LexerUtil.SyntaxError");

function next_line(lexbuf) {
  var pos = lexbuf[/* lex_curr_p */11];
  lexbuf[/* lex_curr_p */11] = /* record */[
    /* pos_fname */pos[/* pos_fname */0],
    /* pos_lnum */pos[/* pos_lnum */1] + 1 | 0,
    /* pos_bol */lexbuf[/* lex_curr_pos */5],
    /* pos_cnum */pos[/* pos_cnum */3]
  ];
  return /* () */0;
}

function position(lexbuf) {
  var p = lexbuf[/* lex_curr_p */11];
  return Curry._3(Printf.sprintf(/* Format */[
                  /* String */Block.__(2, [
                      /* No_padding */0,
                      /* Char_literal */Block.__(12, [
                          /* ":" */58,
                          /* Int */Block.__(4, [
                              /* Int_d */0,
                              /* No_padding */0,
                              /* No_precision */0,
                              /* Char_literal */Block.__(12, [
                                  /* ":" */58,
                                  /* Int */Block.__(4, [
                                      /* Int_d */0,
                                      /* No_padding */0,
                                      /* No_precision */0,
                                      /* End_of_format */0
                                    ])
                                ])
                            ])
                        ])
                    ]),
                  "%s:%d:%d"
                ]), p[/* pos_fname */0], p[/* pos_lnum */1], p[/* pos_cnum */3] - p[/* pos_bol */2] | 0);
}

function error(lexbuf, msg) {
  throw [
        $$SyntaxError,
        position(lexbuf) + (" " + msg)
      ];
}

exports.$$SyntaxError = $$SyntaxError;
exports.next_line = next_line;
exports.position = position;
exports.error = error;
/* No side effect */