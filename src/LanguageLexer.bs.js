// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Lexing = require("bs-platform/lib/js/lexing.js");
var LexerUtil = require("./LexerUtil.bs.js");

var __ocaml_lex_tables = /* record */[
  /* lex_base */"\0\0\xf5\xff\x01\0\xf7\xff\xf8\xff\xf9\xff\xfa\xff\xfb\xff\xfc\xff\0\0N\0\x02\0\x07\0\xfd\xff\b\0\xf6\xff\x04\0",
  /* lex_backtrk */"\xff\xff\xff\xff\n\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\n\0\x01\0\0\0\0\0\xff\xff\xff\xff\xff\xff\t\0",
  /* lex_default */"\x01\0\0\0\xff\xff\0\0\0\0\0\0\0\0\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\0\0\x0e\0\0\0\xff\xff",
  /* lex_trans */"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x0b\0\x0b\0\x0b\0\x0b\0\f\0\x0f\0\x0b\0\x0b\0\x0b\0\x0f\0\0\0\x0b\0\x10\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x0b\0\0\0\x0b\0\0\0\0\0\0\0\0\0\x0b\0\b\0\x07\0\0\0\0\0\0\0\n\0\x05\0\x02\0\x0e\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\t\0\x06\0\0\0\r\0\0\0\0\0\0\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\0\0\0\0\0\0\0\0\n\0\0\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\x04\0\0\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\0\0\0\0\0\0\0\0\n\0\0\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x03\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xff\xff\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
  /* lex_check */"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\x0b\0\x0b\0\0\0\x10\0\x0b\0\f\0\f\0\x0e\0\xff\xff\f\0\x0e\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\x0b\0\xff\xff\xff\xff\xff\xff\xff\xff\f\0\0\0\0\0\xff\xff\xff\xff\xff\xff\0\0\0\0\0\0\x02\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\xff\xff\t\0\xff\xff\xff\xff\xff\xff\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\n\0\0\0\xff\xff\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\xff\xff\xff\xff\xff\xff\xff\xff\n\0\xff\xff\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0e\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
  /* lex_base_code */"",
  /* lex_backtrk_code */"",
  /* lex_default_code */"",
  /* lex_trans_code */"",
  /* lex_check_code */"",
  /* lex_code */""
];

function __ocaml_lex_read_rec(lexbuf, ___ocaml_lex_state) {
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf);
    switch (__ocaml_lex_state$1) {
      case 0 : 
          ___ocaml_lex_state = 0;
          continue ;
      case 1 : 
          return /* ID */[Lexing.lexeme(lexbuf)];
      case 2 : 
          return /* ASSIGN */9;
      case 3 : 
          return /* LEFT_PAREN */3;
      case 4 : 
          return /* RIGHT_PAREN */1;
      case 5 : 
          return /* SEMICOLON */0;
      case 6 : 
          return /* DOT */6;
      case 7 : 
          return /* BAR */8;
      case 8 : 
          return /* EOF */5;
      case 9 : 
          LexerUtil.next_line(lexbuf);
          ___ocaml_lex_state = 0;
          continue ;
      case 10 : 
          return LexerUtil.error(lexbuf, "Unexpected char: " + Lexing.lexeme(lexbuf));
      default:
        Curry._1(lexbuf[/* refill_buff */0], lexbuf);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function read(lexbuf) {
  return __ocaml_lex_read_rec(lexbuf, 0);
}

exports.__ocaml_lex_tables = __ocaml_lex_tables;
exports.read = read;
exports.__ocaml_lex_read_rec = __ocaml_lex_read_rec;
/* No side effect */
