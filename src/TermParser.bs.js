// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var MenhirLib = require("./menhirLib.bs.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");

var $$Error = Caml_exceptions.create("TermParser.MenhirBasics.Error");

function token2terminal(_tok) {
  if (typeof _tok === "number") {
    switch (_tok) {
      case 0 : 
          return 2;
      case 1 : 
          return 3;
      case 2 : 
          return 4;
      case 3 : 
          return 5;
      case 4 : 
          return 6;
      case 5 : 
          return 9;
      case 6 : 
          return 10;
      
    }
  } else {
    switch (_tok.tag | 0) {
      case 0 : 
          return 1;
      case 1 : 
          return 7;
      case 2 : 
          return 8;
      
    }
  }
}

function token2value(_tok) {
  if (typeof _tok === "number") {
    return /* () */0;
  } else {
    return _tok[0];
  }
}

var semantic_action = /* array */[
  (function (_menhir_env) {
      var _menhir_stack = _menhir_env[/* stack */2];
      var _menhir_s = _menhir_env[/* current */3];
      var _startpos = _menhir_stack[/* endp */3];
      return /* record */[
              /* state */_menhir_s,
              /* semv : [] */0,
              /* startp */_startpos,
              /* endp */_startpos,
              /* next */_menhir_stack
            ];
    }),
  (function (_menhir_env) {
      var _menhir_stack = _menhir_env[/* stack */2];
      return /* record */[
              /* state */_menhir_stack[/* state */0],
              /* semv */_menhir_stack[/* semv */1],
              /* startp */_menhir_stack[/* startp */2],
              /* endp */_menhir_stack[/* endp */3],
              /* next */_menhir_stack[/* next */4]
            ];
    }),
  (function (_menhir_env) {
      var _menhir_stack = _menhir_env[/* stack */2];
      var _menhir_s = _menhir_env[/* current */3];
      var _startpos = _menhir_stack[/* endp */3];
      return /* record */[
              /* state */_menhir_s,
              /* semv : [] */0,
              /* startp */_startpos,
              /* endp */_startpos,
              /* next */_menhir_stack
            ];
    }),
  (function (_menhir_env) {
      var _menhir_stack = _menhir_env[/* stack */2];
      return /* record */[
              /* state */_menhir_stack[/* state */0],
              /* semv */_menhir_stack[/* semv */1],
              /* startp */_menhir_stack[/* startp */2],
              /* endp */_menhir_stack[/* endp */3],
              /* next */_menhir_stack[/* next */4]
            ];
    }),
  (function (_menhir_env) {
      var _menhir_stack = _menhir_env[/* stack */2];
      var _v = /* PrimInteger */Block.__(0, [_menhir_stack[/* semv */1]]);
      return /* record */[
              /* state */_menhir_stack[/* state */0],
              /* semv */_v,
              /* startp */_menhir_stack[/* startp */2],
              /* endp */_menhir_stack[/* endp */3],
              /* next */_menhir_stack[/* next */4]
            ];
    }),
  (function (_menhir_env) {
      var _menhir_stack = _menhir_env[/* stack */2];
      var _v = /* PrimString */Block.__(1, [_menhir_stack[/* semv */1]]);
      return /* record */[
              /* state */_menhir_stack[/* state */0],
              /* semv */_v,
              /* startp */_menhir_stack[/* startp */2],
              /* endp */_menhir_stack[/* endp */3],
              /* next */_menhir_stack[/* next */4]
            ];
    }),
  (function (_menhir_env) {
      var _menhir_stack = _menhir_env[/* stack */2];
      var match = _menhir_stack[/* next */4][/* next */4];
      var _3 = _menhir_stack[/* semv */1];
      var _v_000 = /* :: */[
        match[/* semv */1],
        _3[0]
      ];
      var _v_001 = _3[1];
      var _v = /* Scope */[
        _v_000,
        _v_001
      ];
      return /* record */[
              /* state */match[/* state */0],
              /* semv */_v,
              /* startp */match[/* startp */2],
              /* endp */_menhir_stack[/* endp */3],
              /* next */match[/* next */4]
            ];
    }),
  (function (_menhir_env) {
      var _menhir_stack = _menhir_env[/* stack */2];
      var _v_001 = _menhir_stack[/* semv */1];
      var _v = /* Scope */[
        /* [] */0,
        _v_001
      ];
      return /* record */[
              /* state */_menhir_stack[/* state */0],
              /* semv */_v,
              /* startp */_menhir_stack[/* startp */2],
              /* endp */_menhir_stack[/* endp */3],
              /* next */_menhir_stack[/* next */4]
            ];
    }),
  (function (_menhir_env) {
      var _menhir_stack = _menhir_env[/* stack */2];
      var _v_000 = _menhir_stack[/* semv */1];
      var _v = /* :: */[
        _v_000,
        /* [] */0
      ];
      return /* record */[
              /* state */_menhir_stack[/* state */0],
              /* semv */_v,
              /* startp */_menhir_stack[/* startp */2],
              /* endp */_menhir_stack[/* endp */3],
              /* next */_menhir_stack[/* next */4]
            ];
    }),
  (function (_menhir_env) {
      var _menhir_stack = _menhir_env[/* stack */2];
      var match = _menhir_stack[/* next */4][/* next */4];
      var _v_000 = match[/* semv */1];
      var _v_001 = _menhir_stack[/* semv */1];
      var _v = /* :: */[
        _v_000,
        _v_001
      ];
      return /* record */[
              /* state */match[/* state */0],
              /* semv */_v,
              /* startp */match[/* startp */2],
              /* endp */_menhir_stack[/* endp */3],
              /* next */match[/* next */4]
            ];
    }),
  (function (_menhir_env) {
      var _menhir_stack = _menhir_env[/* stack */2];
      var _v_000 = _menhir_stack[/* semv */1];
      var _v = /* :: */[
        _v_000,
        /* [] */0
      ];
      return /* record */[
              /* state */_menhir_stack[/* state */0],
              /* semv */_v,
              /* startp */_menhir_stack[/* startp */2],
              /* endp */_menhir_stack[/* endp */3],
              /* next */_menhir_stack[/* next */4]
            ];
    }),
  (function (_menhir_env) {
      var _menhir_stack = _menhir_env[/* stack */2];
      var match = _menhir_stack[/* next */4][/* next */4];
      var _v_000 = match[/* semv */1];
      var _v_001 = _menhir_stack[/* semv */1];
      var _v = /* :: */[
        _v_000,
        _v_001
      ];
      return /* record */[
              /* state */match[/* state */0],
              /* semv */_v,
              /* startp */match[/* startp */2],
              /* endp */_menhir_stack[/* endp */3],
              /* next */match[/* next */4]
            ];
    }),
  (function (_menhir_env) {
      var _menhir_stack = _menhir_env[/* stack */2];
      var match = _menhir_stack[/* next */4];
      var match$1 = match[/* next */4][/* next */4];
      var _v_000 = match$1[/* semv */1];
      var _v_001 = match[/* semv */1];
      var _v = /* Operator */Block.__(0, [
          _v_000,
          _v_001
        ]);
      return /* record */[
              /* state */match$1[/* state */0],
              /* semv */_v,
              /* startp */match$1[/* startp */2],
              /* endp */_menhir_stack[/* endp */3],
              /* next */match$1[/* next */4]
            ];
    }),
  (function (_menhir_env) {
      var _menhir_stack = _menhir_env[/* stack */2];
      var _v = /* Var */Block.__(1, [_menhir_stack[/* semv */1]]);
      return /* record */[
              /* state */_menhir_stack[/* state */0],
              /* semv */_v,
              /* startp */_menhir_stack[/* startp */2],
              /* endp */_menhir_stack[/* endp */3],
              /* next */_menhir_stack[/* next */4]
            ];
    }),
  (function (_menhir_env) {
      var _menhir_stack = _menhir_env[/* stack */2];
      var match = _menhir_stack[/* next */4];
      var match$1 = match[/* next */4];
      var _v = /* Sequence */Block.__(2, [match[/* semv */1]]);
      return /* record */[
              /* state */match$1[/* state */0],
              /* semv */_v,
              /* startp */match$1[/* startp */2],
              /* endp */_menhir_stack[/* endp */3],
              /* next */match$1[/* next */4]
            ];
    }),
  (function (_menhir_env) {
      var _menhir_stack = _menhir_env[/* stack */2];
      var _v = /* Primitive */Block.__(3, [_menhir_stack[/* semv */1]]);
      return /* record */[
              /* state */_menhir_stack[/* state */0],
              /* semv */_v,
              /* startp */_menhir_stack[/* startp */2],
              /* endp */_menhir_stack[/* endp */3],
              /* next */_menhir_stack[/* next */4]
            ];
    }),
  (function (_menhir_env) {
      var _menhir_stack = _menhir_env[/* stack */2];
      var match = _menhir_stack[/* next */4];
      return /* record */[
              /* state */match[/* state */0],
              /* semv */match[/* semv */1],
              /* startp */match[/* startp */2],
              /* endp */_menhir_stack[/* endp */3],
              /* next */match[/* next */4]
            ];
    })
];

var ET = MenhirLib.TableInterpreter[/* MakeEngineTable */0]([
      token2terminal,
      0,
      token2value,
      /* tuple */[
        8,
        "\0\x07\0\x06\0\0\0\0\t\b\x11\x03\0\0\x0b\0\x0e\0\0\r\x05\0\x10\x01\0\x12"
      ],
      /* tuple */[
        11,
        "C\x80\x01.\0\x02\xc4\xa7\r\n\x1c\0\0\0\0\0\x03\0\x87\0\0\x80\0\x05\x01\x0e\0\0\0\x10\0\0\0\0@\0"
      ],
      /* tuple */[
        /* tuple */[
          8,
          "\x03\0\x03\0\x0e\x0e\x1c\x0e\0\0\0\0\x03\x0e\0\x02\0 \x03\0\0\"\0\0\x1a\0"
        ],
        /* tuple */[
          8,
          "\x066%\rB\n\x0e\x12\x069\x059\x16\n\x0e\x1a99J\x16-Zg\0\x1e"
        ]
      ],
      /* tuple */[
        4,
        "\b\x87veTC2\"!"
      ],
      /* tuple */[
        /* tuple */[
          8,
          "\x10\0\n\0\0\x05\0 \0\0\0\0\0\x16\0\0\0\0\"\0\0\0\0\0\0\0"
        ],
        /* tuple */[
          8,
          "\t\0\f\r\x0b\0\x10\x12\x15\x18\x19\x0b\x16\t\x0b\x0f\r\x0b\t\x12\x14\n\x0b\x0b"
        ]
      ],
      1,
      semantic_action,
      $$Error,
      undefined
    ]);

var TI = MenhirLib.Engine[/* Make */0]([
      ET[0],
      ET[1],
      ET[2],
      ET[3],
      ET[4],
      ET[6],
      ET[8],
      ET[9],
      ET[10],
      ET[13],
      ET[14],
      ET[15],
      ET[16],
      ET[11],
      ET[17],
      ET[18],
      ET[19],
      ET[20],
      ET[21]
    ]);

var start = TI[32];

var entry = TI[37];

function top_term(lexer, lexbuf) {
  return Curry._3(entry, 0, lexer, lexbuf);
}

function top_term$1(initial_position) {
  return Curry._2(start, 0, initial_position);
}

var Incremental = /* module */[/* top_term */top_term$1];

var MenhirInterpreter_000 = TI[33];

var MenhirInterpreter_001 = TI[34];

var MenhirInterpreter_002 = TI[35];

var MenhirInterpreter_003 = TI[36];

var MenhirInterpreter_004 = TI[38];

var MenhirInterpreter_005 = TI[39];

var MenhirInterpreter_006 = TI[40];

var MenhirInterpreter_007 = TI[41];

var MenhirInterpreter_008 = TI[0];

var MenhirInterpreter_009 = TI[6];

var MenhirInterpreter_010 = TI[7];

var MenhirInterpreter_011 = TI[42];

var MenhirInterpreter_012 = TI[43];

var MenhirInterpreter_013 = TI[52];

var MenhirInterpreter_014 = TI[53];

var MenhirInterpreter_015 = TI[45];

var MenhirInterpreter_016 = TI[44];

var MenhirInterpreter_017 = TI[46];

var MenhirInterpreter_018 = TI[48];

var MenhirInterpreter_019 = TI[47];

var MenhirInterpreter_020 = TI[49];

var MenhirInterpreter_021 = TI[50];

var MenhirInterpreter_022 = TI[51];

var MenhirInterpreter = [
  MenhirInterpreter_000,
  MenhirInterpreter_001,
  MenhirInterpreter_002,
  MenhirInterpreter_003,
  MenhirInterpreter_004,
  MenhirInterpreter_005,
  MenhirInterpreter_006,
  MenhirInterpreter_007,
  MenhirInterpreter_008,
  MenhirInterpreter_009,
  MenhirInterpreter_010,
  MenhirInterpreter_011,
  MenhirInterpreter_012,
  MenhirInterpreter_013,
  MenhirInterpreter_014,
  MenhirInterpreter_015,
  MenhirInterpreter_016,
  MenhirInterpreter_017,
  MenhirInterpreter_018,
  MenhirInterpreter_019,
  MenhirInterpreter_020,
  MenhirInterpreter_021,
  MenhirInterpreter_022
];

exports.$$Error = $$Error;
exports.top_term = top_term;
exports.MenhirInterpreter = MenhirInterpreter;
exports.Incremental = Incremental;
/* ET Not a pure module */
