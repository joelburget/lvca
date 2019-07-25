// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var Util = require("./Util.bs.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var MenhirLib = require("./menhirLib.bs.js");
var Belt_MapString = require("bs-platform/lib/js/belt_MapString.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");

var $$Error = Caml_exceptions.create("LanguageParser.MenhirBasics.Error");

function token2terminal(_tok) {
  if (typeof _tok === "number") {
    switch (_tok) {
      case 0 : 
          return 1;
      case 1 : 
          return 2;
      case 2 : 
          return 3;
      case 3 : 
          return 4;
      case 4 : 
          return 5;
      case 5 : 
          return 7;
      case 6 : 
          return 8;
      case 7 : 
          return 9;
      case 8 : 
          return 10;
      case 9 : 
          return 11;
      
    }
  } else {
    return 6;
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
      var match = _menhir_stack[/* next */4];
      var match$1 = match[/* next */4][/* next */4][/* next */4];
      var match$2 = match$1[/* next */4];
      var _v_000 = match$1[/* semv */1];
      var _v_001 = match[/* semv */1];
      var _v = /* Arity */[
        _v_000,
        _v_001
      ];
      return /* record */[
              /* state */match$2[/* state */0],
              /* semv */_v,
              /* startp */match$2[/* startp */2],
              /* endp */_menhir_stack[/* endp */3],
              /* next */match$2[/* next */4]
            ];
    }),
  (function (_menhir_env) {
      var _menhir_stack = _menhir_env[/* stack */2];
      var match = _menhir_stack[/* next */4];
      var match$1 = match[/* next */4];
      var _v_001 = match[/* semv */1];
      var _v = /* Arity */[
        /* [] */0,
        _v_001
      ];
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
      var match = _menhir_stack[/* next */4];
      var _v = /* Language */[Belt_MapString.fromArray(Belt_List.toArray(match[/* semv */1]))];
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
      var match = _menhir_stack[/* next */4];
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
      var _v_000 = match[/* semv */1];
      var _v_001 = _menhir_stack[/* semv */1];
      var _v = /* OperatorDef */[
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
      var _menhir_s = _menhir_env[/* current */3];
      var _startpos = _menhir_stack[/* endp */3];
      return /* record */[
              /* state */_menhir_s,
              /* semv */undefined,
              /* startp */_startpos,
              /* endp */_startpos,
              /* next */_menhir_stack
            ];
    }),
  (function (_menhir_env) {
      var _menhir_stack = _menhir_env[/* stack */2];
      var _v = _menhir_stack[/* semv */1];
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
      var _v_001 = Belt_List.toArray(match[/* semv */1]);
      var _v = /* SortAp */[
        _v_000,
        _v_001
      ];
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
      var match = _menhir_stack[/* next */4][/* next */4][/* next */4];
      var _v_000 = match[/* semv */1];
      var _v_001 = /* SortDef */[
        /* [] */0,
        _menhir_stack[/* semv */1]
      ];
      var _v = /* tuple */[
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
      var _v = /* VariableValence */Block.__(1, [
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
      var match = Util.unsnoc(_menhir_stack[/* semv */1]);
      var _v_000 = match[0];
      var _v_001 = match[1];
      var _v = /* FixedValence */Block.__(0, [
          _v_000,
          _v_001
        ]);
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
      return /* record */[
              /* state */_menhir_stack[/* state */0],
              /* semv */_menhir_stack[/* semv */1],
              /* startp */_menhir_stack[/* startp */2],
              /* endp */_menhir_stack[/* endp */3],
              /* next */_menhir_stack[/* next */4]
            ];
    })
];

var ET = MenhirLib.TableInterpreter[/* MakeEngineTable */0]([
      token2terminal,
      0,
      token2value,
      /* tuple */[
        8,
        "\0\0\0\r\0\0\0\0\0\0\0\0\x15\x06\0\x18\0\0\x1a\0\x03\0\0\0\0\x13\x17\x1b\b\x1c\0\0\0\x11\0\0\0\0\x02\x0b\x19\0\0\x0f\0\n\0\x04\x01"
      ],
      /* tuple */[
        12,
        "\x02\0\x01\x02 \0\x02\0\xc0\"\0\xc0\"\0\x80`\0 \0\0\0 \0\0\x02\x01\0\0\x02\0\0\x06\0\x02\x06\b\x02\0\0\0\0\0\0\0\0\x02\x01\x04\x02\0\0\x10\0\x80\"\x02\0\0\0\0\0\x002\x02\0\0\x03\0\0\x01\0\0\0\0"
      ],
      /* tuple */[
        /* tuple */[
          8,
          "\x03\x1a\x05\0&\n\x12\x10\x18\x10\x1e\x18\0\x000\0\x182\x006\0$\x12\x03\x18\0\0\0\0\x000$0\x0088\x12>\0\0\0\t&\0 \x006\0\0"
        ],
        /* tuple */[
          8,
          "EE55-\x06\xaab\x0e\x1az\x19\"B\x11\x1e.M&ZU=\x06!\n\x16>\x82JR~\x8e\x92\x9a\xbf"
        ]
      ],
      /* tuple */[
        8,
        "\0\x10\x10\x0f\x0e\x0e\r\r\f\f\x0b\n\n\t\t\b\b\x07\x07\x06\x06\x05\x05\x04\x03\x02\x02\x01"
      ],
      /* tuple */[
        /* tuple */[
          8,
          "\x16\0\x03\0 \0\x03\0$\0\x000\0\0\0\0\"\0\0\0\0\0\x1a\0\x05\0\0\0\0\x004\x006\0\0\0\f\0\0\0\0\0(\0<\0\0\0\0"
        ],
        /* tuple */[
          8,
          "\x14\x16\x18\x18\x1d\x1a\x1c&\x16\x05\x18\x1d\x1e\x1c-\x16(\x18\x1b\x1e\x1c\x12\x0b/\x0e)1*\x0b,\r*\x0f-#\"\0\0\0\0\0\0."
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

function language_def(lexer, lexbuf) {
  return Curry._3(entry, 0, lexer, lexbuf);
}

function language_def$1(initial_position) {
  return Curry._2(start, 0, initial_position);
}

var Incremental = /* module */[/* language_def */language_def$1];

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
exports.language_def = language_def;
exports.MenhirInterpreter = MenhirInterpreter;
exports.Incremental = Incremental;
/* ET Not a pure module */
