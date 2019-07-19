// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.js");
var Util = require("../src/Util.bs.js");
var Block = require("bs-platform/lib/js/block.js");
var Lexing = require("bs-platform/lib/js/lexing.js");
var Belt_Result = require("bs-platform/lib/js/belt_Result.js");
var Belt_MapString = require("bs-platform/lib/js/belt_MapString.js");
var ConcreteSyntax = require("../src/ConcreteSyntax.bs.js");
var ConcreteSyntaxLexer = require("../src/ConcreteSyntaxLexer.bs.js");
var ConcreteSyntaxParser = require("../src/ConcreteSyntaxParser.bs.js");

Jest.describe("ConcreteSyntax", (function (param) {
        var description = "\n  ADD    := \"+\"\n  SUB    := \"-\"\n  NAME   := [a-z][a-zA-Z0-9]*\n\n  arith :=\n    | arith; ADD; arith     { add($1; $3) }\n    | arith; SUB; arith     { sub($1; $3) }\n    | NAME                  { var($1)     }\n  ";
        var language = /* Language */[Belt_MapString.fromArray(/* array */[/* tuple */[
                  "arith",
                  /* SortDef */[
                    /* [] */0,
                    /* :: */[
                      /* OperatorDef */[
                        "add",
                        /* Arity */[
                          /* [] */0,
                          /* :: */[
                            /* FixedValence */Block.__(0, [
                                /* [] */0,
                                /* SortAp */[
                                  "arith",
                                  /* [] */0
                                ]
                              ]),
                            /* :: */[
                              /* FixedValence */Block.__(0, [
                                  /* [] */0,
                                  /* SortAp */[
                                    "arith",
                                    /* [] */0
                                  ]
                                ]),
                              /* [] */0
                            ]
                          ]
                        ]
                      ],
                      /* [] */0
                    ]
                  ]
                ]])];
        Jest.testAll("regex_piece_to_string", /* :: */[
              Jest.Expect[/* toBe */2]("\\+", Jest.Expect[/* expect */0](ConcreteSyntax.regex_piece_to_string(/* ReString */Block.__(0, ["+"])))),
              /* :: */[
                Jest.Expect[/* toBe */2]("\\*", Jest.Expect[/* expect */0](ConcreteSyntax.regex_piece_to_string(/* ReString */Block.__(0, ["*"])))),
                /* :: */[
                  Jest.Expect[/* toBe */2]("\\?", Jest.Expect[/* expect */0](ConcreteSyntax.regex_piece_to_string(/* ReString */Block.__(0, ["?"])))),
                  /* :: */[
                    Jest.Expect[/* toBe */2]("\\-", Jest.Expect[/* expect */0](ConcreteSyntax.regex_piece_to_string(/* ReString */Block.__(0, ["-"])))),
                    /* :: */[
                      Jest.Expect[/* toBe */2]("[a-z]", Jest.Expect[/* expect */0](ConcreteSyntax.regex_piece_to_string(/* ReSet */Block.__(1, ["a-z"])))),
                      /* [] */0
                    ]
                  ]
                ]
              ]
            ], Util.id);
        Jest.test("language parses", (function (param) {
                var lex = Lexing.from_string(description);
                ConcreteSyntaxParser.language(ConcreteSyntaxLexer.read, lex);
                return Jest.pass;
              }));
        var lex = Lexing.from_string(description);
        var concrete = ConcreteSyntaxParser.language(ConcreteSyntaxLexer.read, lex);
        var arith = /* SortAp */[
          "arith",
          /* [] */0
        ];
        var tree = ConcreteSyntax.mk_tree(arith, /* Operator */Block.__(0, ["add"]), /* array */[
              /* Right */Block.__(1, [ConcreteSyntax.mk_tree(arith, /* Var */0, /* array */[/* Left */Block.__(0, ["x"])])]),
              /* Left */Block.__(0, ["+"]),
              /* Right */Block.__(1, [ConcreteSyntax.mk_tree(arith, /* Var */0, /* array */[/* Left */Block.__(0, ["y"])])])
            ]);
        var tree$prime = ConcreteSyntax.mk_tree(arith, /* Operator */Block.__(0, ["sub"]), /* array */[
              /* Right */Block.__(1, [ConcreteSyntax.mk_tree(arith, /* Operator */Block.__(0, ["add"]), /* array */[
                        /* Right */Block.__(1, [ConcreteSyntax.mk_tree(arith, /* Var */0, /* array */[/* Left */Block.__(0, ["x"])])]),
                        /* Left */Block.__(0, ["+"]),
                        /* Right */Block.__(1, [ConcreteSyntax.mk_tree(arith, /* Var */0, /* array */[/* Left */Block.__(0, ["y"])])])
                      ])]),
              /* Left */Block.__(0, ["-"]),
              /* Right */Block.__(1, [ConcreteSyntax.mk_tree(arith, /* Var */0, /* array */[/* Left */Block.__(0, ["z"])])])
            ]);
        var ast = /* Operator */Block.__(0, [
            "add",
            /* :: */[
              /* Scope */[
                /* [] */0,
                /* Var */Block.__(1, ["x"])
              ],
              /* :: */[
                /* Scope */[
                  /* [] */0,
                  /* Var */Block.__(1, ["y"])
                ],
                /* [] */0
              ]
            ]
          ]);
        Jest.test("of_ast", (function (param) {
                return Jest.Expect[/* toEqual */12](tree, Jest.Expect[/* expect */0](ConcreteSyntax.of_ast(language, concrete, arith, ast)));
              }));
        Jest.test("to_ast", (function (param) {
                return Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [ast]), Jest.Expect[/* expect */0](ConcreteSyntax.to_ast(language, tree)));
              }));
        Jest.test("to_string", (function (param) {
                return Jest.Expect[/* toEqual */12]("x+y", Jest.Expect[/* expect */0](ConcreteSyntax.to_string(tree)));
              }));
        return Jest.testAll("parse", /* :: */[
                    Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [tree]), Jest.Expect[/* expect */0](ConcreteSyntax.parse(concrete, "x+y"))),
                    /* :: */[
                      Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [true]), Jest.Expect[/* expect */0](Belt_Result.map(ConcreteSyntax.parse(concrete, "x + y"), (function (param) {
                                      return ConcreteSyntax.equivalent(tree, param);
                                    })))),
                      /* :: */[
                        Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [tree$prime]), Jest.Expect[/* expect */0](ConcreteSyntax.parse(concrete, "x+y-z"))),
                        /* :: */[
                          Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [true]), Jest.Expect[/* expect */0](Belt_Result.map(ConcreteSyntax.parse(concrete, "x + y-z"), (function (param) {
                                          return ConcreteSyntax.equivalent(tree$prime, param);
                                        })))),
                          /* [] */0
                        ]
                      ]
                    ]
                  ], Util.id);
      }));

var to_ast = ConcreteSyntax.to_ast;

var to_string = ConcreteSyntax.to_string;

var of_ast = ConcreteSyntax.of_ast;

var mk_tree = ConcreteSyntax.mk_tree;

var parse = ConcreteSyntax.parse;

var equivalent = ConcreteSyntax.equivalent;

var regex_piece_to_string = ConcreteSyntax.regex_piece_to_string;

exports.to_ast = to_ast;
exports.to_string = to_string;
exports.of_ast = of_ast;
exports.mk_tree = mk_tree;
exports.parse = parse;
exports.equivalent = equivalent;
exports.regex_piece_to_string = regex_piece_to_string;
/*  Not a pure module */