// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.js");
var Util = require("../src/Util.bs.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Types = require("../src/Types.bs.js");
var Bigint = require("bs-zarith/src/Bigint.js");
var Binding = require("../src/Binding.bs.js");

Jest.describe("Nominal.(jsonify, serialize, hash)", (function (param) {
        var serialize = function (tm) {
          return Types.$$ArrayBuffer[/* to_hex */0](Curry._1(Types.$$Uint8Array[/* to_array_buffer */2], Curry._1(Binding.Nominal[/* serialize */3], tm)));
        };
        var tm = /* Var */Block.__(1, ["x"]);
        Jest.testAll("(var) x", /* :: */[
              Jest.Expect[/* toEqual */12](( ["v", "x"] ), Jest.Expect[/* expect */0](Curry._1(Binding.Nominal[/* jsonify */2], tm))),
              /* :: */[
                Jest.Expect[/* toBe */2]("8261766178", Jest.Expect[/* expect */0](serialize(tm))),
                /* :: */[
                  Jest.Expect[/* toBe */2]("bbc37ed1e26f8481398dc797bd0b3ec2e3ae954e1c3f69b461a487d8703ec3d6", Jest.Expect[/* expect */0](Curry._1(Binding.Nominal[/* hash */4], tm))),
                  /* [] */0
                ]
              ]
            ], Util.id);
        var tm$1 = /* Operator */Block.__(0, [
            "Z",
            /* [] */0
          ]);
        Jest.testAll("Z()", /* :: */[
              Jest.Expect[/* toEqual */12](( ["t", "Z", []] ), Jest.Expect[/* expect */0](Curry._1(Binding.Nominal[/* jsonify */2], tm$1))),
              /* :: */[
                Jest.Expect[/* toBe */2]("836174615a80", Jest.Expect[/* expect */0](serialize(tm$1))),
                /* :: */[
                  Jest.Expect[/* toBe */2]("cc55b934e76de136a1664dc89c473b2fdc52948d8ba4394bfad5e1219841ffb3", Jest.Expect[/* expect */0](Curry._1(Binding.Nominal[/* hash */4], tm$1))),
                  /* [] */0
                ]
              ]
            ], Util.id);
        var tm$2 = /* Operator */Block.__(0, [
            "S",
            /* :: */[
              /* Scope */[
                /* :: */[
                  "x",
                  /* [] */0
                ],
                /* Var */Block.__(1, ["x"])
              ],
              /* [] */0
            ]
          ]);
        Jest.testAll("S(x. x)", /* :: */[
              Jest.Expect[/* toEqual */12](( ["t", "S", [[["x"], ["v", "x"]]]] ), Jest.Expect[/* expect */0](Curry._1(Binding.Nominal[/* jsonify */2], tm$2))),
              /* :: */[
                Jest.Expect[/* toBe */2]("836174615381828161788261766178", Jest.Expect[/* expect */0](serialize(tm$2))),
                /* :: */[
                  Jest.Expect[/* toBe */2]("22e98205b448e5d79e3dd8fe46469288e9292c0a10eb1b6eb0b896d54e016661", Jest.Expect[/* expect */0](Curry._1(Binding.Nominal[/* hash */4], tm$2))),
                  /* [] */0
                ]
              ]
            ], Util.id);
        var tm$3 = /* Primitive */Block.__(3, [/* PrimInteger */Block.__(0, [Bigint.of_string("12345")])]);
        Jest.testAll("12345", /* :: */[
              Jest.Expect[/* toEqual */12](( ["p", ["i", "12345"]] ), Jest.Expect[/* expect */0](Curry._1(Binding.Nominal[/* jsonify */2], tm$3))),
              /* :: */[
                Jest.Expect[/* toBe */2]("826170826169653132333435", Jest.Expect[/* expect */0](serialize(tm$3))),
                /* :: */[
                  Jest.Expect[/* toBe */2]("e69505a495d739f89cf515c31cf3a2cca4e29a1a4fede9a331b45207a6fb33e5", Jest.Expect[/* expect */0](Curry._1(Binding.Nominal[/* hash */4], tm$3))),
                  /* [] */0
                ]
              ]
            ], Util.id);
        var tm$4 = /* Sequence */Block.__(2, [/* [] */0]);
        Jest.testAll("[]", /* :: */[
              Jest.Expect[/* toEqual */12](( ["s", []] ), Jest.Expect[/* expect */0](Curry._1(Binding.Nominal[/* jsonify */2], tm$4))),
              /* :: */[
                Jest.Expect[/* toBe */2]("82617380", Jest.Expect[/* expect */0](serialize(tm$4))),
                /* :: */[
                  Jest.Expect[/* toBe */2]("8afbfb879b5a95214c4c483c401313235040663bbdc08220992a5841801a421e", Jest.Expect[/* expect */0](Curry._1(Binding.Nominal[/* hash */4], tm$4))),
                  /* [] */0
                ]
              ]
            ], Util.id);
        var tm$5 = /* Sequence */Block.__(2, [/* :: */[
              /* Var */Block.__(1, ["x"]),
              /* [] */0
            ]]);
        return Jest.testAll("[x]", /* :: */[
                    Jest.Expect[/* toEqual */12](( ["s", [["v", "x"]]] ), Jest.Expect[/* expect */0](Curry._1(Binding.Nominal[/* jsonify */2], tm$5))),
                    /* :: */[
                      Jest.Expect[/* toBe */2]("826173818261766178", Jest.Expect[/* expect */0](serialize(tm$5))),
                      /* :: */[
                        Jest.Expect[/* toBe */2]("28b6e8f2124dd5931d69e1a5350f5c44ebdec7e0f6be9f98d2c717fcf09fa3d8", Jest.Expect[/* expect */0](Curry._1(Binding.Nominal[/* hash */4], tm$5))),
                        /* [] */0
                      ]
                    ]
                  ], Util.id);
      }));

/*  Not a pure module */
