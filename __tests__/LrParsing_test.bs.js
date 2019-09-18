// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.js");
var Util = require("../src/Util.bs.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var LrParsing = require("../src/LrParsing.bs.js");
var Belt_MapInt = require("bs-platform/lib/js/belt_MapInt.js");
var Belt_SetInt = require("bs-platform/lib/js/belt_SetInt.js");
var Belt_MutableSet = require("bs-platform/lib/js/belt_MutableSet.js");

var grammar_000 = /* nonterminals */Belt_MapInt.fromArray(/* array */[
      /* tuple */[
        0,
        /* record */[/* productions : :: */[
            /* :: */[
              /* Nonterminal */Block.__(1, [1]),
              /* [] */0
            ],
            /* [] */0
          ]]
      ],
      /* tuple */[
        1,
        /* record */[/* productions : :: */[
            /* :: */[
              /* Nonterminal */Block.__(1, [1]),
              /* :: */[
                /* Terminal */Block.__(0, [0]),
                /* :: */[
                  /* Nonterminal */Block.__(1, [2]),
                  /* [] */0
                ]
              ]
            ],
            /* :: */[
              /* :: */[
                /* Nonterminal */Block.__(1, [2]),
                /* [] */0
              ],
              /* [] */0
            ]
          ]]
      ],
      /* tuple */[
        2,
        /* record */[/* productions : :: */[
            /* :: */[
              /* Nonterminal */Block.__(1, [2]),
              /* :: */[
                /* Terminal */Block.__(0, [1]),
                /* :: */[
                  /* Nonterminal */Block.__(1, [3]),
                  /* [] */0
                ]
              ]
            ],
            /* :: */[
              /* :: */[
                /* Nonterminal */Block.__(1, [3]),
                /* [] */0
              ],
              /* [] */0
            ]
          ]]
      ],
      /* tuple */[
        3,
        /* record */[/* productions : :: */[
            /* :: */[
              /* Terminal */Block.__(0, [2]),
              /* :: */[
                /* Nonterminal */Block.__(1, [1]),
                /* :: */[
                  /* Terminal */Block.__(0, [3]),
                  /* [] */0
                ]
              ]
            ],
            /* :: */[
              /* :: */[
                /* Terminal */Block.__(0, [4]),
                /* [] */0
              ],
              /* [] */0
            ]
          ]]
      ]
    ]);

var grammar = /* record */[
  grammar_000,
  /* num_terminals */5
];

var Grammar = /* module */[/* grammar */grammar];

Jest.describe("LrParsing", (function (param) {
        var Lr0$prime = LrParsing.Lr0(Grammar);
        Jest.testAll("mk_item / view_item", /* :: */[
              Jest.Expect[/* toEqual */12](/* record */[
                    /* production_num */0,
                    /* position */0
                  ], Jest.Expect[/* expect */0](LrParsing.view_item(LrParsing.mk_item$prime(0, 0)))),
              /* :: */[
                Jest.Expect[/* toEqual */12](/* record */[
                      /* production_num */0,
                      /* position */1
                    ], Jest.Expect[/* expect */0](LrParsing.view_item(LrParsing.mk_item$prime(0, 1)))),
                /* :: */[
                  Jest.Expect[/* toEqual */12](/* record */[
                        /* production_num */1,
                        /* position */0
                      ], Jest.Expect[/* expect */0](LrParsing.view_item(LrParsing.mk_item$prime(1, 0)))),
                  /* :: */[
                    Jest.Expect[/* toEqual */12](/* record */[
                          /* production_num */1,
                          /* position */1
                        ], Jest.Expect[/* expect */0](LrParsing.view_item(LrParsing.mk_item$prime(1, 1)))),
                    /* [] */0
                  ]
                ]
              ]
            ], Util.id);
        var items0 = /* array */[LrParsing.mk_item$prime(0, 0)];
        var expected0_000 = /* kernel_items */Belt_SetInt.fromArray(items0);
        var expected0_001 = /* nonkernel_items */Belt_SetInt.fromArray(/* array */[
              LrParsing.mk_item$prime(1, 0),
              LrParsing.mk_item$prime(2, 0),
              LrParsing.mk_item$prime(3, 0),
              LrParsing.mk_item$prime(4, 0),
              LrParsing.mk_item$prime(5, 0),
              LrParsing.mk_item$prime(6, 0)
            ]);
        var expected0 = /* record */[
          expected0_000,
          expected0_001
        ];
        var items1 = /* array */[
          LrParsing.mk_item$prime(0, 1),
          LrParsing.mk_item$prime(1, 1)
        ];
        var expected1_000 = /* kernel_items */Belt_SetInt.fromArray(items1);
        var expected1_001 = /* nonkernel_items */Belt_SetInt.fromArray(/* array */[]);
        var expected1 = /* record */[
          expected1_000,
          expected1_001
        ];
        var items7 = /* array */[LrParsing.mk_item$prime(3, 2)];
        var expected7_000 = /* kernel_items */Belt_SetInt.fromArray(items7);
        var expected7_001 = /* nonkernel_items */Belt_SetInt.fromArray(/* array */[
              LrParsing.mk_item$prime(5, 0),
              LrParsing.mk_item$prime(6, 0)
            ]);
        var expected7 = /* record */[
          expected7_000,
          expected7_001
        ];
        Jest.testAll("closure", /* :: */[
              Jest.Expect[/* toEqual */12](expected0, Jest.Expect[/* expect */0](Curry._1(Lr0$prime[/* closure */7], Belt_SetInt.fromArray(items0)))),
              /* :: */[
                Jest.Expect[/* toEqual */12](expected1, Jest.Expect[/* expect */0](Curry._1(Lr0$prime[/* closure */7], Belt_SetInt.fromArray(items1)))),
                /* :: */[
                  Jest.Expect[/* toEqual */12](expected7, Jest.Expect[/* expect */0](Curry._1(Lr0$prime[/* closure */7], Belt_SetInt.fromArray(items7)))),
                  /* [] */0
                ]
              ]
            ], Util.id);
        var goto_kernel = Belt_SetInt.fromArray(/* array */[LrParsing.mk_item$prime(1, 2)]);
        var goto_nonkernel = Belt_SetInt.fromArray(/* array */[
              LrParsing.mk_item$prime(3, 0),
              LrParsing.mk_item$prime(4, 0),
              LrParsing.mk_item$prime(5, 0),
              LrParsing.mk_item$prime(6, 0)
            ]);
        Jest.testAll("goto", /* :: */[
              Jest.Expect[/* toEqual */12](goto_kernel, Jest.Expect[/* expect */0](Curry._2(Lr0$prime[/* goto_kernel */10], Belt_SetInt.fromArray(items1), /* Terminal */Block.__(0, [0])))),
              /* :: */[
                Jest.Expect[/* toEqual */12](/* record */[
                      /* kernel_items */goto_kernel,
                      /* nonkernel_items */goto_nonkernel
                    ], Jest.Expect[/* expect */0](Curry._2(Lr0$prime[/* goto */11], Belt_SetInt.fromArray(items1), /* Terminal */Block.__(0, [0])))),
                /* [] */0
              ]
            ], Util.id);
        var expected_item_sets = Belt_MutableSet.fromArray(/* array */[
              Belt_SetInt.fromArray(/* array */[
                    LrParsing.mk_item$prime(0, 0),
                    LrParsing.mk_item$prime(1, 0),
                    LrParsing.mk_item$prime(2, 0),
                    LrParsing.mk_item$prime(3, 0),
                    LrParsing.mk_item$prime(4, 0),
                    LrParsing.mk_item$prime(5, 0),
                    LrParsing.mk_item$prime(6, 0)
                  ]),
              Belt_SetInt.fromArray(/* array */[
                    LrParsing.mk_item$prime(0, 1),
                    LrParsing.mk_item$prime(1, 1)
                  ]),
              Belt_SetInt.fromArray(/* array */[
                    LrParsing.mk_item$prime(2, 1),
                    LrParsing.mk_item$prime(3, 1)
                  ]),
              Belt_SetInt.fromArray(/* array */[LrParsing.mk_item$prime(4, 1)]),
              Belt_SetInt.fromArray(/* array */[
                    LrParsing.mk_item$prime(5, 1),
                    LrParsing.mk_item$prime(1, 0),
                    LrParsing.mk_item$prime(2, 0),
                    LrParsing.mk_item$prime(3, 0),
                    LrParsing.mk_item$prime(4, 0),
                    LrParsing.mk_item$prime(5, 0),
                    LrParsing.mk_item$prime(6, 0)
                  ]),
              Belt_SetInt.fromArray(/* array */[LrParsing.mk_item$prime(6, 1)]),
              Belt_SetInt.fromArray(/* array */[
                    LrParsing.mk_item$prime(1, 2),
                    LrParsing.mk_item$prime(3, 0),
                    LrParsing.mk_item$prime(4, 0),
                    LrParsing.mk_item$prime(5, 0),
                    LrParsing.mk_item$prime(6, 0)
                  ]),
              Belt_SetInt.fromArray(/* array */[
                    LrParsing.mk_item$prime(3, 2),
                    LrParsing.mk_item$prime(5, 0),
                    LrParsing.mk_item$prime(6, 0)
                  ]),
              Belt_SetInt.fromArray(/* array */[
                    LrParsing.mk_item$prime(1, 1),
                    LrParsing.mk_item$prime(5, 2)
                  ]),
              Belt_SetInt.fromArray(/* array */[
                    LrParsing.mk_item$prime(1, 3),
                    LrParsing.mk_item$prime(3, 1)
                  ]),
              Belt_SetInt.fromArray(/* array */[LrParsing.mk_item$prime(3, 3)]),
              Belt_SetInt.fromArray(/* array */[LrParsing.mk_item$prime(5, 3)])
            ], LrParsing.ComparableSet);
        var normalize = function (items) {
          return Belt_List.map(Belt_MutableSet.toList(items), Belt_SetInt.toList);
        };
        return Jest.testAll("items", /* :: */[
                    Jest.Expect[/* toEqual */12](normalize(expected_item_sets), Jest.Expect[/* expect */0](normalize(Lr0$prime[/* items */12]))),
                    /* [] */0
                  ], Util.id);
      }));

var M = 0;

var SI = 0;

exports.M = M;
exports.SI = SI;
exports.Grammar = Grammar;
/* grammar Not a pure module */
