// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var Core = require("../src/Core.bs.js");
var Jest = require("@glennsl/bs-jest/src/jest.js");
var Util = require("../src/Util.bs.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Bigint = require("bs-zarith/src/Bigint.js");
var Parsing = require("../src/Parsing.bs.js");
var Belt_Result = require("bs-platform/lib/js/belt_Result.js");

Jest.describe("Core", (function (param) {
        var one = Bigint.of_int(1);
        var sort_001 = /* array */[];
        var sort = /* SortAp */[
          "bool",
          sort_001
        ];
        var dynamics = /* DenotationChart */[/* :: */[
            /* tuple */[
              /* DPatternTm */Block.__(0, [
                  "true",
                  /* [] */0
                ]),
              /* Operator */Block.__(0, [
                  "true",
                  /* [] */0
                ])
            ],
            /* :: */[
              /* tuple */[
                /* DPatternTm */Block.__(0, [
                    "false",
                    /* [] */0
                  ]),
                /* Operator */Block.__(0, [
                    "false",
                    /* [] */0
                  ])
              ],
              /* :: */[
                /* tuple */[
                  /* DPatternTm */Block.__(0, [
                      "ite",
                      /* :: */[
                        /* DenotationScopePat */[
                          /* [] */0,
                          /* DVar */Block.__(1, ["t1"])
                        ],
                        /* :: */[
                          /* DenotationScopePat */[
                            /* [] */0,
                            /* DVar */Block.__(1, ["t2"])
                          ],
                          /* :: */[
                            /* DenotationScopePat */[
                              /* [] */0,
                              /* DVar */Block.__(1, ["t3"])
                            ],
                            /* [] */0
                          ]
                        ]
                      ]
                    ]),
                  /* Case */Block.__(6, [
                      /* Meaning */Block.__(8, ["t1"]),
                      sort,
                      /* :: */[
                        /* tuple */[
                          /* PatternTerm */Block.__(0, [
                              "true",
                              /* [] */0
                            ]),
                          /* CoreScope */[
                            /* [] */0,
                            /* Meaning */Block.__(8, ["t2"])
                          ]
                        ],
                        /* :: */[
                          /* tuple */[
                            /* PatternTerm */Block.__(0, [
                                "false",
                                /* [] */0
                              ]),
                            /* CoreScope */[
                              /* [] */0,
                              /* Meaning */Block.__(8, ["t3"])
                            ]
                          ],
                          /* [] */0
                        ]
                      ]
                    ])
                ],
                /* :: */[
                  /* tuple */[
                    /* DPatternTm */Block.__(0, [
                        "ap",
                        /* :: */[
                          /* DenotationScopePat */[
                            /* [] */0,
                            /* DVar */Block.__(1, ["f"])
                          ],
                          /* :: */[
                            /* DenotationScopePat */[
                              /* [] */0,
                              /* DVar */Block.__(1, ["arg"])
                            ],
                            /* [] */0
                          ]
                        ]
                      ]),
                    /* CoreApp */Block.__(5, [
                        /* Meaning */Block.__(8, ["f"]),
                        /* :: */[
                          /* Meaning */Block.__(8, ["arg"]),
                          /* [] */0
                        ]
                      ])
                  ],
                  /* :: */[
                    /* tuple */[
                      /* DPatternTm */Block.__(0, [
                          "fun",
                          /* :: */[
                            /* DenotationScopePat */[
                              /* :: */[
                                "v",
                                /* [] */0
                              ],
                              /* DVar */Block.__(1, ["body"])
                            ],
                            /* [] */0
                          ]
                        ]),
                      /* Lambda */Block.__(4, [/* CoreScope */[
                            /* :: */[
                              "v",
                              /* [] */0
                            ],
                            /* Meaning */Block.__(8, ["body"])
                          ]])
                    ],
                    /* [] */0
                  ]
                ]
              ]
            ]
          ]];
        var P_lang = Parsing.Incremental(Parsing.Parseable_language);
        var P_dyn = Parsing.Incremental(Parsing.Parseable_dynamics);
        Curry._1(P_lang[/* parse */5], "\n  bool := true() | false()\n  expr :=\n    | lit(bool)\n    | ite(expr; expr; expr)\n  ");
        var lit_dynamics = Belt_Result.getExn(Curry._1(P_dyn[/* parse */5], "\n  [[ lit(b) ]] = b\n  [[ ite(t; l; r) ]] = case(\n    [[ t ]] : bool;\n    true()  -> [[ l ]];\n    false() -> [[ r ]]\n  )\n  "));
        var true_tm = /* Operator */Block.__(0, [
            "true",
            /* [] */0
          ]);
        var false_tm = /* Operator */Block.__(0, [
            "false",
            /* [] */0
          ]);
        var true_val = /* Operator */Block.__(0, [
            "true",
            /* [] */0
          ]);
        var false_val = /* Operator */Block.__(0, [
            "false",
            /* [] */0
          ]);
        var ite_tm_001 = /* :: */[
          /* Scope */[
            /* [] */0,
            true_tm
          ],
          /* :: */[
            /* Scope */[
              /* [] */0,
              false_tm
            ],
            /* :: */[
              /* Scope */[
                /* [] */0,
                true_tm
              ],
              /* [] */0
            ]
          ]
        ];
        var ite_tm = /* Operator */Block.__(0, [
            "ite",
            ite_tm_001
          ]);
        var ite_val_002 = /* :: */[
          /* tuple */[
            /* PatternTerm */Block.__(0, [
                "true",
                /* [] */0
              ]),
            /* CoreScope */[
              /* [] */0,
              false_val
            ]
          ],
          /* :: */[
            /* tuple */[
              /* PatternTerm */Block.__(0, [
                  "false",
                  /* [] */0
                ]),
              /* CoreScope */[
                /* [] */0,
                true_val
              ]
            ],
            /* [] */0
          ]
        ];
        var ite_val = /* Case */Block.__(6, [
            true_val,
            sort,
            ite_val_002
          ]);
        var fun_tm_001 = /* :: */[
          /* Scope */[
            /* [] */0,
            /* Operator */Block.__(0, [
                "fun",
                /* :: */[
                  /* Scope */[
                    /* :: */[
                      "x",
                      /* [] */0
                    ],
                    /* Var */Block.__(1, [0])
                  ],
                  /* [] */0
                ]
              ])
          ],
          /* :: */[
            /* Scope */[
              /* [] */0,
              true_tm
            ],
            /* [] */0
          ]
        ];
        var fun_tm = /* Operator */Block.__(0, [
            "ap",
            fun_tm_001
          ]);
        var fun_val_000 = /* Lambda */Block.__(4, [/* CoreScope */[
              /* :: */[
                "x",
                /* [] */0
              ],
              /* Var */Block.__(1, ["x"])
            ]]);
        var fun_val_001 = /* :: */[
          true_val,
          /* [] */0
        ];
        var fun_val = /* CoreApp */Block.__(5, [
            fun_val_000,
            fun_val_001
          ]);
        Jest.testAll("to_ast", /* :: */[
              Jest.Expect[/* toEqual */12](/* Primitive */Block.__(3, [/* PrimInteger */Block.__(0, [one])]), Jest.Expect[/* expect */0](Core.to_ast(/* Primitive */Block.__(3, [/* PrimInteger */Block.__(0, [one])])))),
              /* :: */[
                Jest.Expect[/* toEqual */12](/* Operator */Block.__(0, [
                        "foo",
                        /* :: */[
                          /* Scope */[
                            /* [] */0,
                            /* Primitive */Block.__(3, [/* PrimInteger */Block.__(0, [one])])
                          ],
                          /* [] */0
                        ]
                      ]), Jest.Expect[/* expect */0](Core.to_ast(/* Operator */Block.__(0, [
                                "foo",
                                /* :: */[
                                  /* CoreScope */[
                                    /* [] */0,
                                    /* Primitive */Block.__(3, [/* PrimInteger */Block.__(0, [one])])
                                  ],
                                  /* [] */0
                                ]
                              ])))),
                /* [] */0
              ]
            ], Util.id);
        Jest.testAll("term_denotation", /* :: */[
              Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [true_val]), Jest.Expect[/* expect */0](Core.term_denotation(dynamics, /* [] */0, true_tm))),
              /* :: */[
                Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [false_val]), Jest.Expect[/* expect */0](Core.term_denotation(dynamics, /* [] */0, false_tm))),
                /* :: */[
                  Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [ite_val]), Jest.Expect[/* expect */0](Core.term_denotation(dynamics, /* [] */0, ite_tm))),
                  /* :: */[
                    Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [fun_val]), Jest.Expect[/* expect */0](Core.term_denotation(dynamics, /* [] */0, fun_tm))),
                    /* :: */[
                      Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [true_val]), Jest.Expect[/* expect */0](Core.term_denotation(lit_dynamics, /* [] */0, /* Operator */Block.__(0, [
                                      "lit",
                                      /* :: */[
                                        /* Scope */[
                                          /* [] */0,
                                          true_tm
                                        ],
                                        /* [] */0
                                      ]
                                    ])))),
                      /* [] */0
                    ]
                  ]
                ]
              ]
            ], Util.id);
        return Jest.testAll("eval", /* :: */[
                    Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [true_val]), Jest.Expect[/* expect */0](Core.$$eval(true_val))),
                    /* :: */[
                      Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [false_val]), Jest.Expect[/* expect */0](Core.$$eval(false_val))),
                      /* :: */[
                        Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [false_val]), Jest.Expect[/* expect */0](Core.$$eval(ite_val))),
                        /* :: */[
                          Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [true_val]), Jest.Expect[/* expect */0](Core.$$eval(fun_val))),
                          /* [] */0
                        ]
                      ]
                    ]
                  ], Util.id);
      }));

/*  Not a pure module */
