// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var Core = require("../src/Core.bs.js");
var Jest = require("@glennsl/bs-jest/src/jest.js");
var Util = require("../src/Util.bs.js");
var Block = require("bs-platform/lib/js/block.js");
var Bigint = require("bs-zarith/src/Bigint.js");

Jest.describe("Core", (function (param) {
        var one = Bigint.of_int(1);
        Jest.testAll("val_to_ast", /* :: */[
              Jest.Expect[/* toEqual */12](/* Primitive */Block.__(3, [/* PrimInteger */Block.__(0, [one])]), Jest.Expect[/* expect */0](Core.val_to_ast(/* PrimVal */Block.__(1, [/* PrimInteger */Block.__(0, [one])])))),
              /* :: */[
                Jest.Expect[/* toEqual */12](/* Operator */Block.__(0, [
                        "lam",
                        /* :: */[
                          /* Scope */[
                            /* :: */[
                              "x",
                              /* :: */[
                                "y",
                                /* [] */0
                              ]
                            ],
                            /* Var */Block.__(1, ["x"])
                          ],
                          /* [] */0
                        ]
                      ]), Jest.Expect[/* expect */0](Core.val_to_ast(/* LamVal */Block.__(2, [
                                /* :: */[
                                  "x",
                                  /* :: */[
                                    "y",
                                    /* [] */0
                                  ]
                                ],
                                /* CoreVar */Block.__(0, ["x"])
                              ])))),
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
                        ]), Jest.Expect[/* expect */0](Core.val_to_ast(/* OperatorVal */Block.__(0, [
                                  "foo",
                                  /* :: */[
                                    /* PrimVal */Block.__(1, [/* PrimInteger */Block.__(0, [one])]),
                                    /* [] */0
                                  ]
                                ])))),
                  /* [] */0
                ]
              ]
            ], Util.id);
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
              /* CoreVal */Block.__(1, [/* OperatorVal */Block.__(0, [
                      "true",
                      /* [] */0
                    ])])
            ],
            /* :: */[
              /* tuple */[
                /* DPatternTm */Block.__(0, [
                    "false",
                    /* [] */0
                  ]),
                /* CoreVal */Block.__(1, [/* OperatorVal */Block.__(0, [
                        "false",
                        /* [] */0
                      ])])
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
                  /* Case */Block.__(3, [
                      /* Meaning */Block.__(5, ["t1"]),
                      sort,
                      /* :: */[
                        /* tuple */[
                          /* PatternTerm */Block.__(0, [
                              "true",
                              /* [] */0
                            ]),
                          /* Meaning */Block.__(5, ["t2"])
                        ],
                        /* :: */[
                          /* tuple */[
                            /* PatternTerm */Block.__(0, [
                                "false",
                                /* [] */0
                              ]),
                            /* Meaning */Block.__(5, ["t3"])
                          ],
                          /* [] */0
                        ]
                      ]
                    ])
                ],
                /* [] */0
              ]
            ]
          ]];
        var true_tm = /* Operator */Block.__(0, [
            "true",
            /* [] */0
          ]);
        var false_tm = /* Operator */Block.__(0, [
            "false",
            /* [] */0
          ]);
        var true_val = /* CoreVal */Block.__(1, [/* OperatorVal */Block.__(0, [
                "true",
                /* [] */0
              ])]);
        var false_val = /* CoreVal */Block.__(1, [/* OperatorVal */Block.__(0, [
                "false",
                /* [] */0
              ])]);
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
            false_val
          ],
          /* :: */[
            /* tuple */[
              /* PatternTerm */Block.__(0, [
                  "false",
                  /* [] */0
                ]),
              true_val
            ],
            /* [] */0
          ]
        ];
        var ite_val = /* Case */Block.__(3, [
            true_val,
            sort,
            ite_val_002
          ]);
        Jest.testAll("term_to_core", /* :: */[
              Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [true_val]), Jest.Expect[/* expect */0](Core.term_to_core(dynamics, true_tm))),
              /* :: */[
                Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [false_val]), Jest.Expect[/* expect */0](Core.term_to_core(dynamics, false_tm))),
                /* :: */[
                  Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [ite_val]), Jest.Expect[/* expect */0](Core.term_to_core(dynamics, ite_tm))),
                  /* [] */0
                ]
              ]
            ], Util.id);
        return Jest.testAll("eval", /* :: */[
                    Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [/* OperatorVal */Block.__(0, [
                                "true",
                                /* [] */0
                              ])]), Jest.Expect[/* expect */0](Core.$$eval(true_val))),
                    /* :: */[
                      Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [/* OperatorVal */Block.__(0, [
                                  "false",
                                  /* [] */0
                                ])]), Jest.Expect[/* expect */0](Core.$$eval(false_val))),
                      /* :: */[
                        Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [/* OperatorVal */Block.__(0, [
                                    "false",
                                    /* [] */0
                                  ])]), Jest.Expect[/* expect */0](Core.$$eval(ite_val))),
                        /* [] */0
                      ]
                    ]
                  ], Util.id);
      }));

/*  Not a pure module */
