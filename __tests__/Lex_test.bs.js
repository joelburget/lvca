// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Lex = require("../src/Lex.bs.js");
var Jest = require("@glennsl/bs-jest/src/jest.js");
var Block = require("bs-platform/lib/js/block.js");

Jest.describe("Lex", (function (param) {
        var result = Lex.lex(/* :: */[
              /* tuple */[
                "if",
                "IF"
              ],
              /* :: */[
                /* tuple */[
                  "then",
                  "THEN"
                ],
                /* :: */[
                  /* tuple */[
                    "else",
                    "ELSE"
                  ],
                  /* :: */[
                    /* tuple */[
                      "<|>|<=|>=|==|!=",
                      "OP"
                    ],
                    /* :: */[
                      /* tuple */[
                        "[a-zA-Z][a-zA-Z0-9_]*",
                        "ID"
                      ],
                      /* :: */[
                        /* tuple */[
                          "\\d+",
                          "NUM"
                        ],
                        /* :: */[
                          /* tuple */[
                            "\".*\"",
                            "LIT"
                          ],
                          /* :: */[
                            /* tuple */[
                              "\\s+",
                              "WHITE"
                            ],
                            /* [] */0
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ], "if a > b then 90 else 91");
        Jest.test("lex 1", (function (param) {
                return Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [/* array */[
                                /* record */[
                                  /* name */"IF",
                                  /* start */0,
                                  /* finish */2
                                ],
                                /* record */[
                                  /* name */"WHITE",
                                  /* start */2,
                                  /* finish */3
                                ],
                                /* record */[
                                  /* name */"ID",
                                  /* start */3,
                                  /* finish */4
                                ],
                                /* record */[
                                  /* name */"WHITE",
                                  /* start */4,
                                  /* finish */5
                                ],
                                /* record */[
                                  /* name */"OP",
                                  /* start */5,
                                  /* finish */6
                                ],
                                /* record */[
                                  /* name */"WHITE",
                                  /* start */6,
                                  /* finish */7
                                ],
                                /* record */[
                                  /* name */"ID",
                                  /* start */7,
                                  /* finish */8
                                ],
                                /* record */[
                                  /* name */"WHITE",
                                  /* start */8,
                                  /* finish */9
                                ],
                                /* record */[
                                  /* name */"THEN",
                                  /* start */9,
                                  /* finish */13
                                ],
                                /* record */[
                                  /* name */"WHITE",
                                  /* start */13,
                                  /* finish */14
                                ],
                                /* record */[
                                  /* name */"NUM",
                                  /* start */14,
                                  /* finish */16
                                ],
                                /* record */[
                                  /* name */"WHITE",
                                  /* start */16,
                                  /* finish */17
                                ],
                                /* record */[
                                  /* name */"ELSE",
                                  /* start */17,
                                  /* finish */21
                                ],
                                /* record */[
                                  /* name */"WHITE",
                                  /* start */21,
                                  /* finish */22
                                ],
                                /* record */[
                                  /* name */"NUM",
                                  /* start */22,
                                  /* finish */24
                                ]
                              ]]), Jest.Expect[/* expect */0](result));
              }));
        var result$1 = Lex.lex(/* :: */[
              /* tuple */[
                "\\+",
                "+"
              ],
              /* :: */[
                /* tuple */[
                  "\\*",
                  "*"
                ],
                /* :: */[
                  /* tuple */[
                    "\\(",
                    "("
                  ],
                  /* :: */[
                    /* tuple */[
                      "\\)",
                      ")"
                    ],
                    /* :: */[
                      /* tuple */[
                        "\\w+",
                        "id"
                      ],
                      /* :: */[
                        /* tuple */[
                          "\\s+",
                          "WHITE"
                        ],
                        /* [] */0
                      ]
                    ]
                  ]
                ]
              ]
            ], "foo + bar");
        Jest.test("lex 2", (function (param) {
                return Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [/* array */[
                                /* record */[
                                  /* name */"id",
                                  /* start */0,
                                  /* finish */3
                                ],
                                /* record */[
                                  /* name */"WHITE",
                                  /* start */3,
                                  /* finish */4
                                ],
                                /* record */[
                                  /* name */"+",
                                  /* start */4,
                                  /* finish */5
                                ],
                                /* record */[
                                  /* name */"WHITE",
                                  /* start */5,
                                  /* finish */6
                                ],
                                /* record */[
                                  /* name */"id",
                                  /* start */6,
                                  /* finish */9
                                ]
                              ]]), Jest.Expect[/* expect */0](result$1));
              }));
        var result$2 = Lex.lex(/* :: */[
              /* tuple */[
                "['a' - 'z' 'A' - 'Z'] ['a' - 'z' 'A' - 'Z' '0' - '9' '_'] *",
                "ID"
              ],
              /* :: */[
                /* tuple */[
                  ":",
                  "COLON"
                ],
                /* :: */[
                  /* tuple */[
                    "if",
                    "IF"
                  ],
                  /* :: */[
                    /* tuple */[
                      "then",
                      "THEN"
                    ],
                    /* :: */[
                      /* tuple */[
                        "else",
                        "ELSE"
                      ],
                      /* :: */[
                        /* tuple */[
                          "fun",
                          "FUN"
                        ],
                        /* :: */[
                          /* tuple */[
                            "->",
                            "ARROW"
                          ],
                          /* :: */[
                            /* tuple */[
                              "true",
                              "TRUE"
                            ],
                            /* :: */[
                              /* tuple */[
                                "false",
                                "FALSE"
                              ],
                              /* :: */[
                                /* tuple */[
                                  "bool",
                                  "BOOL"
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "[ ]+",
                                    "SPACE"
                                  ],
                                  /* [] */0
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ], "if false then false else true");
        return Jest.test("lex 3", (function (param) {
                      return Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [/* array */[
                                      /* record */[
                                        /* name */"IF",
                                        /* start */0,
                                        /* finish */2
                                      ],
                                      /* record */[
                                        /* name */"SPACE",
                                        /* start */2,
                                        /* finish */3
                                      ],
                                      /* record */[
                                        /* name */"FALSE",
                                        /* start */3,
                                        /* finish */8
                                      ],
                                      /* record */[
                                        /* name */"SPACE",
                                        /* start */8,
                                        /* finish */9
                                      ],
                                      /* record */[
                                        /* name */"THEN",
                                        /* start */9,
                                        /* finish */13
                                      ],
                                      /* record */[
                                        /* name */"SPACE",
                                        /* start */13,
                                        /* finish */14
                                      ],
                                      /* record */[
                                        /* name */"FALSE",
                                        /* start */14,
                                        /* finish */19
                                      ],
                                      /* record */[
                                        /* name */"SPACE",
                                        /* start */19,
                                        /* finish */20
                                      ],
                                      /* record */[
                                        /* name */"ELSE",
                                        /* start */20,
                                        /* finish */24
                                      ],
                                      /* record */[
                                        /* name */"SPACE",
                                        /* start */24,
                                        /* finish */25
                                      ],
                                      /* record */[
                                        /* name */"TRUE",
                                        /* start */25,
                                        /* finish */29
                                      ]
                                    ]]), Jest.Expect[/* expect */0](result$2));
                    }));
      }));

/*  Not a pure module */
