// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Core = require("./Core.bs.js");
var List = require("bs-platform/lib/js/list.js");
var Util = require("./Util.bs.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");
var Binding = require("./Binding.bs.js");
var Parsing = require("./Parsing.bs.js");
var EvalView = require("./EvalView.bs.js");
var LrParsing = require("./LrParsing.bs.js");
var CodeMirror = require("./CodeMirror.bs.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var ReactDOMRe = require("reason-react/src/ReactDOMRe.js");
var Belt_Result = require("bs-platform/lib/js/belt_Result.js");
var ParseStatus = require("./ParseStatus.bs.js");
var LrParsingView = require("./LrParsingView.bs.js");
var ConcreteSyntax = require("./ConcreteSyntax.bs.js");
var LanguageSimple = require("./LanguageSimple.bs.js");
var Caml_splice_call = require("bs-platform/lib/js/caml_splice_call.js");
var ReactCodemirror2 = require("react-codemirror2");
var Caml_js_exceptions = require("bs-platform/lib/js/caml_js_exceptions.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function read_eval_input(language, concrete, statics, dynamics, input) {
  var match;
  var exit = 0;
  var val;
  try {
    val = ConcreteSyntax.parse(concrete, input);
    exit = 1;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === Util.InvariantViolation) {
      var msg = exn[1];
      match = /* tuple */[
        /* Error */Block.__(1, [/* tuple */[
              msg,
              undefined
            ]]),
        /* Error */Block.__(1, [msg])
      ];
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    if (val.tag) {
      var msg$1 = val[0];
      match = /* tuple */[
        /* Error */Block.__(1, [/* tuple */[
              msg$1,
              undefined
            ]]),
        /* Error */Block.__(1, [msg$1])
      ];
    } else {
      var match$1 = ConcreteSyntax.to_ast(language, val[0]);
      if (match$1.tag) {
        var msg$2 = match$1[0];
        match = /* tuple */[
          /* Error */Block.__(1, [/* tuple */[
                msg$2,
                undefined
              ]]),
          /* Error */Block.__(1, [msg$2])
        ];
      } else {
        var ast = match$1[0];
        match = /* tuple */[
          /* Ok */Block.__(0, [ast]),
          Curry._1(Binding.DeBruijn[/* from_nominal */1], ast)
        ];
      }
    }
  }
  var abtResult = match[1];
  var astResult = match[0];
  var eval$prime = function (tm) {
    return Util.map_error(Core.$$eval(tm), (function (msg) {
                  return /* tuple */[
                          msg,
                          undefined
                        ];
                }));
  };
  if (abtResult.tag) {
    var msg$3 = abtResult[0];
    return /* tuple */[
            /* Error */Block.__(1, [/* tuple */[
                  msg$3,
                  undefined
                ]]),
            /* Error */Block.__(1, [/* tuple */[
                  msg$3,
                  undefined
                ]])
          ];
  } else {
    var match$2 = Belt_Result.flatMap(Core.term_denotation(dynamics, /* [] */0, abtResult[0]), eval$prime);
    if (match$2.tag) {
      return /* tuple */[
              astResult,
              /* Error */Block.__(1, [match$2[0]])
            ];
    } else {
      return /* tuple */[
              astResult,
              /* Ok */Block.__(0, [match$2[0]])
            ];
    }
  }
}

function shift_from_to(shift_from, shift_to, elem) {
  if (shift_from) {
    var shift_to$prime = /* :: */[
      elem,
      shift_to
    ];
    return /* tuple */[
            shift_from[1],
            shift_to$prime,
            shift_from[0]
          ];
  } else {
    return /* tuple */[
            shift_from,
            shift_to,
            elem
          ];
  }
}

function step_forward(language, concrete, statics, dynamics, param) {
  var input = param[/* input */2];
  var match = read_eval_input(language, concrete, statics, dynamics, input);
  var match$1 = shift_from_to(param[/* after */1], param[/* before */0], /* record */[
        /* input */input,
        /* parsed */match[0],
        /* result */match[1]
      ]);
  return /* record */[
          /* before */match$1[1],
          /* after */match$1[0],
          /* input */match$1[2][/* input */0]
        ];
}

function step_back(language, concrete, statics, dynamics, param) {
  var input = param[/* input */2];
  var match = read_eval_input(language, concrete, statics, dynamics, input);
  var match$1 = shift_from_to(param[/* before */0], param[/* after */1], /* record */[
        /* input */input,
        /* parsed */match[0],
        /* result */match[1]
      ]);
  return /* record */[
          /* before */match$1[0],
          /* after */match$1[1],
          /* input */match$1[2][/* input */0]
        ];
}

function go_back(lang, concrete, statics, dyn, hist, i) {
  if (i !== 0) {
    return step_back(lang, concrete, statics, dyn, go_back(lang, concrete, statics, dyn, hist, i - 1 | 0));
  } else {
    return hist;
  }
}

function go_forward(lang, concrete, statics, dyn, hist, i) {
  if (i !== 0) {
    return step_forward(lang, concrete, statics, dyn, go_forward(lang, concrete, statics, dyn, hist, i - 1 | 0));
  } else {
    return hist;
  }
}

function make_div(children) {
  return Caml_splice_call.spliceApply(React.createElement, [
              "div",
              { },
              children
            ]);
}

var preventDefault = (evt => evt.preventDefault());

function Index$Repl(Props) {
  var history = Props.history;
  var language = Props.language;
  var concrete = Props.concrete;
  var statics = Props.statics;
  var dynamics = Props.dynamics;
  var setInput = Props.setInput;
  var handleEnter = Props.handleEnter;
  var handleUp = Props.handleUp;
  var handleDown = Props.handleDown;
  var input = history[/* input */2];
  var options = {
    mode: "lvca"
  };
  var match = read_eval_input(language, concrete, statics, dynamics, input);
  var handleKey = function (_editor, evt) {
    var key = evt.key;
    var shift = evt.shiftKey;
    if (shift) {
      if (key === "Enter") {
        Curry._1(preventDefault, evt);
        return Curry._1(handleEnter, /* () */0);
      } else if (key === "ArrowUp") {
        Curry._1(preventDefault, evt);
        return Curry._1(handleUp, 1);
      } else if (key === "ArrowDown") {
        Curry._1(preventDefault, evt);
        return Curry._1(handleDown, 1);
      } else {
        return 0;
      }
    } else {
      return 0;
    }
  };
  var beforeElems = List.rev(List.mapi((function (i, param) {
              return React.createElement("div", {
                          className: "history-item"
                        }, React.createElement("div", {
                              className: "history-input",
                              onClick: (function (_evt) {
                                  return Curry._1(handleUp, i + 1 | 0);
                                })
                            }, param[/* input */0]), React.createElement("div", {
                              className: "term-view"
                            }, React.createElement(EvalView.make, {
                                  input: param[/* parsed */1],
                                  evalResult: param[/* result */2]
                                })));
            }), history[/* before */0]));
  var afterElems = List.mapi((function (i, param) {
          return React.createElement("div", {
                      className: "history-item"
                    }, React.createElement("div", {
                          className: "history-input",
                          onClick: (function (_evt) {
                              return Curry._1(handleDown, i + 1 | 0);
                            })
                        }, param[/* input */0]), React.createElement("div", {
                          className: "term-view"
                        }, React.createElement(EvalView.make, {
                              input: param[/* parsed */1],
                              evalResult: param[/* result */2]
                            })));
        }), history[/* after */1]);
  return React.createElement(React.Fragment, undefined, make_div($$Array.of_list(beforeElems)), React.createElement("div", undefined, React.createElement("div", {
                      className: "term-input"
                    }, React.createElement(ReactCodemirror2.Controlled, {
                          value: input,
                          onBeforeChange: (function (_editor, _data, value) {
                              return Curry._1(setInput, value);
                            }),
                          options: options,
                          onKeyDown: handleKey,
                          editorDidMount: CodeMirror.focusEditor
                        })), React.createElement("div", {
                      className: "term-view"
                    }, React.createElement(EvalView.make, {
                          input: match[0],
                          evalResult: match[1]
                        }))), make_div($$Array.of_list(afterElems)));
}

var Repl = /* module */[
  /* preventDefault */preventDefault,
  /* make */Index$Repl
];

function Index$AbstractSyntaxEditor(Props) {
  var onContinue = Props.onContinue;
  var match = React.useState((function () {
          return LanguageSimple.abstractSyntax;
        }));
  var setAsInput = match[1];
  var asInput = match[0];
  var Parseable_language$prime = ParseStatus.Make(Parsing.Parseable_language);
  var match$1 = Curry._1(Parseable_language$prime[/* parse */1], asInput);
  var language = match$1[1];
  var continueView;
  if (language.tag) {
    continueView = null;
  } else {
    var language$prime = language[0];
    continueView = React.createElement("button", {
          onClick: (function (param) {
              return Curry._1(onContinue, language$prime);
            })
        }, "continue");
  }
  return React.createElement("div", undefined, React.createElement("h2", {
                  className: "header2 header2-abstract-syntax"
                }, "Abstract Syntax ", match$1[0]), React.createElement("div", {
                  className: "abstract-syntax-pane"
                }, React.createElement(ReactCodemirror2.Controlled, {
                      value: asInput,
                      onBeforeChange: (function (param, param$1, str) {
                          return Curry._1(setAsInput, (function (param) {
                                        return str;
                                      }));
                        }),
                      options: {
                        mode: "default"
                      }
                    })), continueView);
}

var AbstractSyntaxEditor = /* module */[/* make */Index$AbstractSyntaxEditor];

function Index$ConcreteSyntaxEditor(Props) {
  var onComplete = Props.onComplete;
  var match = React.useReducer((function (param, action) {
          var syntaxDesc = param[2];
          var showGrammarPane = param[1];
          if (action) {
            return /* tuple */[
                    action[0],
                    showGrammarPane,
                    syntaxDesc
                  ];
          } else {
            return /* tuple */[
                    param[0],
                    !showGrammarPane,
                    syntaxDesc
                  ];
          }
        }), /* tuple */[
        LanguageSimple.concrete,
        false,
        undefined
      ]);
  var dispatch = match[1];
  var match$1 = match[0];
  var showGrammarPane = match$1[1];
  var concreteInput = match$1[0];
  var Parseable_concrete = ParseStatus.Make(Parsing.Parseable_concrete_syntax);
  var match$2 = Curry._1(Parseable_concrete[/* parse */1], concreteInput);
  var concrete = match$2[1];
  React.useEffect((function () {
          if (concrete.tag) {
            return undefined;
          } else {
            Curry._1(onComplete, concrete[0]);
            return undefined;
          }
        }), /* array */[concrete]);
  var getGrammarPane = function (concrete) {
    var grammar = ConcreteSyntax.to_grammar(concrete);
    var Lr0$prime = LrParsing.Lr0(/* module */[/* grammar */grammar]);
    var action_table = Curry._1(Lr0$prime[/* full_action_table */31], /* () */0);
    var goto_table = Curry._1(Lr0$prime[/* full_goto_table */32], /* () */0);
    return React.createElement(LrParsingView.Tables[/* make */0], {
                grammar: grammar,
                action_table: action_table,
                goto_table: goto_table
              });
  };
  var grammarPane;
  if (concrete.tag) {
    grammarPane = React.createElement("div", undefined, "grammar not available");
  } else {
    try {
      grammarPane = getGrammarPane(concrete[0]);
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn[0] === Util.InvariantViolation) {
        grammarPane = React.createElement("div", undefined, exn[1]);
      } else {
        throw exn;
      }
    }
  }
  return React.createElement("div", undefined, React.createElement("h2", {
                  className: "header2 header2-concrete"
                }, "Concrete Syntax ", match$2[0], React.createElement("button", {
                      onClick: (function (param) {
                          return Curry._1(dispatch, /* ToggleGrammarPane */0);
                        })
                    }, showGrammarPane ? "hide grammar tables" : "show grammar tables")), React.createElement("div", {
                  className: "concrete-pane"
                }, showGrammarPane ? grammarPane : null, React.createElement(ReactCodemirror2.Controlled, {
                      value: concreteInput,
                      onBeforeChange: (function (param, param$1, str) {
                          return Curry._1(dispatch, /* Typing */[str]);
                        }),
                      options: {
                        mode: "default"
                      }
                    })));
}

var ConcreteSyntaxEditor = /* module */[/* make */Index$ConcreteSyntaxEditor];

function Index$StaticsEditor(Props) {
  var onComplete = Props.onComplete;
  var match = React.useState((function () {
          return LanguageSimple.statics;
        }));
  var setStaticsInput = match[1];
  var staticsInput = match[0];
  var Parseable_statics$prime = ParseStatus.Make(Parsing.Parseable_statics);
  var match$1 = Curry._1(Parseable_statics$prime[/* parse */1], staticsInput);
  var statics = match$1[1];
  React.useEffect((function () {
          if (statics.tag) {
            return undefined;
          } else {
            Curry._1(onComplete, statics[0]);
            return undefined;
          }
        }), /* array */[statics]);
  return React.createElement("div", undefined, React.createElement("h2", {
                  className: "header2 header2-statics"
                }, "Statics ", match$1[0]), React.createElement("div", {
                  className: "statics-pane"
                }, React.createElement(ReactCodemirror2.Controlled, {
                      value: staticsInput,
                      onBeforeChange: (function (param, param$1, str) {
                          return Curry._1(setStaticsInput, (function (param) {
                                        return str;
                                      }));
                        }),
                      options: {
                        mode: "default"
                      }
                    })));
}

var StaticsEditor = /* module */[/* make */Index$StaticsEditor];

function Index$DynamicsEditor(Props) {
  var onComplete = Props.onComplete;
  var match = React.useState((function () {
          return LanguageSimple.dynamics;
        }));
  var setDynamicsInput = match[1];
  var dynamicsInput = match[0];
  var Parseable_dynamics$prime = ParseStatus.Make(Parsing.Parseable_dynamics);
  var match$1 = Curry._1(Parseable_dynamics$prime[/* parse */1], dynamicsInput);
  var dynamics = match$1[1];
  React.useEffect((function () {
          if (dynamics.tag) {
            return undefined;
          } else {
            Curry._1(onComplete, dynamics[0]);
            return undefined;
          }
        }), /* array */[dynamics]);
  return React.createElement("div", undefined, React.createElement("h2", {
                  className: "header2 header2-dynamics"
                }, "Dynamics ", match$1[0]), React.createElement("div", {
                  className: "dynamics-pane"
                }, React.createElement(ReactCodemirror2.Controlled, {
                      value: dynamicsInput,
                      onBeforeChange: (function (param, param$1, str) {
                          return Curry._1(setDynamicsInput, (function (param) {
                                        return str;
                                      }));
                        }),
                      options: {
                        mode: "default"
                      }
                    })));
}

var DynamicsEditor = /* module */[/* make */Index$DynamicsEditor];

function Index$ReplPane(Props) {
  var language = Props.language;
  var concrete = Props.concrete;
  var statics = Props.statics;
  var dynamics = Props.dynamics;
  var match = React.useState((function () {
          return /* record */[
                  /* before : [] */0,
                  /* after : [] */0,
                  /* input */"if false then false else true"
                ];
        }));
  var setHistory = match[1];
  return React.createElement("div", undefined, React.createElement("h2", {
                  className: "header2 header2-repl"
                }, "repl"), React.createElement("div", {
                  className: "repl-pane"
                }, React.createElement(Index$Repl, {
                      history: match[0],
                      language: language,
                      concrete: concrete,
                      statics: statics,
                      dynamics: dynamics,
                      setInput: (function (input) {
                          return Curry._1(setHistory, (function (hist) {
                                        return /* record */[
                                                /* before */hist[/* before */0],
                                                /* after */hist[/* after */1],
                                                /* input */input
                                              ];
                                      }));
                        }),
                      handleEnter: (function (param) {
                          return Curry._1(setHistory, (function (hist) {
                                        var input = hist[/* input */2];
                                        var after = hist[/* after */1];
                                        if (after) {
                                          return step_forward(language, concrete, statics, dynamics, hist);
                                        } else {
                                          var match = read_eval_input(language, concrete, statics, dynamics, input);
                                          var before$prime_000 = /* record */[
                                            /* input */input,
                                            /* parsed */match[0],
                                            /* result */match[1]
                                          ];
                                          var before$prime_001 = hist[/* before */0];
                                          var before$prime = /* :: */[
                                            before$prime_000,
                                            before$prime_001
                                          ];
                                          return /* record */[
                                                  /* before */before$prime,
                                                  /* after */after,
                                                  /* input */""
                                                ];
                                        }
                                      }));
                        }),
                      handleUp: (function (n) {
                          return Curry._1(setHistory, (function (hist) {
                                        return go_back(language, concrete, statics, dynamics, hist, n);
                                      }));
                        }),
                      handleDown: (function (n) {
                          return Curry._1(setHistory, (function (hist) {
                                        return go_forward(language, concrete, statics, dynamics, hist, n);
                                      }));
                        })
                    })));
}

var ReplPane = /* module */[/* make */Index$ReplPane];

function mk_details(abstract_syntax) {
  return /* DetailsStage */Block.__(0, [
            /* record */[
              /* abstract_syntax */abstract_syntax,
              /* concrete_syntax */undefined,
              /* statics */undefined,
              /* dynamics */undefined
            ],
            /* ConcreteTab */0
          ]);
}

function change_tab(details, tab) {
  return /* DetailsStage */Block.__(0, [
            details,
            tab
          ]);
}

function Index$LvcaViewer(Props) {
  var match = React.useReducer((function (state, action) {
          if (typeof state === "number") {
            if (typeof action === "number" || action.tag !== 1) {
              return Pervasives.failwith("invariant violation: unexpected action in AbstractSyntaxStage");
            } else {
              return mk_details(action[0]);
            }
          } else if (state.tag) {
            throw [
                  Caml_builtin_exceptions.match_failure,
                  /* tuple */[
                    "Index.re",
                    491,
                    25
                  ]
                ];
          } else {
            var tab = state[1];
            var details = state[0];
            if (typeof action === "number") {
              return Pervasives.failwith("invariant violation: unexpected action in DetailsStage");
            } else {
              switch (action.tag | 0) {
                case 0 : 
                    return /* DetailsStage */Block.__(0, [
                              details,
                              action[0]
                            ]);
                case 1 : 
                    return Pervasives.failwith("invariant violation: unexpected action in DetailsStage");
                case 2 : 
                    return /* DetailsStage */Block.__(0, [
                              /* record */[
                                /* abstract_syntax */details[/* abstract_syntax */0],
                                /* concrete_syntax */action[0],
                                /* statics */details[/* statics */2],
                                /* dynamics */details[/* dynamics */3]
                              ],
                              tab
                            ]);
                case 3 : 
                    return /* DetailsStage */Block.__(0, [
                              /* record */[
                                /* abstract_syntax */details[/* abstract_syntax */0],
                                /* concrete_syntax */details[/* concrete_syntax */1],
                                /* statics */action[0],
                                /* dynamics */details[/* dynamics */3]
                              ],
                              tab
                            ]);
                case 4 : 
                    return /* DetailsStage */Block.__(0, [
                              /* record */[
                                /* abstract_syntax */details[/* abstract_syntax */0],
                                /* concrete_syntax */details[/* concrete_syntax */1],
                                /* statics */details[/* statics */2],
                                /* dynamics */action[0]
                              ],
                              tab
                            ]);
                case 5 : 
                    return /* ReplStage */Block.__(1, [action[0]]);
                
              }
            }
          }
        }), /* AbstractSyntaxStage */0);
  var dispatch = match[1];
  var state = match[0];
  var view;
  if (typeof state === "number") {
    view = React.createElement(Index$AbstractSyntaxEditor, {
          onContinue: (function (lang) {
              return Curry._1(dispatch, /* ASContinue */Block.__(1, [lang]));
            })
        });
  } else if (state.tag) {
    var match$1 = state[0];
    view = React.createElement(Index$ReplPane, {
          language: match$1[/* abstract_syntax */0],
          concrete: match$1[/* concrete_syntax */1],
          statics: match$1[/* statics */2],
          dynamics: match$1[/* dynamics */3]
        });
  } else {
    var details = state[0];
    var tab_contents;
    switch (state[1]) {
      case 0 : 
          tab_contents = React.createElement(Index$ConcreteSyntaxEditor, {
                onComplete: (function (syntax_desc) {
                    return Curry._1(dispatch, /* CompleteConcreteSyntax */Block.__(2, [syntax_desc]));
                  })
              });
          break;
      case 1 : 
          tab_contents = React.createElement(Index$StaticsEditor, {
                onComplete: (function (statics) {
                    return Curry._1(dispatch, /* CompleteStatics */Block.__(3, [statics]));
                  })
              });
          break;
      case 2 : 
          tab_contents = React.createElement(Index$DynamicsEditor, {
                onComplete: (function (dynamics) {
                    return Curry._1(dispatch, /* CompleteDynamics */Block.__(4, [dynamics]));
                  })
              });
          break;
      
    }
    var details$prime_000 = details[/* concrete_syntax */1];
    var details$prime_001 = details[/* statics */2];
    var details$prime_002 = details[/* dynamics */3];
    var match$2 = details$prime_000;
    var continue_button;
    if (match$2 !== undefined) {
      var match$3 = details$prime_001;
      if (match$3 !== undefined) {
        var match$4 = details$prime_002;
        if (match$4 !== undefined) {
          var details_000 = /* abstract_syntax */details[/* abstract_syntax */0];
          var details$1 = /* record */[
            details_000,
            /* concrete_syntax */match$2,
            /* statics */match$3,
            /* dynamics */match$4
          ];
          continue_button = React.createElement("button", {
                onClick: (function (param) {
                    return Curry._1(dispatch, /* DetailsContinue */Block.__(5, [details$1]));
                  })
              }, "Continue to REPL");
        } else {
          continue_button = null;
        }
      } else {
        continue_button = null;
      }
    } else {
      continue_button = null;
    }
    view = React.createElement("div", undefined, React.createElement("button", {
              onClick: (function (param) {
                  return Curry._1(dispatch, /* ChangeTab */Block.__(0, [/* ConcreteTab */0]));
                })
            }, "Concrete syntax"), React.createElement("button", {
              onClick: (function (param) {
                  return Curry._1(dispatch, /* ChangeTab */Block.__(0, [/* StaticsTab */1]));
                })
            }, "Statics"), React.createElement("button", {
              onClick: (function (param) {
                  return Curry._1(dispatch, /* ChangeTab */Block.__(0, [/* DynamicsTab */2]));
                })
            }, "Dynamics"), continue_button, tab_contents);
  }
  return React.createElement("div", {
              className: "lvca-viewer"
            }, React.createElement("h1", {
                  className: "header"
                }, "LVCA"), view);
}

var LvcaViewer = /* module */[
  /* mk_details */mk_details,
  /* change_tab */change_tab,
  /* make */Index$LvcaViewer
];

ReactDOMRe.renderToElementWithId(React.createElement(Index$LvcaViewer, { }), "index");

var Result = 0;

var LrTables = 0;

exports.Result = Result;
exports.LrTables = LrTables;
exports.read_eval_input = read_eval_input;
exports.shift_from_to = shift_from_to;
exports.step_forward = step_forward;
exports.step_back = step_back;
exports.go_back = go_back;
exports.go_forward = go_forward;
exports.make_div = make_div;
exports.Repl = Repl;
exports.AbstractSyntaxEditor = AbstractSyntaxEditor;
exports.ConcreteSyntaxEditor = ConcreteSyntaxEditor;
exports.StaticsEditor = StaticsEditor;
exports.DynamicsEditor = DynamicsEditor;
exports.ReplPane = ReplPane;
exports.LvcaViewer = LvcaViewer;
/* preventDefault Not a pure module */
