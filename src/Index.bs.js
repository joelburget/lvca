// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
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
var CodeMirror = require("./CodeMirror.bs.js");
var ReactDOMRe = require("reason-react/src/ReactDOMRe.js");
var Belt_Result = require("bs-platform/lib/js/belt_Result.js");
var ParseStatus = require("./ParseStatus.bs.js");
var LanguageSimple = require("./LanguageSimple.bs.js");
var ReactCodemirror2 = require("react-codemirror2");

var Parse_term = Parsing.Incremental(Parsing.Parseable_term);

function read_eval_input(language, dynamics, input) {
  var match = Curry._1(Parse_term[/* parse */5], input);
  var match$1;
  if (match.tag) {
    var msg = match[0];
    match$1 = /* tuple */[
      /* Error */Block.__(1, [/* tuple */[
            msg,
            undefined
          ]]),
      /* Error */Block.__(1, [msg])
    ];
  } else {
    var ast = match[0];
    match$1 = /* tuple */[
      /* Ok */Block.__(0, [ast]),
      Curry._3(Binding.DeBruijn[/* from_nominal */1], language, "tm", ast)
    ];
  }
  var abtResult = match$1[1];
  var astResult = match$1[0];
  var eval$prime = function (tm) {
    return Util.map_error(Core.$$eval(tm), (function (msg) {
                  return /* tuple */[
                          msg,
                          undefined
                        ];
                }));
  };
  if (abtResult.tag) {
    var msg$1 = abtResult[0];
    return /* tuple */[
            /* Error */Block.__(1, [/* tuple */[
                  msg$1,
                  undefined
                ]]),
            /* Error */Block.__(1, [/* tuple */[
                  msg$1,
                  undefined
                ]])
          ];
  } else {
    var match$2 = Belt_Result.flatMap(Core.term_to_core(dynamics, abtResult[0]), eval$prime);
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

function step_forward(language, dynamics, param) {
  var input = param[/* input */2];
  var match = read_eval_input(language, dynamics, input);
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

function step_back(language, dynamics, param) {
  var input = param[/* input */2];
  var match = read_eval_input(language, dynamics, input);
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

function go_back(lang, dyn, hist, i) {
  if (i !== 0) {
    return step_back(lang, dyn, go_back(lang, dyn, hist, i - 1 | 0));
  } else {
    return hist;
  }
}

function go_forward(lang, dyn, hist, i) {
  if (i !== 0) {
    return step_forward(lang, dyn, go_forward(lang, dyn, hist, i - 1 | 0));
  } else {
    return hist;
  }
}

function make_div(children) {
  return Block.spliceApply(React.createElement, [
              "div",
              { },
              children
            ]);
}

var preventDefault = (evt => evt.preventDefault());

function Index$Repl(Props) {
  var history = Props.history;
  var language = Props.language;
  Props.statics;
  var dynamics = Props.dynamics;
  var setInput = Props.setInput;
  var handleEnter = Props.handleEnter;
  var handleUp = Props.handleUp;
  var handleDown = Props.handleDown;
  var input = history[/* input */2];
  var options = {
    mode: "lvca"
  };
  var match = read_eval_input(language, dynamics, input);
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

function Index$LvcaViewer(Props) {
  var match = React.useState((function () {
          return /* record */[
                  /* before : [] */0,
                  /* after : [] */0,
                  /* input */"ite(val(false()); val(false()); val(true()))"
                ];
        }));
  var setHistory = match[1];
  var match$1 = React.useState((function () {
          return LanguageSimple.abstractSyntax;
        }));
  var setAsInput = match$1[1];
  var asInput = match$1[0];
  var match$2 = React.useState((function () {
          return LanguageSimple.statics;
        }));
  var setStaticsInput = match$2[1];
  var staticsInput = match$2[0];
  var match$3 = React.useState((function () {
          return LanguageSimple.dynamics;
        }));
  var setDynamicsInput = match$3[1];
  var dynamicsInput = match$3[0];
  var Parseable_language$prime = ParseStatus.Make(Parsing.Parseable_language);
  var match$4 = Curry._1(Parseable_language$prime[/* parse */1], asInput);
  var language = match$4[1];
  var Parseable_statics$prime = ParseStatus.Make(Parsing.Parseable_statics);
  var match$5 = Curry._1(Parseable_statics$prime[/* parse */1], staticsInput);
  var statics = match$5[1];
  var Parseable_dynamics$prime = ParseStatus.Make(Parsing.Parseable_dynamics);
  var match$6 = Curry._1(Parseable_dynamics$prime[/* parse */1], dynamicsInput);
  var dynamics = match$6[1];
  var replPane;
  var exit = 0;
  if (language.tag) {
    exit = 1;
  } else {
    var language$1 = language[0];
    if (statics.tag || dynamics.tag) {
      exit = 1;
    } else {
      var dynamics$1 = dynamics[0];
      replPane = React.createElement("div", {
            className: "repl-pane"
          }, React.createElement(Index$Repl, {
                history: match[0],
                language: language$1,
                statics: statics[0],
                dynamics: dynamics$1,
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
                                    return step_forward(language$1, dynamics$1, hist);
                                  } else {
                                    var match = read_eval_input(language$1, dynamics$1, input);
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
                                  return go_back(language$1, dynamics$1, hist, n);
                                }));
                  }),
                handleDown: (function (n) {
                    return Curry._1(setHistory, (function (hist) {
                                  return go_forward(language$1, dynamics$1, hist, n);
                                }));
                  })
              }));
    }
  }
  if (exit === 1) {
    replPane = React.createElement("div", {
          className: "repl-pane disabled"
        });
  }
  return React.createElement("div", {
              className: "lvca-viewer"
            }, React.createElement("h1", {
                  className: "header"
                }, "LVCA"), React.createElement("h2", {
                  className: "header2 header2-abstract-syntax"
                }, "Abstract Syntax ", match$4[0]), React.createElement("div", {
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
                    })), React.createElement("h2", {
                  className: "header2 header2-statics"
                }, "Statics ", match$5[0]), React.createElement("div", {
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
                    })), React.createElement("h2", {
                  className: "header2 header2-dynamics"
                }, "Dynamics ", match$6[0]), React.createElement("div", {
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
                    })), React.createElement("h2", {
                  className: "header2 header2-repl"
                }, "repl"), replPane);
}

var LvcaViewer = /* module */[/* make */Index$LvcaViewer];

ReactDOMRe.renderToElementWithId(React.createElement(Index$LvcaViewer, { }), "index");

var Result = 0;

exports.Result = Result;
exports.Parse_term = Parse_term;
exports.read_eval_input = read_eval_input;
exports.shift_from_to = shift_from_to;
exports.step_forward = step_forward;
exports.step_back = step_back;
exports.go_back = go_back;
exports.go_forward = go_forward;
exports.make_div = make_div;
exports.Repl = Repl;
exports.LvcaViewer = LvcaViewer;
/* Parse_term Not a pure module */
