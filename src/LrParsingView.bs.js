// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var React = require("react");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Belt_MapString = require("bs-platform/lib/js/belt_MapString.js");
var Caml_splice_call = require("bs-platform/lib/js/caml_splice_call.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function LrParsingView$Grammar(Props) {
  Props.grammar;
  return React.createElement("div", undefined, React.createElement("h2", undefined, "terminals"), React.createElement("h2", undefined, "states"));
}

var Grammar = /* module */[/* make */LrParsingView$Grammar];

function LrParsingView$Tables(Props) {
  var grammar = Props.grammar;
  var action_table = Props.action_table;
  var goto_table = Props.goto_table;
  if (action_table.length !== goto_table.length) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "LrParsingView.re",
            31,
            4
          ]
        ];
  }
  var lookup_terminal_name = function (i) {
    var match = Belt_MapString.findFirstBy(grammar[/* terminal_names */2], (function (param, num) {
            return num === i;
          }));
    if (match !== undefined) {
      return match[0];
    } else {
      return "T" + String(i);
    }
  };
  var shared_table = Belt_Array.zip(action_table, goto_table);
  var action_span = Caml_array.caml_array_get(action_table, 0).length;
  var goto_span = Caml_array.caml_array_get(goto_table, 0).length;
  var action_headers = Belt_Array.mapWithIndex(Caml_array.caml_array_get(action_table, 0), (function (i, param) {
          return React.createElement("th", undefined, lookup_terminal_name(i));
        }));
  var goto_headers = Belt_Array.map(Belt_Array.map(Caml_array.caml_array_get(goto_table, 0), (function (param) {
              var symbol = param[0];
              if (symbol.tag) {
                var i = symbol[0];
                var match = Belt_MapString.findFirstBy(grammar[/* nonterminal_names */3], (function (param, num) {
                        return num === i;
                      }));
                if (match !== undefined) {
                  return match[0];
                } else {
                  return "NT" + String(i);
                }
              } else {
                return lookup_terminal_name(symbol[0]);
              }
            })), (function (x) {
          return React.createElement("th", undefined, x);
        }));
  var headers_row = Caml_splice_call.spliceApply(React.createElement, [
        "tr",
        { },
        Belt_Array.concat(action_headers, goto_headers)
      ]);
  var data = Belt_Array.map(shared_table, (function (param) {
          var action_row_elems = Belt_Array.map(Belt_Array.map(param[0], (function (action) {
                      if (typeof action === "number") {
                        if (action === 0) {
                          return "acc";
                        } else {
                          return "err";
                        }
                      } else if (action.tag) {
                        return "r" + String(action[0]);
                      } else {
                        return "s" + String(action[0]);
                      }
                    })), (function (x) {
                  return React.createElement("td", undefined, x);
                }));
          var goto_row_elems = Belt_Array.map(Belt_Array.map(param[1], (function (param) {
                      var m_state = param[1];
                      if (m_state !== undefined) {
                        return String(m_state);
                      } else {
                        return "";
                      }
                    })), (function (x) {
                  return React.createElement("td", undefined, x);
                }));
          return Caml_splice_call.spliceApply(React.createElement, [
                      "tr",
                      { },
                      Belt_Array.concat(/* array */[React.createElement("td", undefined)], Belt_Array.concat(action_row_elems, goto_row_elems))
                    ]);
        }));
  var tbody = Caml_splice_call.spliceApply(React.createElement, [
        "tbody",
        { },
        data
      ]);
  return React.createElement("table", undefined, React.createElement("thead", undefined, React.createElement("tr", undefined, React.createElement("th", {
                          rowSpan: 2
                        }, "state"), React.createElement("th", {
                          colSpan: action_span
                        }, "action"), React.createElement("th", {
                          colSpan: goto_span
                        }, "goto")), headers_row), tbody);
}

var Tables = /* module */[/* make */LrParsingView$Tables];

var BA = 0;

var MS = 0;

exports.BA = BA;
exports.MS = MS;
exports.Grammar = Grammar;
exports.Tables = Tables;
/* react Not a pure module */
