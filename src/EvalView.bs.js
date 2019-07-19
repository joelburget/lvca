// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var Core = require("./Core.bs.js");
var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");
var $$String = require("bs-platform/lib/js/string.js");
var Binding = require("./Binding.bs.js");

function EvalView(Props) {
  var input = Props.input;
  var evalResult = Props.evalResult;
  if (evalResult.tag) {
    var match = evalResult[0];
    var match$1 = match[1];
    var msg = match[0];
    if (match$1 !== undefined) {
      var match$2 = Curry._1(Binding.DeBruijn[/* to_nominal */0], match$1);
      var ast_view = match$2 !== undefined ? React.createElement("div", undefined, Curry._1(Binding.Nominal[/* pp_term' */1], match$2)) : React.createElement("div", undefined);
      return React.createElement("div", {
                  className: "error"
                }, React.createElement("div", undefined, msg), ast_view);
    } else {
      return React.createElement("div", {
                  className: "error"
                }, msg);
    }
  } else {
    var ast = Core.val_to_ast(evalResult[0]);
    var hash;
    hash = input.tag ? input[0][0] : "#" + $$String.sub(Curry._1(Binding.Nominal[/* hash */4], input[0]), 0, 8);
    return React.createElement("div", {
                className: "eval-result-row"
              }, React.createElement("div", {
                    className: "eval-result"
                  }, Curry._1(Binding.Nominal[/* pp_term' */1], ast)), React.createElement("div", {
                    className: "eval-result-hash"
                  }, hash));
  }
}

var make = EvalView;

exports.make = make;
/* Core Not a pure module */
