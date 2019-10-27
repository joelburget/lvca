// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Char = require("bs-platform/lib/js/char.js");
var Bytes = require("bs-platform/lib/js/bytes.js");
var Caml_bytes = require("bs-platform/lib/js/caml_bytes.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function index(i) {
  if (i < 0) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Bitstring.index: negative index"
        ];
  }
  return /* tuple */[
          i / 8 | 0,
          i % 8
        ];
}

function getExn(param, i) {
  if (i >= param[0]) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Bitstring.getExn: index too large"
        ];
  }
  var match = index(i);
  var c = Caml_bytes.get(param[1], match[0]);
  var mask = (1 << match[1]);
  return (c & mask) === mask;
}

function get(bs, i) {
  try {
    return getExn(bs, i);
  }
  catch (exn){
    return undefined;
  }
}

function setExn(param, i, b) {
  var bs = param[1];
  if (i >= param[0]) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Bitstring.setExn: index too large"
        ];
  }
  var match = index(i);
  var bytes_ix = match[0];
  var c = Caml_bytes.get(bs, bytes_ix);
  var mask = (1 << match[1]);
  var new_char = b ? c | mask : c & Pervasives.lnot(mask);
  bs[bytes_ix] = Char.chr(new_char);
  return /* () */0;
}

function set(bs, i, b) {
  try {
    return setExn(bs, i, b);
  }
  catch (exn){
    return undefined;
  }
}

function alloc(len, b) {
  var one = len % 8 === 0 ? 0 : 1;
  return /* Bitstring */[
          len,
          Bytes.make((len / 8 | 0) + one | 0, Char.chr(b ? 255 : 0))
        ];
}

function length(param) {
  return param[0];
}

exports.index = index;
exports.getExn = getExn;
exports.get = get;
exports.setExn = setExn;
exports.set = set;
exports.alloc = alloc;
exports.length = length;
/* No side effect */