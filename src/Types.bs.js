// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Util = require("./Util.bs.js");
var Sjcl = require("sjcl");
var Curry = require("bs-platform/lib/js/curry.js");
var Cbor = require("./cbor");
var Bigint = require("bs-zarith/src/Bigint.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Caml_module = require("bs-platform/lib/js/caml_module.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Belt_MapString = require("bs-platform/lib/js/belt_MapString.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");

function prim_eq(p1, p2) {
  if (p1.tag) {
    if (p2.tag) {
      return p1[0] === p2[0];
    } else {
      return false;
    }
  } else if (p2.tag) {
    return false;
  } else {
    return Bigint.$eq(p1[0], p2[0]);
  }
}

var Sjcl$1 = /* module */[];

function to_hex(buf) {
  return Curry._1((
    function to_hex(buffer) {
      return Array.prototype.map.call(
        new Uint8Array(buffer),
        x => ('00' + x.toString(16)).slice(-2)
      ).join('');
    }
  ), buf);
}

var $$ArrayBuffer = /* module */[/* to_hex */to_hex];

var $$Uint8Array = Caml_module.init_mod([
      "Types.ml",
      70,
      6
    ], [[
        0,
        0,
        0
      ]]);

var BitArray = Caml_module.init_mod([
      "Types.ml",
      102,
      6
    ], [[0]]);

function from_b_array(arr) {
  return Curry._2((
    function fromBitArrayCodec(sjcl, arr) {
        var out = [], bl = sjcl.bitArray.bitLength(arr), i, tmp;
        for (i=0; i<bl/8; i++) {
            if ((i&3) === 0) {
                tmp = arr[i/4];
            }
            out.push(tmp >>> 24);
            tmp <<= 8;
        }
        return out;
    }
  ), Sjcl, arr);
}

function from_array_buffer(buf) {
  return Curry._1((function(buf) { return new Uint8Array(buf); }), buf);
}

function to_array_buffer(buf) {
  return Curry._1((function(arr) { return arr.buffer; }), buf);
}

Caml_module.update_mod([[
        0,
        0,
        0
      ]], $$Uint8Array, /* module */[
      /* from_b_array */from_b_array,
      /* from_array_buffer */from_array_buffer,
      /* to_array_buffer */to_array_buffer
    ]);

function from_u8_array(arr) {
  return Curry._2((
    function toBitArrayCodec(sjcl, bytes) {
        var out = [], i, tmp=0;
        for (i=0; i<bytes.length; i++) {
            tmp = tmp << 8 | bytes[i];
            if ((i&3) === 3) {
                out.push(tmp);
                tmp = 0;
            }
        }
        if (i&3) {
            out.push(sjcl.bitArray.partial(8*(i&3), tmp));
        }
        return out;
    }
  ), Sjcl, arr);
}

Caml_module.update_mod([[0]], BitArray, /* module */[/* from_u8_array */from_u8_array]);

function hash_str(str) {
  return Curry._2((
    function(sjcl, str) {
      var bitArray = sjcl.hash.sha256.hash(str);
      return sjcl.codec.hex.fromBits(bitArray);
    }
  ), Sjcl, str);
}

function hash_ba(ba) {
  return Curry._2((
    function(sjcl, ba) {
      var bitArray = sjcl.hash.sha256.hash(ba);
      return sjcl.codec.hex.fromBits(bitArray);
    }
  ), Sjcl, ba);
}

var Sha256 = /* module */[
  /* hash_str */hash_str,
  /* hash_ba */hash_ba
];

var cbor = Cbor;

function encode_ab(it) {
  return Curry._2((function(cbor, it) { return cbor.encode(it); }), cbor, it);
}

function decode_ab(it) {
  var raw_decode = (function(cbor, it) { return cbor.decode(it); });
  try {
    return Caml_option.some(Curry._2(raw_decode, cbor, it));
  }
  catch (exn){
    return undefined;
  }
}

var Cbor$1 = /* module */[
  /* cbor */cbor,
  /* encode_ab */encode_ab,
  /* decode_ab */decode_ab
];

function token_name(param) {
  if (typeof param === "number") {
    return undefined;
  } else {
    return param[0];
  }
}

function fixity_str(param) {
  switch (param) {
    case 0 : 
        return "left";
    case 1 : 
        return "right";
    case 2 : 
        return "nonassoc";
    
  }
}

var DuplicateVarRules = Caml_exceptions.create("Types.ConcreteSyntaxDescription.DuplicateVarRules");

function partition_nonterminal_matches(matches) {
  return Util.fold_right((function (param) {
                var match = param[1];
                var v_rule = match[1];
                var matches = match[0];
                var match_ = param[0];
                var exit = 0;
                if (match_) {
                  var match$1 = match_[0][0];
                  var match$2 = match$1[/* term_pattern */1];
                  if (match$2.tag || match$2[0] !== "var") {
                    exit = 1;
                  } else {
                    var match$3 = match$2[1];
                    if (match$3) {
                      var match$4 = match$3[0];
                      if (match$4[0] || match$3[1] || match_[1]) {
                        exit = 1;
                      } else {
                        if (v_rule !== undefined) {
                          throw DuplicateVarRules;
                        }
                        return /* tuple */[
                                matches,
                                /* record */[
                                  /* tokens */match$1[/* tokens */0],
                                  /* var_capture */match$4[1]
                                ]
                              ];
                      }
                    } else {
                      exit = 1;
                    }
                  }
                } else {
                  exit = 1;
                }
                if (exit === 1) {
                  return /* tuple */[
                          /* :: */[
                            match_,
                            matches
                          ],
                          v_rule
                        ];
                }
                
              }), matches, /* tuple */[
              /* [] */0,
              undefined
            ]);
}

function make(terminal_rules, sort_rules) {
  return /* record */[
          /* terminal_rules */Belt_MapString.fromArray(Belt_List.toArray(List.map((function (param) {
                          return /* tuple */[
                                  param[0],
                                  param[1]
                                ];
                        }), terminal_rules))),
          /* sort_rules */Belt_MapString.fromArray(Belt_List.toArray(List.map((function (rule) {
                          return /* tuple */[
                                  rule[0][/* sort_name */0],
                                  rule
                                ];
                        }), sort_rules)))
        ];
}

var ConcreteSyntaxDescription = /* module */[
  /* M */0,
  /* token_name */token_name,
  /* fixity_str */fixity_str,
  /* DuplicateVarRules */DuplicateVarRules,
  /* partition_nonterminal_matches */partition_nonterminal_matches,
  /* make */make
];

exports.prim_eq = prim_eq;
exports.Sjcl = Sjcl$1;
exports.$$ArrayBuffer = $$ArrayBuffer;
exports.$$Uint8Array = $$Uint8Array;
exports.BitArray = BitArray;
exports.Sha256 = Sha256;
exports.Cbor = Cbor$1;
exports.ConcreteSyntaxDescription = ConcreteSyntaxDescription;
/* Uint8Array Not a pure module */
