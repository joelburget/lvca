// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Util = require("./Util.bs.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Types = require("./Types.bs.js");
var Bigint = require("bs-zarith/src/Bigint.js");
var Format = require("bs-platform/lib/js/format.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Caml_module = require("bs-platform/lib/js/caml_module.js");
var Belt_MapString = require("bs-platform/lib/js/belt_MapString.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("bs-platform/lib/js/caml_js_exceptions.js");

var DeBruijn = Caml_module.init_mod([
      "Binding.ml",
      30,
      6
    ], [[
        0,
        0,
        0
      ]]);

var Nominal = Caml_module.init_mod([
      "Binding.ml",
      120,
      6
    ], [[
        0,
        0,
        0,
        0,
        0
      ]]);

function to_nominal$prime(ctx, param) {
  switch (param.tag | 0) {
    case 0 : 
        var tag = param[0];
        return Belt_Option.map(Util.sequence_list_option(Belt_List.map(param[1], (function (param) {
                              var ctx$1 = ctx;
                              var param$1 = param;
                              var binders = param$1[0];
                              return Belt_Option.map(to_nominal$prime(Belt_List.concat(binders, ctx$1), param$1[1]), (function (body$prime) {
                                            return /* Scope */[
                                                    binders,
                                                    body$prime
                                                  ];
                                          }));
                            }))), (function (subtms$prime) {
                      return /* Operator */Block.__(0, [
                                tag,
                                subtms$prime
                              ]);
                    }));
    case 1 : 
        return Belt_Option.map(Belt_List.get(ctx, param[0]), (function (name) {
                      return /* Var */Block.__(1, [name]);
                    }));
    case 2 : 
        return Belt_Option.map(Util.sequence_list_option(Belt_List.map(param[0], (function (param) {
                              return to_nominal$prime(ctx, param);
                            }))), (function (tms$prime) {
                      return /* Sequence */Block.__(2, [tms$prime]);
                    }));
    case 3 : 
        return /* Primitive */Block.__(3, [param[0]]);
    
  }
}

function to_nominal(param) {
  return to_nominal$prime(/* [] */0, param);
}

var FailedFromNominal = Caml_exceptions.create("Binding.DeBruijn.FailedFromNominal");

function from_nominal_with_bindings$prime(env, param) {
  switch (param.tag | 0) {
    case 0 : 
        return /* Operator */Block.__(0, [
                  param[0],
                  Belt_List.map(param[1], (function (param) {
                          var env$1 = env;
                          var param$1 = param;
                          var names = param$1[0];
                          var n = Belt_List.length(names);
                          var argNums = Belt_List.zip(names, Belt_List.makeBy(n, (function (i) {
                                      return i;
                                    })));
                          var env$prime = Util.union(Belt_MapString.map(env$1, (function (i) {
                                      return i + n | 0;
                                    })), Belt_MapString.fromArray(Belt_List.toArray(argNums)));
                          return /* Scope */[
                                  names,
                                  from_nominal_with_bindings$prime(env$prime, param$1[1])
                                ];
                        }))
                ]);
    case 1 : 
        var name = param[0];
        var match = Belt_MapString.get(env, name);
        if (match !== undefined) {
          return /* Var */Block.__(1, [match]);
        } else {
          throw [
                FailedFromNominal,
                "couldn't find variable " + name
              ];
        }
    case 2 : 
        return /* Sequence */Block.__(2, [Belt_List.map(param[0], (function (param) {
                          return from_nominal_with_bindings$prime(env, param);
                        }))]);
    case 3 : 
        return /* Primitive */Block.__(3, [param[0]]);
    
  }
}

function from_nominal_with_bindings(bindings, tm) {
  try {
    return /* Ok */Block.__(0, [from_nominal_with_bindings$prime(bindings, tm)]);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === FailedFromNominal) {
      return /* Error */Block.__(1, [exn[1]]);
    } else {
      throw exn;
    }
  }
}

function from_nominal(param) {
  return from_nominal_with_bindings(Belt_MapString.empty, param);
}

Caml_module.update_mod([[
        0,
        0,
        0
      ]], DeBruijn, /* module */[
      /* to_nominal */to_nominal,
      /* from_nominal */from_nominal,
      /* from_nominal_with_bindings */from_nominal_with_bindings
    ]);

function pp_term(ppf, param) {
  switch (param.tag | 0) {
    case 0 : 
        return Curry._3(Format.fprintf(ppf, /* Format */[
                        /* Formatting_gen */Block.__(18, [
                            /* Open_box */Block.__(1, [/* Format */[
                                  /* End_of_format */0,
                                  ""
                                ]]),
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* Char_literal */Block.__(12, [
                                    /* "(" */40,
                                    /* Alpha */Block.__(15, [/* Char_literal */Block.__(12, [
                                            /* ")" */41,
                                            /* Formatting_lit */Block.__(17, [
                                                /* Close_box */0,
                                                /* End_of_format */0
                                              ])
                                          ])])
                                  ])
                              ])
                          ]),
                        "@[%s(%a)@]"
                      ]), param[0], pp_scope_list, param[1]);
    case 1 : 
        return Curry._1(Format.fprintf(ppf, /* Format */[
                        /* String */Block.__(2, [
                            /* No_padding */0,
                            /* End_of_format */0
                          ]),
                        "%s"
                      ]), param[0]);
    case 2 : 
        return Curry._2(Format.fprintf(ppf, /* Format */[
                        /* Formatting_gen */Block.__(18, [
                            /* Open_box */Block.__(1, [/* Format */[
                                  /* End_of_format */0,
                                  ""
                                ]]),
                            /* Char_literal */Block.__(12, [
                                /* "[" */91,
                                /* Alpha */Block.__(15, [/* Char_literal */Block.__(12, [
                                        /* "]" */93,
                                        /* Formatting_lit */Block.__(17, [
                                            /* Close_box */0,
                                            /* End_of_format */0
                                          ])
                                      ])])
                              ])
                          ]),
                        "@[[%a]@]"
                      ]), pp_inner_list, param[0]);
    case 3 : 
        var ppf$1 = ppf;
        var param$1 = param[0];
        if (param$1.tag) {
          return Curry._1(Format.fprintf(ppf$1, /* Format */[
                          /* Char_literal */Block.__(12, [
                              /* "\"" */34,
                              /* String */Block.__(2, [
                                  /* No_padding */0,
                                  /* Char_literal */Block.__(12, [
                                      /* "\"" */34,
                                      /* End_of_format */0
                                    ])
                                ])
                            ]),
                          "\"%s\""
                        ]), param$1[0]);
        } else {
          return Curry._1(Format.fprintf(ppf$1, /* Format */[
                          /* String */Block.__(2, [
                              /* No_padding */0,
                              /* End_of_format */0
                            ]),
                          "%s"
                        ]), Bigint.to_string(param$1[0]));
        }
    
  }
}

function pp_inner_list(ppf, param) {
  if (param) {
    var xs = param[1];
    var x = param[0];
    if (xs) {
      return Curry._4(Format.fprintf(ppf, /* Format */[
                      /* Alpha */Block.__(15, [/* String_literal */Block.__(11, [
                              ", ",
                              /* Alpha */Block.__(15, [/* End_of_format */0])
                            ])]),
                      "%a, %a"
                    ]), pp_term, x, pp_inner_list, xs);
    } else {
      return Curry._2(Format.fprintf(ppf, /* Format */[
                      /* Alpha */Block.__(15, [/* End_of_format */0]),
                      "%a"
                    ]), pp_term, x);
    }
  } else {
    return /* () */0;
  }
}

function pp_scope_list(ppf, param) {
  if (param) {
    var xs = param[1];
    var x = param[0];
    if (xs) {
      return Curry._4(Format.fprintf(ppf, /* Format */[
                      /* Alpha */Block.__(15, [/* String_literal */Block.__(11, [
                              ", ",
                              /* Alpha */Block.__(15, [/* End_of_format */0])
                            ])]),
                      "%a, %a"
                    ]), pp_scope, x, pp_scope_list, xs);
    } else {
      return Curry._2(Format.fprintf(ppf, /* Format */[
                      /* Alpha */Block.__(15, [/* End_of_format */0]),
                      "%a"
                    ]), pp_scope, x);
    }
  } else {
    return /* () */0;
  }
}

function pp_scope(ppf, param) {
  var body = param[1];
  var bindings = param[0];
  if (bindings) {
    return Curry._4(Format.fprintf(ppf, /* Format */[
                    /* Alpha */Block.__(15, [/* Char_literal */Block.__(12, [
                            /* " " */32,
                            /* Alpha */Block.__(15, [/* End_of_format */0])
                          ])]),
                    "%a %a"
                  ]), pp_bindings, bindings, pp_term, body);
  } else {
    return pp_term(ppf, body);
  }
}

function pp_bindings(ppf, param) {
  if (param) {
    var xs = param[1];
    var x = param[0];
    if (xs) {
      return Curry._3(Format.fprintf(ppf, /* Format */[
                      /* String */Block.__(2, [
                          /* No_padding */0,
                          /* String_literal */Block.__(11, [
                              ". ",
                              /* Alpha */Block.__(15, [/* End_of_format */0])
                            ])
                        ]),
                      "%s. %a"
                    ]), x, pp_bindings, xs);
    } else {
      return Curry._1(Format.fprintf(ppf, /* Format */[
                      /* String */Block.__(2, [
                          /* No_padding */0,
                          /* Char_literal */Block.__(12, [
                              /* "." */46,
                              /* End_of_format */0
                            ])
                        ]),
                      "%s."
                    ]), x);
    }
  } else {
    return /* () */0;
  }
}

var pp_term$prime = Curry._1(Format.asprintf(/* Format */[
          /* Alpha */Block.__(15, [/* End_of_format */0]),
          "%a"
        ]), pp_term);

function jsonify_prim(param) {
  if (param.tag) {
    return /* array */[
            "s",
            param[0]
          ];
  } else {
    return /* array */[
            "i",
            Bigint.to_string(param[0])
          ];
  }
}

function jsonify(tm) {
  switch (tm.tag | 0) {
    case 0 : 
        return /* array */[
                "t",
                tm[0],
                Belt_List.toArray(Belt_List.map(tm[1], jsonify_scope))
              ];
    case 1 : 
        return /* array */[
                "v",
                tm[0]
              ];
    case 2 : 
        return /* array */[
                "s",
                Belt_List.toArray(Belt_List.map(tm[0], jsonify))
              ];
    case 3 : 
        return /* array */[
                "p",
                jsonify_prim(tm[0])
              ];
    
  }
}

function jsonify_scope(param) {
  return /* array */[
          Belt_List.toArray(Belt_List.map(param[0], (function (prim) {
                      return prim;
                    }))),
          jsonify(param[1])
        ];
}

function serialize(tm) {
  return Curry._1(Types.$$Uint8Array[/* from_array_buffer */1], Types.Cbor[/* encode_ab */1](jsonify(tm)));
}

function hash(tm) {
  return Types.Sha256[/* hash_ba */1](Curry._1(Types.BitArray[/* from_u8_array */0], serialize(tm)));
}

Caml_module.update_mod([[
        0,
        0,
        0,
        0,
        0
      ]], Nominal, /* module */[
      /* pp_term */pp_term,
      /* pp_term' */pp_term$prime,
      /* jsonify */jsonify,
      /* serialize */serialize,
      /* hash */hash
    ]);

var Result = 0;

var $$Option = 0;

var L = 0;

var M = 0;

var sequence_list_option = Util.sequence_list_option;

var sequence_list_result = Util.sequence_list_result;

var union = Util.union;

exports.Result = Result;
exports.$$Option = $$Option;
exports.L = L;
exports.M = M;
exports.sequence_list_option = sequence_list_option;
exports.sequence_list_result = sequence_list_result;
exports.union = union;
exports.DeBruijn = DeBruijn;
exports.Nominal = Nominal;
/* DeBruijn Not a pure module */
