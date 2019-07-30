// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Belt_Result = require("bs-platform/lib/js/belt_Result.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Belt_MapString = require("bs-platform/lib/js/belt_MapString.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("bs-platform/lib/js/caml_js_exceptions.js");

function unsnoc(lst) {
  if (lst) {
    var lst$prime = lst[1];
    var x = lst[0];
    if (lst$prime) {
      var match = unsnoc(lst$prime);
      return /* tuple */[
              /* :: */[
                x,
                match[0]
              ],
              match[1]
            ];
    } else {
      return /* tuple */[
              /* [] */0,
              x
            ];
    }
  } else {
    return Pervasives.failwith("unsnoc empty list");
  }
}

function intersperse(list, el) {
  if (list) {
    var match = list[1];
    if (match) {
      return /* :: */[
              list[0],
              /* :: */[
                el,
                intersperse(/* :: */[
                      match[0],
                      match[1]
                    ], el)
              ]
            ];
    } else {
      return list;
    }
  } else {
    return list;
  }
}

function intersperse_after(list, el) {
  if (list) {
    var match = list[1];
    var list_el = list[0];
    if (match) {
      return /* :: */[
              list_el,
              /* :: */[
                el,
                intersperse_after(/* :: */[
                      match[0],
                      match[1]
                    ], el)
              ]
            ];
    } else {
      return /* :: */[
              list_el,
              /* :: */[
                el,
                /* [] */0
              ]
            ];
    }
  } else {
    return /* [] */0;
  }
}

function get_first(f, _lst) {
  while(true) {
    var lst = _lst;
    if (lst) {
      var some_b = Curry._1(f, lst[0]);
      if (some_b !== undefined) {
        return some_b;
      } else {
        _lst = lst[1];
        continue ;
      }
    } else {
      return undefined;
    }
  };
}

function traverse_list_result(f, lst) {
  if (lst) {
    var match = Curry._1(f, lst[0]);
    if (match.tag) {
      return /* Error */Block.__(1, [match[0]]);
    } else {
      var b = match[0];
      return Belt_Result.flatMap(traverse_list_result(f, lst[1]), (function (rest$prime) {
                    return /* Ok */Block.__(0, [/* :: */[
                                b,
                                rest$prime
                              ]]);
                  }));
    }
  } else {
    return /* Ok */Block.__(0, [/* [] */0]);
  }
}

function sequence_list_result(lst) {
  if (lst) {
    var match = lst[0];
    if (match.tag) {
      return /* Error */Block.__(1, [match[0]]);
    } else {
      var a = match[0];
      return Belt_Result.map(sequence_list_result(lst[1]), (function (rest$prime) {
                    return /* :: */[
                            a,
                            rest$prime
                          ];
                  }));
    }
  } else {
    return /* Ok */Block.__(0, [/* [] */0]);
  }
}

function ArrayApplicative(A) {
  var Traversal_exn = Caml_exceptions.create("Util.ArrayApplicative(A).Traversal_exn");
  var sequence_array_result = function (arr) {
    try {
      return /* Ok */Block.__(0, [Belt_Array.map(arr, (function (param) {
                        if (param.tag) {
                          throw [
                                Traversal_exn,
                                param[0]
                              ];
                        } else {
                          return param[0];
                        }
                      }))]);
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn[0] === Traversal_exn) {
        return /* Error */Block.__(1, [exn[1]]);
      } else {
        throw exn;
      }
    }
  };
  var traverse_array_result = function (f, arr) {
    try {
      return /* Ok */Block.__(0, [Belt_Array.map(arr, (function (a) {
                        var match = Curry._1(f, a);
                        if (match.tag) {
                          throw [
                                Traversal_exn,
                                match[0]
                              ];
                        } else {
                          return match[0];
                        }
                      }))]);
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn[0] === Traversal_exn) {
        return /* Error */Block.__(1, [exn[1]]);
      } else {
        throw exn;
      }
    }
  };
  return /* module */[
          /* Traversal_exn */Traversal_exn,
          /* sequence_array_result */sequence_array_result,
          /* traverse_array_result */traverse_array_result
        ];
}

function sequence_list_option(lst) {
  if (lst) {
    var match = lst[0];
    if (match !== undefined) {
      var a = Caml_option.valFromOption(match);
      return Belt_Option.map(sequence_list_option(lst[1]), (function (rest$prime) {
                    return /* :: */[
                            a,
                            rest$prime
                          ];
                  }));
    } else {
      return undefined;
    }
  } else {
    return /* [] */0;
  }
}

function keep_some(_lst) {
  while(true) {
    var lst = _lst;
    if (lst) {
      var match = lst[0];
      if (match !== undefined) {
        return /* :: */[
                Caml_option.valFromOption(match),
                keep_some(lst[1])
              ];
      } else {
        _lst = lst[1];
        continue ;
      }
    } else {
      return /* [] */0;
    }
  };
}

function union(m1, m2) {
  return Belt_MapString.merge(m1, m2, (function (_k, v1, v2) {
                if (v2 !== undefined) {
                  return Caml_option.some(Caml_option.valFromOption(v2));
                } else if (v1 !== undefined) {
                  return Caml_option.some(Caml_option.valFromOption(v1));
                } else {
                  return undefined;
                }
              }));
}

function fold_right(f, lst, b) {
  if (lst) {
    return Curry._1(f, /* tuple */[
                lst[0],
                fold_right(f, lst[1], b)
              ]);
  } else {
    return b;
  }
}

function map_error(result, f) {
  if (result.tag) {
    return /* Error */Block.__(1, [Curry._1(f, result[0])]);
  } else {
    return /* Ok */Block.__(0, [result[0]]);
  }
}

function sum(param) {
  if (param) {
    return param[0] + sum(param[1]) | 0;
  } else {
    return 0;
  }
}

function find(f, _lst) {
  while(true) {
    var lst = _lst;
    if (lst) {
      var x = lst[0];
      if (Curry._1(f, x)) {
        return Caml_option.some(x);
      } else {
        _lst = lst[1];
        continue ;
      }
    } else {
      return undefined;
    }
  };
}

function flip(f, b, a) {
  return Curry._2(f, a, b);
}

function id(a) {
  return a;
}

function list_flat_map(f, lst) {
  return Belt_List.flatten(Belt_List.map(lst, f));
}

function is_none(param) {
  return param === undefined;
}

function is_some(param) {
  return param !== undefined;
}

exports.unsnoc = unsnoc;
exports.intersperse = intersperse;
exports.intersperse_after = intersperse_after;
exports.get_first = get_first;
exports.traverse_list_result = traverse_list_result;
exports.sequence_list_result = sequence_list_result;
exports.ArrayApplicative = ArrayApplicative;
exports.sequence_list_option = sequence_list_option;
exports.keep_some = keep_some;
exports.union = union;
exports.fold_right = fold_right;
exports.map_error = map_error;
exports.sum = sum;
exports.find = find;
exports.flip = flip;
exports.id = id;
exports.list_flat_map = list_flat_map;
exports.is_none = is_none;
exports.is_some = is_some;
/* No side effect */
