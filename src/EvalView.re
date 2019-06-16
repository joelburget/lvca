let show_prim(prim:Types.primitive) = switch (prim) {
  | PrimInteger(i)  => React.string(Bigint.to_string(i))
  | PrimString(str) => React.string("\"" ++ str ++ "\"")
  | PrimBool(true)  => React.string("true")
  | PrimBool(false) => React.string("false")
};

let make_span = children => {
  ReactDOMRe.createDOMElementVariadic(
    "span",
    ~props=ReactDOMRe.domProps(),
    Array.concat(children)
  )
};

let make_div = children => {
  ReactDOMRe.createDOMElementVariadic(
    "div",
    ~props=ReactDOMRe.domProps(),
    children
  )
};

module CoreValView = {
  open Util;
  open Types.Core;

  let rec view_core_pat = pat => switch (pat) {
  | PatternTerm(name, pats) =>
    <span>
      {React.string(name ++ "(") /* XXX children */}
      {React.string(")")}
    </span>
  | PatternVar(None) => React.string("_")
  | PatternVar(Some(name)) => React.string(name)
  | PatternLit(lit)     => show_prim(lit)
  | PatternDefault      => React.string("default")
  }

  and view_core = core => switch (core) {
  | CoreVar(name) => React.string(name)
  | CoreVal(core_val) => view_core_val(core_val)
  | CoreApp(f, args) =>
    make_span([
      [| React.string("app(") |],
      [| view_core(f) |],
      [| React.string("; ") |],
      Array.of_list(intersperse(List.map(view_core, args), React.string("; "))),
      [| React.string(")") |],
    ])
  | Lam(args, body) => make_span([
    [| React.string("lam(") |],
    Array.of_list(intersperse(List.map(React.string, args), React.string(". "))),
    [| view_core(body) |],
    [| React.string(")") |],
  ])
  | Case(arg, _ty, _cases) => make_span([
    [| React.string("case(") |],
    [| view_core(arg) |],
    [| React.string("; ...)") |],
  ])
  | Meaning(name) => make_span([
    [| React.string("[[") |],
    [| React.string(name) |],
    [| React.string("]]") |],
  ])
  }

  and view_core_val = coreVal => switch (coreVal) {
  | ValTm(name, children) => make_span([
    [| React.string(name) |],
    Array.of_list(intersperse(List.map(view_core_val, children), React.string("; "))),
  ])
  | ValLit(lit) => show_prim(lit)
  | ValPrimop(name) => React.string(name) /* TODO: take a look at this */
  | ValLam(vars, core) => make_span([
    Array.of_list(intersperse(List.map(React.string, vars), React.string(". "))),
    [| view_core(core) |],
  ])
  };

  [@react.component]
  let make = (~coreVal:core_val) => {
    <div> {view_core_val(coreVal)} </div>
  };
}


module TermViewer = {
  open Util;
  open Types.Abt;

  let rec show_term(term:term) = switch (term) {
      | Term(name, lst) =>
        make_span([
          [| React.string(name) |],
          [| React.string("(") |],
          Array.of_list(intersperse(List.map(show_scope, lst), React.string(";"))),
          [| React.string(")") |]
        ])
      | Var(name)     => React.string(string_of_int(name)) // TODO: convert to ast
      | Sequence(tms) => make_span([
          [| React.string("[") |],
          Array.of_list(List.map(show_term, tms)),
          [| React.string("]") |]
        ])
      | Primitive(prim) => show_prim(prim)
      }

  and show_scope(scope:scope) = switch (scope) {
    | Scope(names, tm) => make_span([
      Array.of_list(intersperse_after(List.map(React.string, names), React.string("."))),
      [| show_term(tm) |]
    ])
  };

  [@react.component]
  let make = (~term:term) => {
    <div>{show_term(term)}</div>
  };
};

// TODO: duplicated in index
type item_result = Types.Core.translation_result(Types.Core.core_val);

[@react.component]
let make = (~evalResult: item_result) => {
  switch (evalResult) {
  | Ok(coreVal)
    => <CoreValView coreVal=coreVal />
  | Error((msg, None))
    => <div className="error"> {React.string(msg)} </div>
  | Error((msg, Some(tm)))
    => <div className="error">
         {React.string(msg)}
         <TermViewer term=tm />
       </div>
  };
}
