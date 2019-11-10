exception Bad(string)

let str_of_tm = tm => Binding.Nominal.pp_term'(NonBinding.to_nominal(tm));

let render_inlineAtom = (tm: NonBinding.term): ReasonReact.reactElement => switch (tm) {
  | Operator("inlineAtom", [attrs, Primitive(PrimString(str))])
  => {
    // let attrs' = TODO;
    ReactDOMRe.createDOMElementVariadic(
      "span",
      ~props=ReactDOMRe.domProps(/*~foo=bar,*/ ()),
      [|React.string(str)|]
    )
  }
  | tm => raise(Bad("render_inlineAtom: unexpected term: " ++ str_of_tm(tm)))
};

let render_paragraph = (tm: NonBinding.term): ReasonReact.reactElement => switch (tm) {
  | Operator("inline", [Sequence(inlineAtoms)]) => {
    let inlineAtoms' = inlineAtoms
      |. Belt.List.toArray
      |. Belt.Array.map(render_inlineAtom);

    ReactDOMRe.createElement(ReasonReact.fragment, inlineAtoms');
  }
  | tm => raise(Bad("render_paragraph: unexpected term: " ++ str_of_tm(tm)))
};

let render_block = (tm: NonBinding.term): ReasonReact.reactElement => switch (tm) {
  | Operator("header", [ level, Primitive(PrimString(str)) ]) => switch (level) {
    | Operator("h1", []) => <h1>{React.string(str)}</h1>
    | Operator("h2", []) => <h2>{React.string(str)}</h2>
    | Operator("h3", []) => <h3>{React.string(str)}</h3>
    | tm => raise(Bad("render_block header: unexpected term: " ++ str_of_tm(tm)))
  }
  | Operator("paragraph", [para]) => render_paragraph(para)
  | tm => raise(Bad("render_block paragraph: unexpected term: " ++ str_of_tm(tm)))
};

let render_doc = (tm: NonBinding.term): ReasonReact.reactElement => switch(tm) {
  | Operator("document", [Sequence(blocks)]) => {
    let blocks' = blocks
      |. Belt.List.toArray
      |. Belt.Array.map(render_block);
    ReactUtil.make_div(blocks')
  }
  | tm => raise(Bad("render_doc: unexpected term: " ++ str_of_tm(tm)))
};

let render_tm (tm: Binding.Nominal.term)
  : Belt.Result.t(ReasonReact.reactElement, string)
  = switch (NonBinding.from_nominal(tm)) {
      | None => Belt.Result.Error("Failed to convert term")
      | Some(db_tm) => switch (render_doc(db_tm)) {
        | result => Ok(result)
        | exception Bad(msg) => Error(msg)
      }
    };
