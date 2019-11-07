let make_elem = (name, children) => {
  ReactDOMRe.createDOMElementVariadic(
    name,
    ~props=ReactDOMRe.domProps(),
    children
  )
};

let make_div = make_elem("div");
