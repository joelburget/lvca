module Debugger = {
  [@react.component]
  let make = () => {
    <div>{React.string("hello, world!")}</div>
  }
};

ReactDOMRe.renderToElementWithId(<Debugger />, "index");
