let startStr = {|document(viewbox(0; 0; 100; 100); 400; 400; [
  styled-element(""; rect(70; 80; 20; 20; nothing(); nothing())),
  styled-element(""; circle(50; 50; 20)),
  styled-element(""; polygon([point(25;25), point(30;10), point(30;30), point(50;0)]))
])|};

ReactDOMRe.renderToElementWithId(
  <TermRender
    eval=LanguageSvg.eval_tm
    startStr=startStr
  />,
  "index");
