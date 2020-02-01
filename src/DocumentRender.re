let startStr = {|document([
  header(h2(); "foo"),
  paragraph(inline([inlineAtom([]; "paragraph text")]))
])|};

ReactDOMRe.renderToElementWithId(
  <TermRender
    eval=LanguageDocumentRender.render_tm
    startStr=startStr
  />,
  "index");
