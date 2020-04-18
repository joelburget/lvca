open Bidirectional;
open Bidirectional_TestUtil;
open Statics;


module CheckingDebugger = {

  [@react.component]
  let make = () => {

    let abstract = mkSection("Abstract", true, state.abstractExpanded,
                             ToggleAbstract, () =>
      <ContainedAbstractSyntaxEditor
        onUpdate=(update => dispatch(AbstractUpdate(update)))
        // XXX update initialInput
        initialInput=LanguageSimple.abstractSyntax
      />
    );

    let statics = mkSection("Statics", abstractValid, state.staticsExpanded,
                            ToggleStatics, () =>
      <StaticsEditor
        onUpdate=(statics => dispatch(StaticsUpdate(statics)))
        initialInput=LanguageSimple.statics
      />
    );

    let debugger = mkSection("Debugger", abstractValid && staticsValid,
                             state.debuggerExpanded, ToggleDebugger, () => {
      let debuggerInput =
        <input
          type_="text"
          onKeyUp=(event => if (ReactEvent.Keyboard.key(event) == "Enter") {
            dispatch(Evaluate(ReactEvent.Keyboard.target(event)##value));
          })
        />;
    });

    <div className="lvca-viewer">
      (abstract)
      (statics)
      (debugger)
    </div>
  }
};

ReactDOMRe.renderToElementWithId(<CheckingDebugger />, "index");
