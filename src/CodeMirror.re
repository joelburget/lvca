[@bs.module "codemirror/keymap/vim"] external _vimImport : unit = "default";
[@bs.module] external _modeImport : unit = "../../../src/lvca-mode";

[@bs.deriving abstract]
type options = {
  [@bs.optional] keyMap: string,
  [@bs.optional] mode: string,
};

// A CodeMirror editor
type editor = {.};

// A React event
[@bs.deriving abstract]
type event = {
  key: string,
  shiftKey: bool,
};

let focusEditor : editor => unit = [%raw "editor => editor.focus()" ];

[@react.component][@bs.module "react-codemirror2"]
external make:
  (~value: string,
   ~onBeforeChange: (editor, {.}, string) => unit,
   ~options: options,
   ~onKeyDown: (editor, event) => unit =?,
   ~editorDidMount: (editor) => unit = ?)
  => React.element
  = "Controlled";
