import CodeMirror from "codemirror";
import "codemirror/addon/mode/simple";

CodeMirror.defineSimpleMode("lvca", {
  // The start state contains the rules that are intially used
  start: [
    // literals
    {regex: /"(?:[^\\]|\\.)*?(?:"|$)/, token: "string"},
    {regex: /true|false/, token: "atom"},
    {regex: /0x[a-f\d]+|[-+]?(?:\.\d+|\d+\.?\d*)(?:e[-+]?\d+)?/i,
     token: "number"},

    // variables / operators
    {regex: /[a-z-][\w-]*/, token: "variable"},

    {regex: /[\(]/, indent: true},
    {regex: /[\)]/, dedent: true},

    // comments
    {regex: /\/\/.*/, token: "comment"},
    {regex: /\/\*/, token: "comment", next: "comment"},
  ],
  // The multi-line comment state.
  comment: [
    {regex: /.*?\*\//, token: "comment", next: "start"},
    {regex: /.*/, token: "comment"}
  ],
  // The meta property contains global information about the mode. It
  // can contain properties like lineComment, which are supported by
  // all modes, and also directives like dontIndentStates, which are
  // specific to simple modes.
  meta: {
    dontIndentStates: ["comment"],
    lineComment: "//"
  }
});
