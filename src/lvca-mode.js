import CodeMirror from "codemirror";
import "codemirror/addon/mode/simple";

/*
import termLexer from "./termLexer.bs.js";
const Lexing = require("bs-platform/lib/js/lexing.js");

CodeMirror.defineMode("lvca", () => ({
  token: stream => {
    // This is pretty wasteful -- for each token we rebuild the lexbuf from the
    // entire input string
    const str = stream.string.substring(stream.pos);
    const lexbuf = Lexing.from_string(str);
    let token = null;
    try {
      token = termLexer.read(lexbuf);
    } catch (error) {
      stream.skipToEnd();
      return null;
    }
    let ret = null;
    console.log({ token: token[0], tag: token.tag, str });
    switch (token.tag) {
      case 2:
        ret = "variable";
        break;
      case 3:
        ret = "number";
        break;
      case 4:
      case 5:
        ret = "variable";
        break;
      // TODO: etc
    }
    console.log({ lexeme_end: Lexing.lexeme_end(lexbuf), "stream.pos": stream.pos });
    stream.pos += Lexing.lexeme_end(lexbuf);
    // for (let i = 0; i < Lexing.lexeme_end(lexbuf); i++) {
    //   stream.next();
    // }
    console.log({ peek: stream.peek() });
    return ret;
  }
}));
*/

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
