// These extra CSS imports probably would go over poorly with typescript, so we expose
// a smaller interface
// https://github.com/codemirror/CodeMirror/issues/5484
import CodeMirror from "codemirror/lib/codemirror.js";
// https://github.com/survivejs-demos/react-demos/blob/master/demos/index.js
// https://stackoverflow.com/questions/55685029/codemirror-with-vanilla-typescript-and-webpack
import "codemirror/lib/codemirror.css";
import "codemirror/mode/gas/gas";
import "codemirror/theme/elegant.css";
// import "codemirror/theme/monokai.css";

export class CodeMirrorWrapper {
  constructor() {
    // TODO define custom types for RISC-V etc.
    // GAS supports ARM and x86-64
    this.codeMirror = CodeMirror.fromTextArea(document.getElementById("code"), {
      mode: "gas",
      lineNumbers: "true",
      theme: "elegant",
    });
  }

  getProgram() {
    return this.codeMirror.getValue();
  }
}
