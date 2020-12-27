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

const STORAGE_MAIN_FILE_KEY = "mainfile";

export class CodeMirrorWrapper {
  constructor() {
    // TODO define custom types for RISC-V etc.
    // GAS supports ARM and x86-64
    this.localStorage = window.localStorage;
    let textArea = document.getElementById("code");
    // This needs to be set instead of the "value" parameter of the cm constructor
    textArea.innerText = this.localStorage.getItem(STORAGE_MAIN_FILE_KEY);
    let cm = CodeMirror.fromTextArea(textArea, {
      mode: "gas",
      lineNumbers: "true",
      theme: "elegant",
    });
    CodeMirror.commands.save = () => {
      console.log("Saved file at " + new Date().toTimeString());
      this.save();
    };
    this.codeMirror = cm;
  }

  getProgram() {
    return this.codeMirror.getValue();
  }

  // Saves the text of the main file in local storage for the time being
  // this will change when multiple files become supported
  save() {
    this.localStorage.setItem(
      STORAGE_MAIN_FILE_KEY,
      this.getProgram()
    );
  }
}
