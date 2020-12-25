import { init, simulate } from "duna_frontend";
import { memory } from "duna_frontend/duna_frontend_bg.wasm";
// import CodeMirror from "codemirror";

init();

console.log("You're running the duna simulator, version", BUILD_VERSION);

// TODO define custom types for RISC-V etc.
// GAS supports ARM and x86-64
const codeMirror = // CodeMirror.fromTextArea(
    <HTMLTextAreaElement>document.getElementById("code");
//     { mode: "gas" }
// );
const stdout = <HTMLTextAreaElement>document.getElementById("stdout");
const exitCode = <HTMLSpanElement>document.getElementById("exit-code");
const compileErrors = <HTMLPreElement>document.getElementById("compile-errors");

const getProgram = function () {
  return codeMirror.value;
};

const assembleAndRunProgram = function () {
  const program = getProgram();
  const simResult = simulate(program);
  exitCode.innerText = simResult.exit_code.toString();
  stdout.value = simResult.get_stdout();
  compileErrors.innerText = simResult.get_compile_errs();
};

const goButton = document.getElementById("go");
goButton.onclick = (e) => assembleAndRunProgram();
