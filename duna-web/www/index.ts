import { init, simulate } from "duna_web";
import { memory } from "duna_web/duna_web_bg.wasm";
import { getProgram } from "./components/codemirror";

init();

console.log("You're running the duna simulator, version", BUILD_VERSION);


const stdout = <HTMLTextAreaElement>document.getElementById("stdout");
const exitCode = <HTMLSpanElement>document.getElementById("exit-code");
const compileErrors = <HTMLPreElement>document.getElementById("compile-errors");

const assembleAndRunProgram = function () {
  const program = getProgram();
  const simResult = simulate(program);
  exitCode.innerText = simResult.exit_code.toString();
  stdout.value = simResult.get_stdout();
  compileErrors.innerText = simResult.get_compile_errs();
};

const goButton = document.getElementById("go");
goButton.onclick = (e) => assembleAndRunProgram();
