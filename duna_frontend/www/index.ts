import { init, simulate } from "duna_frontend";
import { memory } from "duna_frontend/duna_frontend_bg";

init();

const codeArea = <HTMLTextAreaElement>document.getElementById("code");
const stdout = <HTMLTextAreaElement>document.getElementById("stdout");
const exitCode = <HTMLSpanElement>document.getElementById("exit-code");

const getProgram = function () {
  return codeArea.value;
};

const assembleAndRunProgram = function () {
  const program = getProgram();
  const simResult = simulate(program);
  exitCode.innerText = simResult.exit_code.toString();
  stdout.value = simResult.get_stdout();
};

const goButton = document.getElementById("go");
goButton.onclick = (e) => assembleAndRunProgram();
