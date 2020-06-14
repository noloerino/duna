import { init, simulate } from "duna_frontend";
import { memory } from "duna_frontend/duna_frontend_bg";

init();

console.log("You're running the duna emulator, version", BUILD_VERSION);

const codeArea = <HTMLTextAreaElement>document.getElementById("code");
const stdout = <HTMLTextAreaElement>document.getElementById("stdout");
const exitCode = <HTMLSpanElement>document.getElementById("exit-code");
const registers = <HTMLTableElement>document.getElementById("registers");

const getProgram = function () {
  return codeArea.value;
};

const assembleAndRunProgram = function () {
  const program = getProgram();
  const simResult = simulate(program);
  exitCode.innerText = simResult.exit_code.toString();
  stdout.value = simResult.get_stdout();
  // Display registers
  let ptr = simResult.get_register_view();
  let count = simResult.get_register_view_size();
  let buf = new Uint32Array(memory.buffer, ptr, count);
  // for (let i = 0; i < count; i++) {
  //     let toDisplay = buf[i];
  //     console.log(toDisplay)
  // }
};

const goButton = document.getElementById("go");
goButton.onclick = (e) => assembleAndRunProgram();
