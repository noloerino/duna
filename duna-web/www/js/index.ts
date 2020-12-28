import { init as duna_init, SimState, SimSnapshot } from "duna_web";
import { memory } from "duna_web/duna_web_bg.wasm";
import { CodeMirrorWrapper } from "./components/codemirror";
import { goldenInit } from "./components/golden.jsx";

duna_init();
// All DOM manipulation must occur after goldenInit
goldenInit();
let cm = new CodeMirrorWrapper();

let simState = SimState.new();

console.log("You're running the duna simulator, version", BUILD_VERSION);

const assembleProgram = () => {
  const compileErrors = <HTMLPreElement>(
    document.getElementById("compile-errors")
  );
  const program = cm.getProgram();
  simState.assemble(program);
  compileErrors.innerText = simState.get_errors() ?? "No errors!";
};

const updateState = () => {
  const stdout = <HTMLTextAreaElement>document.getElementById("stdout");
  const exitCode = <HTMLSpanElement>document.getElementById("exit-code");
  let ec = simState.result();
  if (ec) {
    exitCode.innerText = ec!!.toString();
  }
  let stdoutText = simState.stdout();
  if (stdoutText) {
    stdout.value = stdoutText;
  }
  let snapshot = simState.snapshot();
  if (!snapshot) {
    return;
  }
  const state = <HTMLTextAreaElement>document.getElementById("sim-state");
  state.innerText =
    (`PC: ${snapshot.curr_pc()}\n` +
    `next instruction: ${snapshot.curr_inst()}\n\n` +
    `${snapshot.reg_dump()}`);
}

const step = () => {
  simState.step()
  updateState();
};

const run = () => {
  simState.run();
  updateState();
};

const assembleButton = document.getElementById("assemble");
assembleButton.onclick = (e) => assembleProgram();

const stepButton = document.getElementById("step");
stepButton.onclick = (e) => step();

const runButton = document.getElementById("run");
runButton.onclick = (e) => run();

window.onbeforeunload = () => {
  cm.save();
};
