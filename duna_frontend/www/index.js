import {init, simulate} from "duna_frontend";
import {memory} from "duna_frontend/duna_frontend_bg";

init();

const codeArea = document.getElementById("code");
const stdout = document.getElementById("stdout");
const exitCode = document.getElementById("exit-code");

const getProgram = function() {
    return codeArea.value;
}

const assembleAndRunProgram = function() {
    const program = getProgram();
    const simResult = simulate(program);
    exitCode.innerText = simResult.exit_code;
    stdout.value = simResult.get_stdout();
};

const goButton = document.getElementById("go");
goButton.onclick = e => assembleAndRunProgram();