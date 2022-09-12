import React from "../no-react.js";

let version = BUILD_VERSION;

// TODO move this to WASM side
let languages = { rv32i: "RV32I (RISC-V 32-bit)", mips32: "MIPS32" };
let languageOpts = Object.entries(languages).map(([key, name], i) => (
  <option value={key}>{name}</option>
));

function Editor(props) {
  return (
    <div>
      {/* TODO put this in a toolbar */}
      <div
        id="buttons"
        className="flexRow"
        style={{
          margin: "0.5em",
        }}
      >
        <button id="assemble">Assemble</button>
        <button id="revert">Step Back</button>
        <button id="step">Step</button>
        <button id="run">Run</button>
        <button id="reset">Reset</button>
        <select name="language" id="language" value={Object.keys(languages)[0]}>
          {/* For some reason our JSX hack isn't happy with iterating*/}
          {languageOpts[0]}
          {languageOpts[1]}
        </select>
        <div style={{ flex: 6 }}></div>
      </div>
      <textarea
        id="code"
        rows="20"
        cols="80"
        placeholder="your code here"
      ></textarea>
    </div>
  );
}

function Console(props) {
  return (
    <div className="flexCol">
      <p style={{ fontSize: "14px", marginLeft: "0.5em" }}>
        Program exited with code <span id="exit-code">--</span>
      </p>
      <textarea
        id="stdout"
        disabled
        placeholder="console output appears here"
        style={{ flex: 6, fontSize: "14px" }}
      ></textarea>
    </div>
  );
}

function ErrorPane(props) {
  return (
    <pre
      id="compile-errors"
      style={{ marginLeft: "0.5em", marginRight: "0.5em", fontSize: "14px" }}
    ></pre>
  );
}

function ProgramState(props) {
  return (
    <textarea
      id="sim-state"
      className="flexCol"
      disabled
      placeholder="program state appears here during simulation -- press assemble & step to begin"
      style={{
        fontSize: "14px",
        whiteSpace: "pre-wrap",
        margin: "0.5em",
      }}
    ></textarea>
  );
}

export function initLayout() {
  let root = document.getElementById("root");
  root.appendChild(
    <div className="flexCol" style={{ flex: 1 }}>
      <div className="flexRow" style={{ flex: 1 }}>
        <div className="flexCol">
          <Editor />
          <ErrorPane />
        </div>
        <div className="flexCol">
          <ProgramState />
          <Console />
        </div>
      </div>
      <p className="plaintext">
        Build version: <i>{version}</i>
      </p>
    </div>
  );
}
