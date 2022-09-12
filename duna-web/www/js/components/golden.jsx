import React from "../no-react.js";

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
        <button id="step">Step</button>
        <button id="run">Run</button>
        <button id="reset">Reset</button>
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
  );
}
