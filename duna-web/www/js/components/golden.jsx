import { GoldenLayout } from "golden-layout";
// import { LayoutConfig, ComponentContainer, JsonValue } from "golden-layout"
import "golden-layout/dist/css/goldenlayout-base.css";
import "golden-layout/dist/css/themes/goldenlayout-light-theme.css";
// Needed for JSX
import React from "../no-react";

// Takes in a function of the form (() => jsx) and appends it to the golden container
// Eventually this should probably be ((state) => jsx)
//
// The second arg to registerComponent needs to be a function keyword, not a lambda
// because reasons
const goldenComponent = (jsxFn) => {
  return function (container, state) {
    container.element.appendChild(jsxFn());
  };
};

const config = {
  root: {
    type: "row",
    content: [
      {
        type: "column",
        content: [
          {
            type: "component",
            componentType: "editor",
            componentState: {},
            content: [],
          },
          {
            type: "component",
            componentType: "errors",
            componentState: {},
            content: [],
          },
        ],
      },
      {
        type: "column",
        content: [
          {
            type: "component",
            componentType: "state",
            componentState: {},
            content: [],
          },
          {
            type: "component",
            componentType: "console",
            componentState: {},
            content: [],
          },
        ],
      },
    ],
  },
  header: {
    show: "top",
  }
};

export const goldenInit = () => {
  let layout = new GoldenLayout(
    document.getElementById("golden-base")
  );
  layout.registerComponentConstructor(
    "editor",
    goldenComponent(() => (
      <div>
        {/* TODO put this in a toolbar */}
        <div id="buttons" className="flexRow" style={{
            margin: "0.5em",
        }}>
          <button id="assemble">Assemble</button>
          <button id="step">Step</button>
          <button id="run">Run</button>
          <button id="reset">Reset</button>
          <div style={{flex: 6}}></div>
        </div>
        <textarea
          id="code"
          rows="20"
          cols="80"
          placeholder="your code here"
        ></textarea>
      </div>
    ))
  );
  layout.registerComponentConstructor(
    "console",
    goldenComponent(() => (
      // Fragment isn't implemented :(
      <div className="flexCol">
        <p style={{ fontSize: "14px", marginLeft: "0.5em" }}>
          Program exited with code <span id="exit-code">--</span>
        </p>
        <textarea
          id="stdout"
          disabled
          placeholder="console output appears here"
          style={{ flex: 1, fontSize: "14px" }}
        ></textarea>
      </div>
    ))
  );
  layout.registerComponentConstructor(
    "errors",
    goldenComponent(() => (
      <pre
        id="compile-errors"
        style={{ marginLeft: "0.5em", marginRight: "0.5em", fontSize: "14px" }}
      ></pre>
    ))
  );
  layout.registerComponentConstructor(
    "state",
    goldenComponent(() => (
      <textarea id="sim-state" className="flexCol"
        disabled
        placeholder="program state appears here during simulation -- press assemble & step to begin"
        style={{
          fontSize: "14px",
          whiteSpace: "pre-wrap",
          margin: "0.5em"
        }}>
      </textarea>
    ))
  );
  layout.loadLayout(config);
};
