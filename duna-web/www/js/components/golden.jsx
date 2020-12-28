// JQuery is required for GoldenLayout, meaning it's available even if it's not a direct dependency
import "jquery";
import GoldenLayout from "golden-layout";
import "golden-layout/src/css/goldenlayout-base.css";
import "golden-layout/src/css/goldenlayout-light-theme.css";
// Needed for JSX
import React from "../no-react";

// Takes in a function of the form ((state) => jsx) and appends it to the golden container
//
// The second arg to registerComponent needs to be a function keyword, not a lambda
// because reasons
const goldenComponent = (jsxFn) => {
  return function (container, state) {
    container.getElement()[0].appendChild(jsxFn(state));
  };
};

const config = {
  settings: {
    showPopoutIcon: false,
    showMaximiseIcon: false,
    showCloseIcon: false,
  },
  content: [
    {
      type: "row",
      height: 100,
      content: [
        {
          type: "column",
          content: [
            {
              type: "component",
              componentName: "editor",
              componentState: {},
              isClosable: false,
            },
            {
              type: "component",
              componentName: "errors",
              componentState: {},
              isClosable: false,
            },
          ],
        },
        {
          type: "stack",
          content: [
            {
              type: "component",
              componentName: "state",
              componentState: {},
              isClosable: false,
            },
            {
              type: "component",
              componentName: "console",
              componentState: {},
              isClosable: false,
            },
          ],
        },
      ],
    },
  ],
};

export const goldenInit = () => {
  let layout = new GoldenLayout(
    config,
    document.getElementById("golden-base")
  );
  layout.registerComponent(
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
  layout.registerComponent(
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
  layout.registerComponent(
    "errors",
    goldenComponent(() => (
      <pre
        id="compile-errors"
        style={{ marginLeft: "0.5em", marginRight: "0.5em", fontSize: "14px" }}
      ></pre>
    ))
  );
  layout.registerComponent(
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
  layout.init();
};
