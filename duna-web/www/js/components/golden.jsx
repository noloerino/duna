// JQuery is required for GoldenLayout, meaning it's available even if it's not a direct
// dependency
import $ from "jquery";
import GoldenLayout from "golden-layout";
import "golden-layout/src/css/goldenlayout-base.css";
import "golden-layout/src/css/goldenlayout-light-theme.css";
// Needed for JSX
import React from "../no-react";

// Takes in a function of the form (state) => jsx
// and appends it to the golden container
const goldenComponent = (jsxFn) => {
  return function (container, state) {
    container.getElement()[0].appendChild(jsxFn(state));
  };
};

const config = {
  settings: {
    showPopoutIcon: false,
    showMaximizeIcon: false,
  },
  content: [
    {
      type: "row",
      height: 100,
      content: [
        {
          type: "component",
          componentName: "editor",
          componentState: {},
        },
        {
          type: "column",
          content: [
            {
              type: "component",
              componentName: "console",
              componentState: {},
            },
            {
              type: "component",
              componentName: "errors",
              componentState: {},
            },
          ],
        },
      ],
    },
  ],
};

export const goldenInit = () => {
  let layout = new GoldenLayout(config,
    // document.getElementById("golden-base")
    $("#golden-base")
    // null
    );
  // The second arg to registerComponent needs to be a function keyword, not a lambda
  // because reasons
  layout.registerComponent(
    "editor",
    goldenComponent(() => (
      <div style="flex: 1;">
        <textarea
          id="code"
          rows="20"
          cols="80"
          placeholder="your code here"
        ></textarea>
        <div style="margin-left: 1em;">
          <p>
            Program exited with code <span id="exit-code">--</span>
          </p>
          {/* TODO replace this with assemble/simulate buttons */}
          <button id="go">Go!</button>
        </div>
      </div>
    )
  ));
  layout.registerComponent(
    "console",
    goldenComponent(() => (
      <textarea
        id="stdout"
        disabled
        placeholder="console output appears here"
        style="flex: 1;"
      ></textarea>
    ))
  );
  layout.registerComponent(
    "errors",
    goldenComponent(() => (
      <pre id="compile-errors" style="margin-left: 2em; margin-right: 2em;"></pre>
    ))
  );
  layout.init();
};
