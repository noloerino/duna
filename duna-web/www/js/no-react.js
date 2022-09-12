// Allows the usage of JSX without too much pain.
// Copy/pasted from here, with modifications:
// https://stackoverflow.com/questions/30430982/can-i-use-jsx-without-react-to-inline-html-in-script

const React = {
  // Fragment: function(props, names) {

  // },

  createElement: function (tag, attrs, children) {
    if (tag instanceof Function) {
      // `tag` is a custom-defined component, and is a function
      return tag();
    }

    var element = document.createElement(tag);

    for (let name in attrs) {
      if (name && attrs.hasOwnProperty(name)) {
        let value = attrs[name];
        if (name === "style") {
          if (typeof value !== "object") {
            console.error("Style value expected JS object, got", value);
          }
          let new_value = "";
          for (let [styleKey, styleValue] of Object.entries(value)) {
            // Convert keys from camel case to hyphenated
            styleKey = styleKey.replace(
              /[A-Z]/g,
              (match) => "-" + match.toLowerCase()
            );
            new_value += styleKey + ": " + styleValue + "; ";
          }
          value = new_value.trim();
        }
        if (name === "className") {
          name = "class";
        }
        if (value === true) {
          element.setAttribute(name, name);
        } else if (value !== false && value != null) {
          element.setAttribute(name, value.toString());
        }
      }
    }
    for (let i = 2; i < arguments.length; i++) {
      let child = arguments[i];
      element.appendChild(
        child.nodeType == null
          ? document.createTextNode(child.toString())
          : child
      );
    }
    return element;
  },
};

export default React;
