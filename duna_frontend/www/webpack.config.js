const CopyWebpackPlugin = require("copy-webpack-plugin");
const path = require('path');

module.exports = {
  entry: "./bootstrap.js",
  devtool: "inline-source-map",
  module: {
      rules: [
          {
              test: /\.tsx?$/,
              use: "ts-loader",
              exclude: /node_modules/
          }
      ]
  },
  resolve: {
      extensions: [".tsx", ".ts", ".js", ".wasm"]
  },
  output: {
    path: path.resolve(__dirname, "dist"),
    filename: "bootstrap.js",
  },
  mode: "development",
  plugins: [
    new CopyWebpackPlugin(['index.html'])
  ],
};
