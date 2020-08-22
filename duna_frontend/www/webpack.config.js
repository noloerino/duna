const webpack = require("webpack");
const CopyWebpackPlugin = require("copy-webpack-plugin");
const path = require("path");
const execSync = require("child_process").execSync;

// obtain commit and date
const version = execSync("git rev-parse --short HEAD").toString().trim()
+
  " (" + execSync("git show -s --format=\"%cd\" --date=short").toString().trim() + ")";

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
    new CopyWebpackPlugin({patterns: ['index.html']}),
    new webpack.DefinePlugin({
      BUILD_VERSION: JSON.stringify(version)
    })
  ],
};
