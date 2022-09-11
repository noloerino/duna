const webpack = require("webpack");
const CopyPlugin = require("copy-webpack-plugin");
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
        // https://github.com/rustwasm/wasm-pack/issues/835
        test: /\.wasm$/,
        type: "webassembly/sync"
      },
      {
        test: /\.tsx?$/,
        use: "ts-loader",
        exclude: /node_modules/
      },
      {
        test: /\.jsx$/,
        exclude: /node_modules/,
        use: {
          loader: "babel-loader",
          options: {
            plugins: ["@babel/plugin-transform-react-jsx"]
          }
        }
      },
      {
        test: /\.css$/i,
        use: ["style-loader", {"loader": "css-loader", "options": {"modules": false}}],
      },
      {
        test: /\.png$/,
        type: "asset/resource"
      }
    ]
  },
  experiments: {
    syncWebAssembly: true
  },
  resolve: {
      extensions: [".js",]
  },
  output: {
    path: path.resolve(__dirname, "dist"),
    filename: "bootstrap.js",
  },
  mode: "development",
  plugins: [
    new CopyPlugin({patterns: ['index.html']}),
    new webpack.DefinePlugin({
      BUILD_VERSION: JSON.stringify(version)
    })
  ],
};
