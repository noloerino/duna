const webpack = require("webpack");
const CopyPlugin = require("copy-webpack-plugin");
const path = require("path");
const execSync = require("child_process").execSync;

// ===
// Copied from here:
// https://github.com/cockpit-project/starter-kit/commit/3220617fec508aabbbc226a87a165c21fb72e913
//
// HACK: OpenSSL 3 does not support md4 any more, but webpack hardcodes it all over the place: https://github.com/webpack/webpack/issues/13572
const crypto = require("crypto");
const crypto_orig_createHash = crypto.createHash;
crypto.createHash = algorithm => crypto_orig_createHash(algorithm == "md4" ? "sha256" : algorithm);
// ===

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
