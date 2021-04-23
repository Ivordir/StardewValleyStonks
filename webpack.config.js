// Adapted from https://github.com/fable-compiler/webpack-config-template

// Dependencies: sass, sass-loader, css-loader, style-loader, file-loader

var path = require("path");
var webpack = require("webpack");
var HtmlWebpackPlugin = require("html-webpack-plugin");
var CopyWebpackPlugin = require("copy-webpack-plugin");
var MiniCssExtractPlugin = require("mini-css-extract-plugin");

var config = {
  indexHtmlTemplate: "./public/index.html",
  fsharpProductionEntry: "./build/production/View.js",
  fsharpDevelopmentEntry: "./build/development/View.js",
  cssEntry: "./sass/main.sass",
  outputDir: "./build/deploy",
  assetsDir: "./public",
  host: '0.0.0.0',
  devServerPort: 8080
}

// If we're running webpack serve, assume we're in development mode
var isProduction = !process.argv.find(v => v.indexOf("serve") !== -1);
console.log("Bundling for " + (isProduction ? "production" : "development") + "...");

// The HtmlWebpackPlugin allows us to use a template for the index.html page
// and automatically injects <script> or <link> tags for generated bundles.
var commonPlugins = [
  new HtmlWebpackPlugin({
    filename: "index.html",
    template: resolve(config.indexHtmlTemplate)
  })
];

module.exports = {
  // In development, bundle styles together with the code so they can also
  // trigger hot reloads. In production, put them in a separate CSS file.
  entry:
    isProduction
      ? { app: [resolve(config.fsharpProductionEntry), resolve(config.cssEntry)] }
      : { app: [resolve(config.fsharpDevelopmentEntry)], style: [resolve(config.cssEntry)] },

  // Add a hash to the output file name in production to prevent browser caching if code changes
  output: {
    path: resolve(config.outputDir),
    filename: isProduction ? "[name].[contenthash].js" : "[name].js",
    clean: true
  },

  mode: isProduction ? "production" : "development",

  devtool: isProduction ? "source-map" : "eval-source-map",
  //devtool: isProduction ? "source-map" : "eval-source-map",

  optimization: {
    // Split the code coming from npm packages into a different file.
    // 3rd party dependencies change less often, let the browser cache them.
    splitChunks: {
      cacheGroups: {
        commons: {
          test: /node_modules/,
          name: "vendors",
          chunks: "all"
        }
      }
    },
  },

  // Besides the HtmlPlugin, we use the following plugins:
  // PRODUCTION
  //      - MiniCssExtractPlugin: Extracts CSS from bundle to a different file
  //          To minify CSS, see https://github.com/webpack-contrib/mini-css-extract-plugin#minimizing-for-production
  //      - CopyWebpackPlugin: Copies static assets to output directory
  // DEVELOPMENT
  //      - HotModuleReplacementPlugin: Enables hot reloading when code changes without refreshing
  plugins: isProduction ?
      commonPlugins.concat([
          new MiniCssExtractPlugin({ filename: "style.css" }),
          new CopyWebpackPlugin({ patterns: [{
            from: resolve(config.assetsDir),
            //globOptions: { ignore: [ "*.html" ] },
            //currently a problem with glob/copywebpack, here we use a filter instead
            filter: path => !(path.includes(".html")),
            noErrorOnMissing: true
          }] })
      ])
      : commonPlugins.concat([
          new webpack.HotModuleReplacementPlugin(),
      ]),

  // Configuration for webpack-dev-server
  devServer: {
      publicPath: "/",
      contentBase: resolve(config.assetsDir),
      port: config.devServerPort,
      hot: true,
      inline: true
  },

  // - sass-loaders: transforms SASS/SCSS into JS
  // - file-loader: Moves files referenced in the code (fonts, images) into output folder
  module: {
    rules: [
	  {
        test: /\.js$/,
        enforce: "pre",
        use: ["source-map-loader"],
      },
      {
        test: /\.(sass|scss|css)$/,
        use: [
          isProduction
            ? MiniCssExtractPlugin.loader
            : "style-loader",
          "css-loader",
          {
            loader: "sass-loader",
            options: { implementation: require("sass") }
          }
        ],
      },
      {
        test: /\.(png|jpg|jpeg|gif|svg|woff|woff2|ttf|eot)(\?.*)?$/,
        use: ["file-loader"]
      }
    ]
  }
};

function resolve(filePath) {
    return path.isAbsolute(filePath) ? filePath : path.join(__dirname, filePath);
}
