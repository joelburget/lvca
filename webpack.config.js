const path = require('path');

const isProd = process.env.NODE_ENV === 'production';

module.exports = {
  entry: {
    index: './lib/js/src/Index.bs.js',
    checkingDebugger: './lib/js/src/CheckingDebugger.bs.js',
    debugger: './lib/js/src/Debugger.bs.js',
    termRender: './lib/js/src/TermRender.bs.js',
  },
  mode: isProd ? 'production' : 'development',
  output: {
    path: path.join(__dirname, "bundledOutputs"),
    filename: '[name].js',
  },
  node: {
    fs: "empty",
  },
};
