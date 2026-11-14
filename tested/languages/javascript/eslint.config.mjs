// CJS require honours NODE_PATH; ESM bare imports do not.
import { createRequire } from "node:module";
const require = createRequire(import.meta.url);
const js = require("@eslint/js");
const globals = require("globals");

// The submission is `source`, without an extension; the tests lint `.js` files.
const files = ["**/!(*.*)", "**/*.js"];

export default [
  { files, ...js.configs.recommended },
  {
    files,
    languageOptions: {
      ecmaVersion: "latest",
      // The yml's `env: node` implies commonjs scoping (globalReturn); "script" flags a top-level `module`.
      sourceType: "commonjs",
      globals: { ...globals.node, ...globals.es2020 },
    },
    rules: {
      "no-var": "warn",
      semi: "warn",
      // Students write the classes, functions and variables, which would be reported as unused.
      "no-unused-vars": "off",
    },
  },
];
