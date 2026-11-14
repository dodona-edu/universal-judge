// CJS require honours NODE_PATH; ESM bare imports do not.
import { createRequire } from "node:module";
const require = createRequire(import.meta.url);
const globals = require("globals");
const tsPlugin = require("@typescript-eslint/eslint-plugin");
const tsParser = require("@typescript-eslint/parser");

// The submission is `source`, without an extension; the tests lint `.ts` files.
const files = ["**/!(*.*)", "**/*.ts"];

export default [
  ...tsPlugin.configs["flat/recommended"].map((config) => ({ ...config, files })),
  {
    files,
    languageOptions: {
      parser: tsParser,
      ecmaVersion: "latest",
      sourceType: "module",
      globals: { ...globals.node, ...globals.es2020 },
    },
    rules: {
      "no-var": "warn",
      semi: "warn",
      "@typescript-eslint/no-unused-expressions": "off",
    },
  },
];
