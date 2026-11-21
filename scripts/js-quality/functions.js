// EVERY FUNCTION'S MEASURES, read with ESLint: cognitive complexity (eslint-plugin-sonarjs, Sonar's own
// implementation), cyclomatic complexity (ESLint core `complexity`), length in lines (core `max-lines-per-function`)
// and the inner defs it holds (the `functions` rule below, which also labels each function). Every rule runs with a limit of 0, so it reports every
// function, and the value is read back from its message; inline `eslint-disable` comments are ignored
// (`allowInlineConfig: false`), so no measure hides behind one.
//
// Unlike the Scala measure (quality/MethodMeasures.scala), which folds a method's lambdas and nested defs into it, the
// JS tools measure EVERY function by itself -- a callback or an inner function is a function of its own row, and its
// branches do not count toward the function that holds it. An IIFE (a classic script's module wrapper) has no length
// row (max-lines-per-function's own `IIFEs: false`) and no inner-defs row.
'use strict';

const path = require('node:path');
const { ESLint } = require('eslint');
const sonarjs = require('eslint-plugin-sonarjs');
const tseslint = require('typescript-eslint');

const isIife = (node) => node.parent && node.parent.type === 'CallExpression' && node.parent.callee === node;

// A NAMED inner function is what Scala calls an inner def: a function declaration, or a function bound to a
// variable (`const f = () => ...`). A callback passed as an argument, an object literal's method or an IIFE is not.
const isNamedDef = (node) =>
  node.type === 'FunctionDeclaration' ||
  (node.parent && node.parent.type === 'VariableDeclarator' && node.parent.init === node);

// The line ESLint's `complexity` and sonarjs report a function at: its parent's for a method or property, the `=>`
// token's for an arrow, its own start otherwise. The rows of the four measures are joined on it.
function headLine(node, sourceCode) {
  const parent = node.parent;
  if (parent && ['Property', 'MethodDefinition', 'PropertyDefinition'].includes(parent.type) && parent.value === node) {
    return parent.loc.start.line;
  }
  if (node.type === 'ArrowFunctionExpression') {
    const arrow = sourceCode.getFirstToken(node, { filter: (token) => token.value === '=>' });
    return arrow ? arrow.loc.start.line : node.loc.start.line;
  }
  return node.loc.start.line;
}

const clip = (text) => (text.length > 40 ? `${text.slice(0, 37)}...` : text).replace(/\s+/g, ' ');

// What a reader calls the function: its own name, the variable, property or assignment it is bound to, or, for a
// callback, the call it is handed to (`test('shows the banner')`, `items.forEach`).
function labelOf(node, sourceCode) {
  const parent = node.parent;
  if (node.id) return node.id.name;
  if (parent.type === 'VariableDeclarator' && parent.id.type === 'Identifier') return parent.id.name;
  if (['Property', 'MethodDefinition', 'PropertyDefinition'].includes(parent.type)) return clip(sourceCode.getText(parent.key));
  if (parent.type === 'AssignmentExpression') return clip(sourceCode.getText(parent.left));
  if (parent.type === 'CallExpression' && parent.callee !== node) {
    const title = parent.arguments.find((argument) => argument.type === 'Literal' && typeof argument.value === 'string');
    return `${clip(sourceCode.getText(parent.callee))}(${title ? `'${clip(title.value)}'` : ''}) callback`;
  }
  return isIife(node) ? 'IIFE' : 'anonymous';
}

// Reports, for every function, its label (`name <label>`) and, when it holds any, its inner defs (`inner defs N`).
const functionsRule = {
  meta: { type: 'suggestion', schema: [] },
  create(context) {
    const stack = [];
    const enter = (node) => {
      const holder = [...stack].reverse().find((entry) => !entry.iife);
      if (holder && isNamedDef(node)) holder.innerDefs += 1;
      stack.push({ node, iife: isIife(node), innerDefs: 0 });
    };
    const exit = () => {
      const entry = stack.pop();
      const loc = { line: headLine(entry.node, context.sourceCode), column: 0 };
      context.report({ loc, message: `name ${labelOf(entry.node, context.sourceCode)}` });
      if (!entry.iife && entry.innerDefs > 0) context.report({ loc, message: `inner defs ${entry.innerDefs}` });
    };
    return {
      FunctionDeclaration: enter, FunctionExpression: enter, ArrowFunctionExpression: enter,
      'FunctionDeclaration:exit': exit, 'FunctionExpression:exit': exit, 'ArrowFunctionExpression:exit': exit,
    };
  },
};

const measureRules = {
  'sonarjs/cognitive-complexity': ['error', 0],
  complexity: ['error', { max: 0 }],
  'max-lines-per-function': ['error', { max: 0 }],
  'js-quality/functions': 'error',
};

// Each message's value, by its rule: what the message says, the number in it.
const readers = [
  ['sonarjs/cognitive-complexity', 'cognitive', /Complexity from (\d+) to/],
  ['complexity', 'cyclomatic', /has a complexity of (\d+)/],
  ['max-lines-per-function', 'lines', /has too many lines \((\d+)\)/],
  ['js-quality/functions', 'innerDefs', /^inner defs (\d+)/],
  ['js-quality/functions', 'name', /^name (.+)$/],
];

const isModuleSource = (text) => /^\s*(import|export)\b/m.test(text);

// The flat config for the given files: TypeScript through typescript-eslint's parser (no type information needed),
// JavaScript through espree, each file parsed as a module when it imports or exports and as CommonJS otherwise
// (sloppy mode, so a classic browser script parses as it runs).
function configFor(files, texts) {
  const plugins = { sonarjs, 'js-quality': { rules: { functions: functionsRule } } };
  const ts = files.filter((file) => /\.(ts|mts|cts|tsx)$/.test(file));
  const js = files.filter((file) => !ts.includes(file));
  const modules = js.filter((file) => isModuleSource(texts.get(file)));
  const scripts = js.filter((file) => !modules.includes(file));
  const block = (list, languageOptions) => ({ files: list, plugins, languageOptions, rules: measureRules });
  return [
    ts.length && block(ts, { parser: tseslint.parser, sourceType: 'module', ecmaVersion: 'latest' }),
    modules.length && block(modules, { sourceType: 'module', ecmaVersion: 'latest' }),
    scripts.length && block(scripts, { sourceType: 'commonjs', ecmaVersion: 'latest' }),
  ].filter(Boolean);
}

function rowsOf(result, relative) {
  const rows = new Map();
  for (const message of result.messages) {
    const reader = readers.find(([rule, , pattern]) => rule === message.ruleId && pattern.test(message.message));
    if (!reader) continue;
    const [, field, pattern] = reader;
    const match = pattern.exec(message.message);
    const key = `${relative}:${message.line}`;
    const row = rows.get(key) || { file: relative, line: message.line, name: 'function', cognitive: 0, cyclomatic: 1, lines: 0, innerDefs: 0 };
    if (field === 'name') row.name = row.name === 'function' ? match[1] : `${row.name} / ${match[1]}`;
    else row[field] = Math.max(row[field], Number(match[1]));
    rows.set(key, row);
  }
  return [...rows.values()];
}

// Every function of `files` (relative to `root`), with its measures, and the files ESLint could not parse.
async function measureFunctions(root, files, texts) {
  if (files.length === 0) return { functions: [], unparsed: [] };
  const eslint = new ESLint({
    cwd: root, overrideConfigFile: true, overrideConfig: configFor(files, texts),
    allowInlineConfig: false, warnIgnored: false, errorOnUnmatchedPattern: false,
  });
  const results = await eslint.lintFiles(files.map((file) => path.join(root, file)));
  const functions = [];
  const unparsed = [];
  for (const result of results) {
    const relative = path.relative(root, result.filePath);
    const fatal = result.messages.find((message) => message.fatal);
    if (fatal) unparsed.push({ file: relative, why: `${fatal.line}: ${fatal.message}` });
    else functions.push(...rowsOf(result, relative));
  }
  return { functions, unparsed };
}

module.exports = { measureFunctions };
