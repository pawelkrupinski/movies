// THE JS/TS EXTRACTOR of the general rule core (scripts/design-rules/, model.py's `Module`): each file's names, the
// effects it touches, the effects run when it is loaded, the module-level state its functions write, its stateful
// and abstract types -- read from the parsers ESLint itself uses (espree for JS, typescript-estree for TS). The
// RULES are not here: design_check.py and js_quality_report.py read these facts, as they read Python's and Scala's.
'use strict';

const path = require('node:path');

// A dotted name (with a leading `window.` / `globalThis.` dropped) -> its effect; the first match wins.
const EffectNames = [
  ['clock', ['Date.now', 'performance.now']],
  ['env', ['process.env']],
  ['network', ['fetch', 'XMLHttpRequest', 'WebSocket', 'EventSource', 'navigator.sendBeacon']],
  ['storage', ['localStorage', 'sessionStorage', 'indexedDB', 'document.cookie']],
  ['random', ['Math.random', 'crypto.getRandomValues', 'crypto.randomUUID']],
  ['logging', ['console']],
  ['dom', ['document', 'window', 'navigator', 'location', 'history']],
];
const EffectModules = new Map([['child_process', 'process'], ['fs', 'files'], ['fs/promises', 'files'],
  ['http', 'network'], ['https', 'network'], ['net', 'network']]);
const AtImport = new Set(['clock', 'process', 'env', 'network', 'storage', 'random', 'files']);
const Mutators = new Set(['push', 'pop', 'shift', 'unshift', 'splice', 'set', 'add', 'delete', 'clear', 'sort', 'reverse', 'fill']);
const Containers = new Set(['ObjectExpression', 'ArrayExpression', 'NewExpression']);
const Skipped = new Set(['parent', 'loc', 'range', 'tokens', 'comments']);
const isFunction = (node) => /Function/.test(node.type) && node.type !== 'TSDeclareFunction';

function children(node) {
  const out = [];
  for (const [key, value] of Object.entries(node)) {
    if (Skipped.has(key) || !value || typeof value !== 'object') continue;
    for (const child of Array.isArray(value) ? value : [value]) if (child && typeof child.type === 'string') out.push(child);
  }
  return out;
}

// Every node under `node`; `enter(node)` false stops the descent below it.
function walk(node, enter) {
  if (enter(node) === false) return;
  for (const child of children(node)) walk(child, enter);
}

function dotted(node) {
  const parts = [];
  let at = node;
  while (at.type === 'MemberExpression' && !at.computed && at.property.type === 'Identifier') {
    parts.unshift(at.property.name);
    at = at.object;
  }
  if (at.type !== 'Identifier') return null;
  const name = [at.name, ...parts].join('.');
  return name.replace(/^(window|globalThis|self)\.(?=.)/, '');
}

function effectOf(name) {
  for (const [kind, prefixes] of EffectNames) {
    if (prefixes.some((p) => name === p || name.startsWith(`${p}.`))) return kind;
  }
  return null;
}

const moduleSource = (node) => {
  if (node.type === 'ImportDeclaration') return node.source.value;
  const isRequire = node.type === 'CallExpression' && node.callee.type === 'Identifier' && node.callee.name === 'require';
  return isRequire && node.arguments[0] && node.arguments[0].type === 'Literal' ? node.arguments[0].value : null;
};

// The effect one node is (null for most): a chain's top, `new Date()`, an import of an effectful node module.
function effectAt(node, parent) {
  const source = moduleSource(node);
  if (typeof source === 'string') return EffectModules.get(source.replace(/^node:/, '')) || null;
  if (node.type === 'NewExpression' && node.callee.type === 'Identifier' && node.callee.name === 'Date' && node.arguments.length === 0) return 'clock';
  if (node.type !== 'MemberExpression' && node.type !== 'Identifier') return null;
  if (parent && parent.type === 'MemberExpression' && parent.object === node) return null;
  if (parent && parent.type === 'MemberExpression' && parent.property === node) return null;
  if (parent && /Property|MethodDefinition/.test(parent.type) && parent.key === node) return null;
  const name = dotted(node);
  return name ? effectOf(name) : null;
}

function countEffects(root, { intoFunctions }) {
  const counts = {};
  const visit = (node, parent) => {
    const kind = effectAt(node, parent);
    if (kind) counts[kind] = (counts[kind] || 0) + 1;
    const isIife = parent && parent.type === 'CallExpression' && parent.callee === node;
    if (isFunction(node) && !intoFunctions && !isIife) return;
    for (const child of children(node)) visit(child, node);
  };
  visit(root, null);
  return counts;
}

const Definitions = /^(FunctionDeclaration|ClassDeclaration|ImportDeclaration|TSInterfaceDeclaration|TSTypeAliasDeclaration)$/;

function importIo(program) {
  let count = 0;
  for (const statement of program.body) {
    const inner = /^Export/.test(statement.type) && statement.declaration ? statement.declaration : statement;
    if (Definitions.test(inner.type)) continue;
    const counts = countEffects(inner, { intoFunctions: false });
    for (const [kind, n] of Object.entries(counts)) if (AtImport.has(kind)) count += n;
  }
  return count;
}

const declarationOf = (statement) => (/^Export/.test(statement.type) && statement.declaration ? statement.declaration : statement);

// Module-level names a function may write: every `let` / `var`, and every `const` bound to a container.
function moduleBindings(program) {
  const rebindable = new Set();
  const containers = new Set();
  for (const statement of program.body.map(declarationOf)) {
    if (statement.type !== 'VariableDeclaration') continue;
    for (const d of statement.declarations) {
      if (d.id.type !== 'Identifier') continue;
      if (statement.kind !== 'const') rebindable.add(d.id.name);
      else if (d.init && Containers.has(d.init.type)) containers.add(d.id.name);
    }
  }
  return { rebindable, containers };
}

const rootName = (node) => {
  let at = node;
  while (at.type === 'MemberExpression') at = at.object;
  return at.type === 'Identifier' ? at.name : null;
};

// The name one node inside a function writes, if it is a module binding.
function writtenName(node, { rebindable, containers }) {
  const target = node.type === 'AssignmentExpression' ? node.left : node.type === 'UpdateExpression' ? node.argument : null;
  if (target && target.type === 'Identifier') return rebindable.has(target.name) ? target.name : null;
  if (target && target.type === 'MemberExpression') {
    const name = rootName(target);
    return rebindable.has(name) || containers.has(name) ? name : null;
  }
  const isMutatorCall = node.type === 'CallExpression' && node.callee.type === 'MemberExpression'
    && !node.callee.computed && Mutators.has(node.callee.property.name);
  if (!isMutatorCall) return null;
  const name = rootName(node.callee.object);
  return rebindable.has(name) || containers.has(name) ? name : null;
}

function moduleState(program) {
  const bindings = moduleBindings(program);
  const written = new Set();
  for (const statement of program.body) {
    walk(statement, (node) => {
      if (!isFunction(node)) return true;
      if (node.body) walk(node.body, (inner) => { const name = writtenName(inner, bindings); if (name) written.add(name); });
      return false;
    });
  }
  return [...written].sort();
}

const writesThis = (fn) => {
  let found = false;
  walk(fn, (node) => {
    if (node.type === 'AssignmentExpression' && node.left.type === 'MemberExpression' && node.left.object.type === 'ThisExpression') found = true;
  });
  return found;
};

function classFacts(node) {
  const methods = node.body.body.filter((m) => m.type === 'MethodDefinition' && m.kind !== 'constructor');
  const publicDefs = methods.filter((m) => m.key.type === 'Identifier' && !m.key.name.startsWith('_')).length;
  const stateful = methods.some((m) => writesThis(m.value));
  return { name: node.id ? node.id.name : 'default', publicDefs, stateful, abstract: Boolean(node.abstract), line: node.loc.start.line };
}

const exportsOnlyTypes = (program) => {
  const exported = program.body.filter((s) => /^Export(Named|Default)Declaration$/.test(s.type) && s.declaration);
  return exported.length > 0 && exported.every((s) => /^TS(InterfaceDeclaration|TypeAliasDeclaration)$/.test(s.declaration.type));
};

const isFunctionBinding = (d) => d.id.type === 'Identifier' && d.init && /Function/.test(d.init.type);

function names(file, program) {
  const out = [{ kind: 'module', name: path.basename(file).replace(/\.[^.]+$/, ''), publicDefs: 0, line: 1 }];
  const classes = [];
  for (const statement of program.body.map(declarationOf)) {
    if (statement.type === 'ClassDeclaration' && statement.id) classes.push(classFacts(statement));
    else if (statement.type === 'FunctionDeclaration' && statement.id) out.push({ kind: 'function', name: statement.id.name, publicDefs: 0, line: statement.loc.start.line });
    else if (statement.type === 'VariableDeclaration') {
      for (const d of statement.declarations.filter(isFunctionBinding)) out.push({ kind: 'function', name: d.id.name, publicDefs: 0, line: d.loc.start.line });
    }
  }
  out.push(...classes.map((c) => ({ kind: 'type', name: c.name, publicDefs: c.publicDefs, line: c.line })));
  return { names: out, classes };
}

// THE FUNCTIONS (the `mixed` rule's inputs, model.Function): every named function -- a declaration, a function bound
// to a name, a method, a property's function -- qualified by the classes and functions holding it; an anonymous
// callback is folded into the function holding it (one at the top level into `(anonymous)`). Each: the effects its own
// body touches (a name bound by an import of `child_process` / `fs` / `http` counts as that effect where it is used),
// the local modules its own calls go through (resolved against `known`, the files measured), its cyclomatic complexity.
const Branches = /^(IfStatement|ConditionalExpression|ForStatement|ForInStatement|ForOfStatement|WhileStatement|DoWhileStatement|CatchClause)$/;

function decisionsAt(node) {
  if (Branches.test(node.type)) return 1;
  if (node.type === 'SwitchCase') return node.test ? 1 : 0;
  return node.type === 'LogicalExpression' ? 1 : 0;
}

const Extensions = ['', '.ts', '.js', '.mjs', '.cjs', '.tsx', '/index.ts', '/index.js'];

function resolveLocal(file, source, known) {
  if (typeof source !== 'string' || !source.startsWith('.')) return null;
  const base = path.posix.normalize(path.posix.join(path.posix.dirname(file), source.replace(/\.js$/, '')));
  const withJs = path.posix.normalize(path.posix.join(path.posix.dirname(file), source));
  for (const candidate of [withJs, ...Extensions.map((e) => base + e)]) if (known.has(candidate)) return candidate;
  return null;
}

// local name -> { effect } for a binding of an effectful node module, { module } for one of a measured local module
function importBindings(file, program, known) {
  const out = new Map();
  // each binding: [local name, the name it imports (null: the whole module, read by its members)]
  const bind = (pairs, source) => {
    const effect = typeof source === 'string' ? EffectModules.get(source.replace(/^node:/, '')) : null;
    const local = resolveLocal(file, source, known);
    for (const [name, imported] of pairs) if (effect || local) out.set(name, effect ? { effect } : { module: local, imported });
  };
  const specifier = (sp) => [sp.local.name, sp.type === 'ImportSpecifier' ? (sp.imported.name || sp.imported.value)
    : sp.type === 'ImportDefaultSpecifier' ? 'default' : null];
  const patternPairs = (id) => (id.type === 'Identifier' ? [[id.name, null]]
    : id.type === 'ObjectPattern' ? id.properties.filter((p) => p.value && p.value.type === 'Identifier' && p.key && p.key.type === 'Identifier')
      .map((p) => [p.value.name, p.key.name]) : []);
  for (const statement of program.body) {
    if (statement.type === 'ImportDeclaration') bind(statement.specifiers.map(specifier), statement.source.value);
    if (statement.type !== 'VariableDeclaration') continue;
    for (const d of statement.declarations) if (d.init && moduleSource(d.init)) bind(patternPairs(d.id), moduleSource(d.init));
  }
  return out;
}

function nameOf(node, parent) {
  if (node.id && node.id.type === 'Identifier') return node.id.name;
  if (!parent) return null;
  if (parent.type === 'VariableDeclarator' && parent.init === node && parent.id.type === 'Identifier') return parent.id.name;
  if (/^(MethodDefinition|Property|PropertyDefinition)$/.test(parent.type) && parent.value === node && !parent.computed) {
    return parent.key.type === 'Identifier' ? parent.key.name : parent.key.type === 'Literal' ? String(parent.key.value) : null;
  }
  if (parent.type === 'AssignmentExpression' && parent.right === node) return dotted(parent.left);
  return null;
}

// An identifier read as a name: no property key, member name, import specifier or TypeScript type.
const isReference = (node, parent) => !parent || !(
  (parent.type === 'MemberExpression' && parent.property === node && !parent.computed)
  || (/Property|MethodDefinition/.test(parent.type) && parent.key === node && !parent.computed)
  || /^Import/.test(parent.type) || /^TS/.test(parent.type));

function functionFacts(file, program, known) {
  const bindings = importBindings(file, program, known);
  const records = new Map();
  const recordOf = (name, line) => {
    if (!records.has(name)) records.set(name, { name, line, effects: {}, calls: new Set(), cyclomatic: 1 });
    return records.get(name);
  };
  const visit = (node, parent, scope, current) => {
    let here = current;
    let inner = scope;
    if (isFunction(node)) {
      const name = nameOf(node, parent);
      if (name) {
        inner = [...scope, name];
        here = recordOf(inner.join('.'), node.loc.start.line);
      } else if (!here) here = recordOf('(anonymous)', node.loc.start.line);
    } else if (/^Class(Declaration|Expression)$/.test(node.type) && node.id) inner = [...scope, node.id.name];
    if (here) {
      const kind = node.type === 'ImportDeclaration' ? null : effectAt(node, parent);
      const bound = node.type === 'Identifier' && isReference(node, parent) ? bindings.get(node.name) : null;
      const effect = kind || (bound && bound.effect) || null;
      if (effect) here.effects[effect] = (here.effects[effect] || 0) + 1;
      if (bound && bound.module) here.calls.add(bound.module);
      here.cyclomatic += decisionsAt(node);
    }
    for (const child of children(node)) visit(child, node, inner, here);
  };
  visit(program, null, [], null);
  return [...records.values()].map((r) => ({ name: r.name, line: r.line, effects: r.effects, calls: [...r.calls].sort(), cyclomatic: r.cyclomatic }));
}

// [module, name] of every name read through an import of a measured local module (a whole module's by its members):
// the split credit's evidence (scripts/design-rules/splits.py).
function moduleReferences(file, program, known) {
  const bindings = importBindings(file, program, known);
  const found = new Set();
  const visit = (node, parent) => {
    const bound = node.type === 'Identifier' && isReference(node, parent) ? bindings.get(node.name) : null;
    if (bound && bound.module) {
      const member = parent && parent.type === 'MemberExpression' && parent.object === node && !parent.computed ? parent.property.name : null;
      const name = bound.imported || member;
      if (name) found.add(JSON.stringify([bound.module, name]));
    }
    for (const child of children(node)) visit(child, node);
  };
  visit(program, null);
  return [...found].sort().map((pair) => JSON.parse(pair));
}

// A file's facts, the shape model.Module reads.
function factsOf(file, program, known = new Set()) {
  const { names: found, classes } = names(file, program);
  return {
    path: file,
    names: found,
    effects: countEffects(program, { intoFunctions: true }),
    importIo: importIo(program),
    state: moduleState(program),
    statefulTypes: classes.filter((c) => c.stateful).map((c) => c.name),
    abstractTypes: classes.filter((c) => c.abstract).length + (exportsOnlyTypes(program) ? 1 : 0),
    functions: functionFacts(file, program, known),
    references: moduleReferences(file, program, known),
  };
}

function parse(file, text, parsers) {
  if (/\.(ts|mts|cts|tsx)$/.test(file)) return parsers.typescript.parse(text, { loc: true, jsx: file.endsWith('x') });
  const options = { ecmaVersion: 'latest', loc: true, ecmaFeatures: { jsx: file.endsWith('x') } };
  try {
    return parsers.espree.parse(text, { ...options, sourceType: 'module' });
  } catch {
    return parsers.espree.parse(text, { ...options, sourceType: 'script' });
  }
}

// Every file's facts (a file that does not parse is left out, and said in `unparsed`).
function measureFacts(files, texts) {
  const parsers = { espree: require('espree'), typescript: require('@typescript-eslint/typescript-estree') };
  const facts = [];
  const unparsed = [];
  const known = new Set(files);
  for (const file of files) {
    try {
      facts.push(factsOf(file, parse(file, texts.get(file), parsers), known));
    } catch (error) {
      unparsed.push({ file, why: String(error.message || error).split('\n')[0] });
    }
  }
  return { facts, unparsed };
}

module.exports = { measureFacts, factsOf, effectOf };
