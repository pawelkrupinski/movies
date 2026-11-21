// MODULE COUPLING: a module (one file) stands for the Scala class, its directory for the package.
//
// Two graphs, by how an area's files reach each other:
//  - IMPORTS (ES modules, CommonJS, TypeScript): dependency-cruiser resolves every `import` / `require` / `export
//    ... from`; an edge is a resolved local file (packages from node_modules and node's core modules are not
//    counted);
//  - GLOBALS (classic browser scripts sharing one global scope, web/src/main/assets/js): no imports exist, so each
//    file's `/* global a, b */` lines are its import list (ESLint's no-undef / no-redeclare keep them true) and a
//    name is resolved to the file that offers it -- its `/* exported ... */` line, or a top-level `function` of that
//    name. A name no file offers comes from the templates (`_sharedJsConfig`, the inline page scripts) and is counted
//    apart.
// Fan-out counts every module a module uses: JS has no data/collaborator split to read (the Scala fan-out counts
// collaborators only). Abstractness: a TypeScript module that exports only types and interfaces is abstract; plain
// JS has none, so a JS directory's distance is |I - 1|.
// A directory isolated from every other (Ca + Ce = 0) has no distance.
'use strict';

const path = require('node:path');
const { execFileSync } = require('node:child_process');

// Strongly connected components of size > 1 (or a self-loop), by Tarjan's algorithm.
function cycles(nodes, edges) {
  let index = 0;
  const indices = new Map();
  const low = new Map();
  const onStack = new Set();
  const stack = [];
  const found = [];
  const visit = (node) => {
    indices.set(node, index);
    low.set(node, index);
    index += 1;
    stack.push(node);
    onStack.add(node);
    for (const next of edges.get(node) || []) {
      if (!indices.has(next)) {
        visit(next);
        low.set(node, Math.min(low.get(node), low.get(next)));
      } else if (onStack.has(next)) {
        low.set(node, Math.min(low.get(node), indices.get(next)));
      }
    }
    if (low.get(node) !== indices.get(node)) return;
    const component = [];
    let member;
    do {
      member = stack.pop();
      onStack.delete(member);
      component.push(member);
    } while (member !== node);
    if (component.length > 1 || (edges.get(node) || new Set()).has(node)) found.push(component.sort());
  };
  for (const node of nodes) if (!indices.has(node)) visit(node);
  return found;
}

function importEdges(root, files, depcruise) {
  if (files.length === 0) return new Map();
  const output = execFileSync(depcruise, [
    '--no-config', '--output-type', 'json', '--ts-pre-compilation-deps',
    '--do-not-follow', 'node_modules', '--exclude', 'node_modules', ...files,
  ], { cwd: root, maxBuffer: 256 * 1024 * 1024, stdio: ['ignore', 'pipe', 'pipe'] }).toString();
  const wanted = new Set(files);
  const edges = new Map(files.map((file) => [file, new Set()]));
  for (const module of JSON.parse(output).modules) {
    if (!wanted.has(module.source)) continue;
    for (const dependency of module.dependencies) {
      if (wanted.has(dependency.resolved) && dependency.resolved !== module.source) edges.get(module.source).add(dependency.resolved);
    }
  }
  return edges;
}

const namesIn = (text, kind) => {
  const names = [];
  for (const match of text.matchAll(new RegExp(String.raw`/\*\s*${kind}\s+([\s\S]*?)\*/`, 'g'))) {
    names.push(...match[1].split(/[\s,]+/).map((name) => name.split(':')[0]).filter(Boolean));
  }
  return names;
};

// The globals graph of `files`, each name read resolved against what every file in `providers` offers.
function globalEdges(files, providers, texts) {
  const offeredBy = new Map();
  for (const file of providers) {
    const text = texts.get(file);
    const offered = [...namesIn(text, 'exported'), ...[...text.matchAll(/^function\s+(\w+)/gm)].map((match) => match[1])];
    for (const name of offered) if (!offeredBy.has(name)) offeredBy.set(name, file);
  }
  const edges = new Map();
  const templateNames = new Map();
  for (const file of files) {
    const used = new Set();
    const fromTemplates = new Set();
    for (const name of namesIn(texts.get(file), 'global')) {
      const provider = offeredBy.get(name);
      if (provider && provider !== file) used.add(provider);
      else if (!provider) fromTemplates.add(name);
    }
    edges.set(file, used);
    templateNames.set(file, fromTemplates.size);
  }
  return { edges, templateNames };
}

const exportsOnlyTypes = (text) => {
  const exports = text.match(/^\s*export\s+(?:declare\s+)?(\w+)/gm) || [];
  return exports.length > 0 && exports.every((line) => /export\s+(?:declare\s+)?(type|interface)\b/.test(line));
};

// Each directory's Ca, Ce, I, A and D (Martin's metrics, a module counted for a class), for the modules in `edges`.
function packages(edges, texts) {
  const dirOf = (file) => path.dirname(file);
  const byDir = new Map();
  for (const file of edges.keys()) byDir.set(dirOf(file), [...(byDir.get(dirOf(file)) || []), file]);
  const result = [];
  for (const [dir, members] of byDir) {
    const inside = new Set(members);
    const efferent = new Set();
    const afferent = new Set();
    for (const [from, targets] of edges) {
      for (const to of targets) {
        if (inside.has(from) && !inside.has(to)) efferent.add(to);
        if (!inside.has(from) && inside.has(to)) afferent.add(from);
      }
    }
    const ce = efferent.size;
    const ca = afferent.size;
    // A directory no other uses and that uses none (Ca + Ce = 0) has no instability: its distance is not measured.
    const instability = ca + ce === 0 ? null : ce / (ca + ce);
    const abstractness = members.filter((file) => /\.(ts|mts|cts)$/.test(file) && exportsOnlyTypes(texts.get(file))).length / members.length;
    const distance = instability === null ? null : Math.abs(abstractness + instability - 1);
    result.push({ dir, modules: members.length, ca, ce, instability, abstractness, distance });
  }
  return result;
}

function packageEdges(edges) {
  const result = new Map();
  for (const [from, targets] of edges) {
    for (const to of targets) {
      const [a, b] = [path.dirname(from), path.dirname(to)];
      if (a === b) continue;
      if (!result.has(a)) result.set(a, new Set());
      result.get(a).add(b);
    }
  }
  return result;
}

// The coupling of one area: its modules' fan-out, the module cycles, the directory cycles and the directories' metrics.
function measureCoupling(edges, texts, templateNames = new Map()) {
  const modules = [...edges.keys()].map((file) => ({ file, fanOut: edges.get(file).size, fromTemplates: templateNames.get(file) || 0 }));
  const directoryEdges = packageEdges(edges);
  const directories = new Set([...edges.keys()].map((file) => path.dirname(file)));
  return {
    modules,
    moduleCycles: cycles([...edges.keys()], edges),
    packageCycles: cycles([...directories], directoryEdges),
    packages: packages(edges, texts),
  };
}

module.exports = { cycles, importEdges, globalEdges, measureCoupling };
