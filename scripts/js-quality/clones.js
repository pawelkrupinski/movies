// CLONE GROUPS of 50+ tokens, found by jscpd over every measured file at once (so a copy across two areas is found
// too). jscpd reports PAIRS of copies; a group is every copy linked through pairs sharing one fragment, as PMD CPD
// (the Scala measure, quality/Clones.scala) reports a group of N copies once. Import and require lines are skipped
// (`--ignore-pattern`), as CPD reads Scala with its imports blanked: two modules importing alike are no clone.
'use strict';

const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const { execFileSync } = require('node:child_process');

const MinTokens = 50;

const ImportPatterns = [
  String.raw`import\s[^;]*?from\s*['"][^'"]+['"];?`,
  String.raw`import\s*['"][^'"]+['"];?`,
  String.raw`(const|let|var)\s+[\w{}\s,:]+=\s*require\(['"][^'"]+['"]\)[\w.]*;?`,
];

function runJscpd(root, files, jscpd) {
  const out = fs.mkdtempSync(path.join(os.tmpdir(), 'js-quality-cpd-'));
  try {
    execFileSync(jscpd, [
      '--min-tokens', String(MinTokens), '--min-lines', '1', '--mode', 'mild', '--absolute', '--silent',
      '--format', 'javascript,typescript', '--ignore-pattern', ImportPatterns.join(','),
      '--reporters', 'json', '--output', out, ...files.map((file) => path.join(root, file)),
    ], { cwd: root, stdio: ['ignore', 'ignore', 'pipe'] });
    return JSON.parse(fs.readFileSync(path.join(out, 'jscpd-report.json'), 'utf8')).duplicates || [];
  } finally {
    fs.rmSync(out, { recursive: true, force: true });
  }
}

function copyOf(root, side) {
  return { file: path.relative(root, side.name), start: side.startLoc.line, end: side.endLoc.line };
}

// Union-find over the copies: a pair joins the groups of its two fragments.
function groupPairs(pairs) {
  const parent = new Map();
  const find = (key) => {
    while (parent.get(key) !== key) key = parent.get(key);
    return key;
  };
  const copies = new Map();
  const tokens = new Map();
  for (const pair of pairs) {
    const keys = pair.copies.map((copy) => `${copy.file}:${copy.start}-${copy.end}`);
    keys.forEach((key, index) => {
      if (!parent.has(key)) parent.set(key, key);
      copies.set(key, pair.copies[index]);
    });
    const [a, b] = keys.map(find);
    if (a !== b) parent.set(b, a);
    tokens.set(keys[0], Math.max(tokens.get(keys[0]) || 0, pair.tokens));
  }
  const groups = new Map();
  for (const key of parent.keys()) {
    const top = find(key);
    const group = groups.get(top) || { tokens: 0, copies: [] };
    group.copies.push(copies.get(key));
    group.tokens = Math.max(group.tokens, tokens.get(key) || 0);
    groups.set(top, group);
  }
  return [...groups.values()].map((group) => ({
    ...group,
    lines: Math.max(...group.copies.map((copy) => copy.end - copy.start + 1)),
    copies: group.copies.sort((x, y) => x.file.localeCompare(y.file) || x.start - y.start),
  }));
}

function findClones(root, files, jscpd) {
  if (files.length < 1) return [];
  const pairs = runJscpd(root, files, jscpd).map((duplicate) => ({
    tokens: duplicate.tokens,
    copies: [copyOf(root, duplicate.firstFile), copyOf(root, duplicate.secondFile)],
  }));
  return groupPairs(pairs).sort((x, y) => y.tokens - x.tokens);
}

module.exports = { findClones, groupPairs, MinTokens };
