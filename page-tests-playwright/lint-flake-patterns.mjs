#!/usr/bin/env node
// Static lint for the Playwright page specs: the shapes that made them time out on a loaded CI host.
//
// Every page-test failure that blocked Main between 2026-10-20 and 10-23 was a 30s test timeout at ci-runner load
// 100–180, where one load of a city listing took 6–13s and a click 1.5–3s; no step ever hung on a wrong state. The
// tests did too much in their budget, or waited on wall time. Retrying them is not an option (NoTestRerunSpec), so
// these shapes are refused before they land:
//
//   wait-for-timeout   `page.waitForTimeout(n)`: a blind sleep. Too short under load, wasted without it. Wait on the
//                      state the sleep stood in for (a poll, an event, an in-page flag).
//   clear-then-reload  `clearLocalStorage(page)` then `reload(page)`: every test gets a fresh browser context, so its
//                      first load already has nothing stored. The reload is a whole extra page load per test (eight
//                      app-banner tests and three swipe-hint tests timed out on it).
//   load-budget        more than MAX_LOADS page loads in one test, counting its beforeEach's: at ~10s a load, four do
//                      not fit 30s. Split the test, or use a lighter page.
//   widened-timeout    `{ timeout: n }` on an assertion: a longer wait hides the dependency instead of removing it.
//
// A line can opt out with a justification on it or the line above: `// flake-lint: allow <rule> — <why>`.
//
//   node page-tests-playwright/lint-flake-patterns.mjs [files...]   (default: every tests/*.spec.ts)
import { readFileSync, readdirSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

export const MAX_LOADS = 3;

const LOAD_CALL = /\b(?:page\.(?:goto|reload|goBack|goForward)|reload|gotoAndWaitForCards|visit|\w*(?:Reload|Goto|Navigate)\w*)\s*\(/g;

function allowed(lines, i, rule) {
  const re = new RegExp(`flake-lint:\\s*allow\\s+${rule}\\b`);
  return re.test(lines[i]) || (i > 0 && re.test(lines[i - 1]));
}

// The body of the call starting at `start` (index of its opening paren's line), by brace depth.
function blockEnd(lines, start) {
  let depth = 0;
  let seen = false;
  for (let i = start; i < lines.length; i++) {
    for (const ch of lines[i].replace(/\/\/.*$/, '')) {
      if (ch === '{') { depth++; seen = true; }
      if (ch === '}') depth--;
    }
    if (seen && depth <= 0) return i;
  }
  return lines.length - 1;
}

function countLoads(lines, from, to) {
  let n = 0;
  for (let i = from; i <= to; i++) {
    const code = lines[i].replace(/\/\/.*$/, '');
    if (/^\s*(?:async\s+)?function\b|^\s*const\s+\w+\s*=\s*(?:async\s*)?\(/.test(code)) continue; // a definition
    n += (code.match(LOAD_CALL) || []).length;
  }
  return n;
}

export function lint(source, file = '<source>') {
  const lines = source.split('\n');
  const problems = [];
  const report = (i, rule, message) => {
    if (!allowed(lines, i, rule)) problems.push({ file, line: i + 1, rule, message });
  };

  lines.forEach((line, i) => {
    const code = line.replace(/\/\/.*$/, '');
    if (/\.waitForTimeout\s*\(/.test(code)) {
      report(i, 'wait-for-timeout', 'blind sleep: wait on the page state it stands in for');
    }
    if (/\bclearLocalStorage\s*\(/.test(code)) {
      const next = lines.slice(i + 1).find((l) => l.trim() !== '' && !l.trim().startsWith('//')) ?? '';
      if (/\breload\s*\(|page\.reload\s*\(/.test(next)) {
        report(i, 'clear-then-reload', 'a fresh context has nothing stored: load once, after the setup, instead');
      }
    }
    if (/\b(?:toBe\w*|toHave\w*|toContain\w*|toMatch\w*|poll)\s*\(.*\{\s*timeout\s*:/.test(code) ||
        /\.(?:toBe\w*|toHave\w*|toContain\w*)\(\s*\{\s*timeout\s*:/.test(code)) {
      report(i, 'widened-timeout', 'a widened assertion timeout: wait on the state, not longer');
    }
  });

  // Load budget, per describe: beforeEach loads count toward each of its tests.
  let hookLoads = 0;
  lines.forEach((line, i) => {
    if (/^\s*test\.describe\s*\(/.test(line)) hookLoads = 0;
    if (/^\s*test\.beforeEach\s*\(/.test(line)) {
      hookLoads = countLoads(lines, i, blockEnd(lines, i));
    }
    if (/^\s*test\s*\(/.test(line)) {
      const loads = hookLoads + countLoads(lines, i, blockEnd(lines, i));
      if (loads > MAX_LOADS) {
        report(i, 'load-budget', `${loads} page loads (beforeEach included) in a 30s test; at most ${MAX_LOADS} fit a loaded CI host`);
      }
    }
  });
  return problems;
}

const isMain = process.argv[1] && fileURLToPath(import.meta.url) === process.argv[1];
if (isMain) {
  const testsDir = join(dirname(fileURLToPath(import.meta.url)), 'tests');
  const files = process.argv.slice(2).length > 0
    ? process.argv.slice(2)
    : readdirSync(testsDir).filter((f) => f.endsWith('.spec.ts')).map((f) => join(testsDir, f));
  const problems = files.flatMap((f) => lint(readFileSync(f, 'utf8'), f));
  for (const p of problems) console.error(`${p.file}:${p.line}  ${p.rule}  ${p.message}`);
  if (problems.length > 0) {
    console.error(`\n${problems.length} page-spec flake pattern(s); see page-tests-playwright/lint-flake-patterns.mjs`);
    process.exit(1);
  }
}
