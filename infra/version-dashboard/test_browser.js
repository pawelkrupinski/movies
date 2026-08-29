#!/usr/bin/env node
/**
 * The bulk run, driven in a real browser.
 *
 * WHY THIS EXISTS SEPARATELY FROM test_app.py. Everything the Python tests can reach is markup:
 * that the button is rendered, that it carries the right count, that the server refuses a second
 * job on one machine. The part most likely to be WRONG is none of those -- it is the queue in
 * bringAllToLatest: whether it really stops taking new machines after a failure, whether the
 * production database's typed confirmation is actually carried into the switch it authorises, and
 * whether a machine that has switched stops being counted. None of that is observable without a
 * DOM, a real fetch, and a real click.
 *
 * The page under test is the REAL one -- rendered by app.py's own `render`, through the fixtures
 * test_app.py already defines -- served by a stub of the two endpoints the browser talks to. So
 * what is faked here is the fleet, never the page or the script driving it.
 *
 * Run:  node infra/version-dashboard/test_browser.js
 * Skips (exit 0) when Playwright's browsers are not installed, which is the CI-on-a-fork case.
 */
'use strict';
const http = require('http');
const path = require('path');
const { execFileSync } = require('child_process');

const HERE = __dirname;
const REPO = path.resolve(HERE, '../..');

// Playwright lives with the page-test suite, and its node_modules is not checked in -- so a fresh
// clone, and every git worktree, has none. Look where it usually is, honour an override for the
// case where it is somewhere else, and SKIP rather than fail when it is nowhere: this file is one
// step of infra/bin/check, which has to stay runnable offline on a fork.
let chromium;
const candidates = [
  process.env.KINOWO_PLAYWRIGHT,
  path.join(REPO, 'page-tests-playwright/node_modules/playwright'),
  'playwright',
].filter(Boolean);
for (const where of candidates) {
  try { ({ chromium } = require(where)); break; } catch (err) { /* try the next */ }
}
if (!chromium) {
  console.log('skip: playwright is not installed here (set KINOWO_PLAYWRIGHT to its path)');
  process.exit(0);
}

// ---------------------------------------------------------------------------------------------
// The page under test, rendered by app.py itself through test_app.py's fixtures.
// ---------------------------------------------------------------------------------------------
function renderPage(machines) {
  const py = `
import json, sys
sys.path.insert(0, ${JSON.stringify(HERE)})
import test_app as t
rows = [t.machine(**m) for m in json.loads(sys.argv[1])]
sys.stdout.write(t.page(*rows))
`;
  return execFileSync('python3', ['-c', py, JSON.stringify(machines)], { encoding: 'utf8' });
}

// ---------------------------------------------------------------------------------------------
// A stub fleet. `plan[name]` says what that machine's check and switch do.
// ---------------------------------------------------------------------------------------------
function startServer(htmlText, plan, posts) {
  const jobs = new Map();
  let seq = 0;
  const srv = http.createServer((req, res) => {
    const url = new URL(req.url, 'http://x');
    const json = (obj) => {
      res.writeHead(200, { 'Content-Type': 'application/json' });
      res.end(JSON.stringify(obj));
    };
    if (url.pathname === '/nixos') {
      res.writeHead(200, { 'Content-Type': 'text/html; charset=utf-8' });
      res.end(htmlText);
      return;
    }
    if (url.pathname === '/fleet-apply' && req.method === 'POST') {
      let body = '';
      req.on('data', (c) => { body += c; });
      req.on('end', () => {
        const b = JSON.parse(body || '{}');
        posts.push(b);
        const spec = (plan[b.machine] || {})[b.phase] || {};
        const id = 'j' + (++seq);
        jobs.set(id, Object.assign({ exit: 0, lines: ['· ' + b.phase + ' ' + b.machine] }, spec));
        json({ job: id, phase: b.phase });
      });
      return;
    }
    if (url.pathname === '/fleet-apply/log') {
      const job = jobs.get(url.searchParams.get('job'));
      if (!job) { json({ error: 'no such job' }); return; }
      const from = parseInt(url.searchParams.get('from') || '0', 10);
      json({ lines: job.lines.slice(from), done: true, exit: job.exit,
             can_switch: job.can_switch || null, result: job.result || null });
      return;
    }
    if (url.pathname === '/fleet-apply/machine') {
      const name = url.searchParams.get('machine');
      const spec = (plan[name] || {}).reading || {};
      json(Object.assign({ rows: `<tr class='ok'><td class='name'>${name}</td><td>x</td>` +
                                 `<td>x</td><td>x</td><td>x</td><td>x</td><td>switched</td></tr>`,
                           store_hash: 'ffffffffffff' }, spec));
      return;
    }
    res.writeHead(404); res.end('nope');
  });
  return new Promise((ok) => srv.listen(0, '127.0.0.1', () => ok(srv)));
}

const CLOSURE = (host) => '/nix/store/' + 'f'.repeat(32) + '-nixos-system-' + host;

// ---------------------------------------------------------------------------------------------
let failures = 0;
function check(label, cond, detail) {
  if (cond) { console.log('  ok  ' + label); return; }
  failures++;
  console.log('  FAILED ' + label + (detail ? '\n         ' + detail : ''));
}

async function withPage(machines, plan, body) {
  const posts = [];
  const srv = await startServer(renderPage(machines), plan, posts);
  const base = 'http://127.0.0.1:' + srv.address().port;
  const browser = await chromium.launch();
  const page = await browser.newPage();
  const errors = [];
  page.on('pageerror', (e) => errors.push(String(e)));
  await page.goto(base + '/nixos');
  try {
    await body(page, posts, errors);
  } finally {
    await browser.close();
    srv.close();
  }
  return errors;
}

async function testBulkRunSwitchesEveryMachine() {
  console.log('bulk run: every staged machine is checked, then switched');
  const machines = [
    { name: 'k3s-worker-1', role: 'k3s-worker', public: '1.1.1.1', actionable: true },
    { name: 'monitoring-1', role: 'monitoring', public: '2.2.2.2', actionable: true },
  ];
  const plan = {};
  for (const m of machines) {
    plan[m.name] = {
      check: { exit: 0, can_switch: CLOSURE(m.name) },
      switch: { exit: 0, result: 'DONE' },
    };
  }
  const errs = await withPage(machines, plan, async (page, posts) => {
    check('the button counts both machines',
      (await page.textContent('#fleetbulkbtn')).includes('(2)'),
      await page.textContent('#fleetbulkbtn'));
    page.on('dialog', (d) => d.accept());
    await page.click('#fleetbulkbtn');
    await page.waitForFunction(
      () => /done — 2 machine\(s\) switched/.test(
        document.querySelector('#fleetbulkcons .consfoot').textContent), null, { timeout: 15000 });
    const phases = posts.map((p) => p.machine + ':' + p.phase).sort();
    check('both machines were checked and switched',
      JSON.stringify(phases) === JSON.stringify(
        ['k3s-worker-1:check', 'k3s-worker-1:switch',
         'monitoring-1:check', 'monitoring-1:switch']), JSON.stringify(phases));
    await page.waitForFunction(
      () => document.querySelector('#fleetbulkbtn').textContent.includes('(0)'),
      null, { timeout: 15000 });
    check('the count falls to zero once both rows confirm', true);
  });
  check('no uncaught JS errors', errs.length === 0, errs.join('; '));
}

async function testProductionDatabaseNeedsItsNameTyped() {
  console.log('bulk run: the production database carries its typed confirmation');
  const machines = [
    { name: 'mongo-1', role: 'mongo', public: '1.1.1.1', actionable: true },
    { name: 'monitoring-1', role: 'monitoring', public: '2.2.2.2', actionable: true },
  ];
  const plan = {
    'mongo-1': { check: { exit: 0, can_switch: CLOSURE('mongo-1') },
                 switch: { exit: 0, result: 'DONE' } },
    'monitoring-1': { check: { exit: 0, can_switch: CLOSURE('monitoring-1') },
                      switch: { exit: 0, result: 'DONE' } },
  };
  await withPage(machines, plan, async (page, posts) => {
    page.on('dialog', (d) => (d.type() === 'prompt' ? d.accept('mongo-1') : d.accept()));
    await page.click('#fleetbulkbtn');
    await page.waitForFunction(
      () => /done — 2 machine\(s\) switched/.test(
        document.querySelector('#fleetbulkcons .consfoot').textContent), null, { timeout: 15000 });
    const dbSwitch = posts.find((p) => p.machine === 'mongo-1' && p.phase === 'switch');
    check('the database switch carries the typed name', dbSwitch && dbSwitch.confirm === 'mongo-1',
      JSON.stringify(dbSwitch));
    const other = posts.find((p) => p.machine === 'monitoring-1' && p.phase === 'switch');
    check('an ordinary machine carries no confirmation', other && !other.confirm,
      JSON.stringify(other));
  });
}

async function testDecliningTheDatabaseSkipsOnlyIt() {
  console.log('bulk run: declining the database excludes it, and does not abandon the run');
  const machines = [
    { name: 'mongo-1', role: 'mongo', public: '1.1.1.1', actionable: true },
    { name: 'monitoring-1', role: 'monitoring', public: '2.2.2.2', actionable: true },
  ];
  const plan = {
    'monitoring-1': { check: { exit: 0, can_switch: CLOSURE('monitoring-1') },
                      switch: { exit: 0, result: 'DONE' } },
  };
  await withPage(machines, plan, async (page, posts) => {
    page.on('dialog', (d) => (d.type() === 'prompt' ? d.dismiss() : d.accept()));
    await page.click('#fleetbulkbtn');
    await page.waitForFunction(
      () => /done — 1 machine\(s\) switched/.test(
        document.querySelector('#fleetbulkcons .consfoot').textContent), null, { timeout: 15000 });
    check('the database was never posted at all',
      !posts.some((p) => p.machine === 'mongo-1'), JSON.stringify(posts));
    check('the console says which machine was skipped',
      (await page.textContent('#fleetbulkcons .out'))
        .includes('skipping (confirmation declined): mongo-1'),
      await page.textContent('#fleetbulkcons .out'));
  });
}

async function testAFailedCheckStopsNewMachinesStarting() {
  console.log('bulk run: nothing NEW starts once a check fails');
  // MORE MACHINES THAN THREADS, deliberately: with six workers and three hosts every machine
  // starts at once and there is no queue left for a stop rule to act on. The rule only means
  // anything when there is something waiting behind the failure.
  const machines = [];
  for (let i = 1; i <= 10; i++) {
    machines.push({ name: 'h' + i, role: 'k3s-worker', public: '10.0.0.' + i, actionable: true });
  }
  const plan = {};
  for (const m of machines) {
    plan[m.name] = { check: { exit: m.name === 'h1' ? 7 : 0, can_switch: CLOSURE(m.name) },
                     switch: { exit: 0, result: 'DONE' } };
  }
  await withPage(machines, plan, async (page, posts) => {
    page.on('dialog', (d) => d.accept());
    await page.click('#fleetbulkbtn');
    await page.waitForFunction(
      () => /^stopped after /.test(
        document.querySelector('#fleetbulkcons .consfoot').textContent), null, { timeout: 20000 });
    const checked = new Set(posts.filter((p) => p.phase === 'check').map((p) => p.machine));
    check('the run stopped well short of all ten machines', checked.size < 10,
      'checked ' + checked.size);
    check('the failure names the machine it stopped on',
      (await page.textContent('#fleetbulkcons .consfoot')).includes('h1'),
      await page.textContent('#fleetbulkcons .consfoot'));
    check('h1 was never switched after its check failed',
      !posts.some((p) => p.machine === 'h1' && p.phase === 'switch'));
  });
}

async function testSingleMachineButton() {
  console.log('single machine: Bring to latest… checks, then offers the switch');
  const machines = [{ name: 'monitoring-1', role: 'monitoring', public: '2.2.2.2',
                      actionable: true }];
  const plan = { 'monitoring-1': { check: { exit: 0, can_switch: CLOSURE('monitoring-1') },
                                   switch: { exit: 0, result: 'DONE' } } };
  await withPage(machines, plan, async (page, posts) => {
    await page.click('.actioncell .actionrow .applybtn');
    // THE SWITCH BUTTON MUST NOT EXIST BEFORE THE DRY RUN HAS SPOKEN.
    await page.waitForSelector('.consfoot .applybtn.go', { timeout: 15000 });
    check('the check ran first, and only the check',
      posts.length === 1 && posts[0].phase === 'check', JSON.stringify(posts));
    page.on('dialog', (d) => d.accept());
    await page.click('.consfoot .applybtn.go');
    await page.waitForFunction(
      () => /the row above now reflects/.test(
        document.querySelector('.actioncell .consfoot').textContent), null, { timeout: 15000 });
    check('the switch followed', posts.length === 2 && posts[1].phase === 'switch',
      JSON.stringify(posts));
    check('the button is retired rather than removed',
      (await page.textContent('.actioncell .actionrow')).includes('reload the page'));
  });
}

(async () => {
  await testBulkRunSwitchesEveryMachine();
  await testProductionDatabaseNeedsItsNameTyped();
  await testDecliningTheDatabaseSkipsOnlyIt();
  await testAFailedCheckStopsNewMachinesStarting();
  await testSingleMachineButton();
  if (failures) {
    console.log('\n' + failures + ' browser check(s) FAILED');
    process.exit(1);
  }
  console.log('\nall browser checks passed');
})().catch((err) => { console.error(err); process.exit(1); });
