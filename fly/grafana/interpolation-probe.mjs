// Reads the PromQL Grafana actually sends for the two memory panels' host-free
// series, per country, by driving the real dashboard in headless Chromium.
//
// Why this exists as a separate probe: `local-harness.sh` can assert what a
// variable's DEFINITION resolves to (a label_values call through the datasource
// proxy), and `smoke-test.sh` can assert what the dashboard JSON SAYS — but
// neither can see Grafana's frontend interpolation, which is the step that turns
// `{app=~"$worker_host_app"}` with All selected into a concrete app list. That
// step is exactly where a scoping bug hides: if All degraded to `.*`, both
// panels would silently plot every Fly app while every assertion stayed green.
//
// The host series are the only ones that need this. Every other country-scoped
// target on the dashboard is scoped by an `and on(app)` data join, which is
// visible to the harness; fly_instance_* lives on a datasource no join can
// reach, so a derived variable is the only lever and interpolation is load-
// bearing. Verified 2026-07-18 on the shipped image (Grafana 11.4.0): All
// expands to the country's option list, single or piped, never `.*`.
//
// Usage:  fly/grafana/local-harness.sh          # boot the seeded stack first
//         node fly/grafana/interpolation-probe.mjs [http://localhost:3998]
//
// Playwright comes from the page-tests-playwright workspace rather than a
// dependency of its own — this probe is run by hand after a dashboard edit, not
// in CI, so it borrows the browser that workspace already installs.
import { createRequire } from 'node:module';

const BASE = process.argv[2] || 'http://localhost:3998';
const PLAYWRIGHT = new URL('../../page-tests-playwright/', import.meta.url).pathname;

let chromium;
try {
  chromium = createRequire(PLAYWRIGHT + 'package.json')('@playwright/test').chromium;
} catch {
  console.error(`FAIL: @playwright/test not installed in ${PLAYWRIGHT}\n` +
                '      run `npm ci` there, or `npx playwright install chromium`.');
  process.exit(1);
}

// (label, url-suffix, expected interpolated expr). The worker/web pair per
// country is the contract: each panel's host line must name that country's app
// and no other — and the two must never name the SAME app, which would mean one
// panel is quietly plotting the other's machine.
const HOST = (app) => `fly_instance_memory_mem_available{app=~"${app}"}`;
const CASES = [
  ['country=pl  worker', 'var-country=pl&viewPanel=panel-43', HOST('kinowo-worker')],
  ['country=pl  web', 'var-country=pl&viewPanel=panel-44', HOST('kinowo')],
  ['country=uk  worker', 'var-country=uk&viewPanel=panel-43', HOST('kinowo-worker-uk')],
  ['country=uk  web', 'var-country=uk&viewPanel=panel-44', HOST('showtimes-uk')],
  ['country=de  worker', 'var-country=de&viewPanel=panel-43', HOST('kinowo-worker-de')],
  ['country=de  web', 'var-country=de&viewPanel=panel-44', HOST('showtimes-de')],
  // Multi-select: $country interpolates to a regex alternation, so the derived
  // variable must too. A variable that mishandled it would drop a country's host
  // line silently rather than error.
  ['country=pl+uk worker', 'var-country=pl&var-country=uk&viewPanel=panel-43',
    HOST('(kinowo-worker|kinowo-worker-uk)')],
];

const browser = await chromium.launch();
const page = await browser.newPage();
let hostExprs = [];
page.on('request', (r) => {
  if (!r.url().includes('/api/ds/query')) return;
  try {
    for (const q of JSON.parse(r.postData() || '{}').queries || []) {
      if (q.expr?.includes('fly_instance_memory_mem_available')) hostExprs.push(q.expr);
    }
  } catch { /* non-JSON body: not a query we care about */ }
});

await page.goto(`${BASE}/login`, { waitUntil: 'networkidle' });
await page.fill('input[name="user"]', 'admin');
await page.fill('input[name="password"]', 'admin');
await page.click('button[type="submit"]');
await page.waitForTimeout(2500);

let fail = 0;
for (const [label, suffix, expected] of CASES) {
  hostExprs = [];
  await page.goto(`${BASE}/d/fly-overview?${suffix}&from=now-2h&to=now`, { waitUntil: 'networkidle' });
  await page.waitForTimeout(3500);
  const got = [...new Set(hostExprs)];
  if (got.length === 1 && got[0] === expected) {
    console.log(`PASS: ${label} → ${got[0]}`);
  } else {
    console.log(`FAIL: ${label} → ${JSON.stringify(got)}, expected [${expected}]`);
    fail = 1;
  }
}

// A KNOWN, ACCEPTED residual, asserted so it stays known: Grafana lets a `var-`
// URL parameter override a derived variable, so a hand-written link CAN still
// pin one country's host line onto another country's panel. Hiding the variable
// (hide: 2) removes the dropdown that made the old $worker_app leak routine —
// Grafana writes `$__all` back into the URL, so bookmarks and shares re-derive —
// but it cannot refuse the parameter. The {{app}} legend is the visible tell.
// If a future Grafana ignores the override, this flips to FAIL: delete it and
// upgrade the comment, don't silently relax it.
hostExprs = [];
await page.goto(`${BASE}/d/fly-overview?var-country=uk&var-worker_host_app=kinowo-worker-de&viewPanel=panel-43&from=now-2h&to=now`,
  { waitUntil: 'networkidle' });
await page.waitForTimeout(3500);
const pinned = [...new Set(hostExprs)];
if (pinned.length === 1 && pinned[0] === HOST('kinowo-worker-de')) {
  console.log('PASS: hand-pinned var- URL param still overrides the derived variable (known residual)');
} else {
  console.log(`FAIL: pinned-URL behaviour changed → ${JSON.stringify(pinned)} — re-read the comment above`);
  fail = 1;
}

await browser.close();
console.log(fail ? '== INTERPOLATION PROBE FAILED ==' : '== INTERPOLATION PROBE PASSED ==');
process.exit(fail);
