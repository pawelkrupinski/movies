#!/usr/bin/env node
// Per-device screenshots of `/` on six phones spanning the size
// spectrum (small Android → large iPhone), in both portrait and
// landscape, then opens the 12 files in Preview.app.
//
// "Two screen heights" — the browser viewport stays at the device's
// actual dimensions so `@media` queries fire correctly, but the
// captured image extends 2 × past the viewport bottom via
// Playwright's `fullPage + clip` mode. Each shot shows the navbar
// + cards above the fold AND what's just below, in a single image
// — useful for spot-checking layout transitions without scrolling
// per shot.
//
// Boots a local `FixtureServerMain` so the screenshots reflect
// the committed CSS (not whatever's deployed on kinowo.fly.dev).
// Override with `KINOWO_BASE_URL=…` to point at prod or any other
// URL. Output dir defaults to `/tmp/phone-screens`, override with
// `OUT_DIR=…`.

import { chromium, webkit, devices } from '../page-tests-playwright/node_modules/playwright/index.mjs';
import { spawn, execSync } from 'node:child_process';
import { existsSync, mkdirSync, readFileSync, unlinkSync } from 'node:fs';
import { dirname, resolve as resolvePath, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT      = resolvePath(__dirname, '..');
const OUT_DIR   = process.env.OUT_DIR || '/tmp/phone-screens';

// Six phones across the modern phone band. Widths span 360 → 440 px
// — Galaxy S10 / A50 sits at the narrow floor; iPhone 17 Pro Max at
// the wide ceiling. iPhone heights are the post-safe-area effective
// viewport (Dynamic Island + home indicator subtracted), matching
// `page-tests-playwright/tests/navbar-overflow.spec.ts` so the
// shots reflect what real iOS Safari renders.
const PHONES = [
  { slug: 'galaxy-s10',          label: 'Galaxy S10 / A50',     width: 360, height: 760, engine: 'chromium', device: 'Pixel 7'   },
  { slug: 'iphone-se',           label: 'iPhone SE (3rd gen)',  width: 375, height: 667, engine: 'webkit',   device: 'iPhone 13' },
  { slug: 'iphone-13',           label: 'iPhone 13 / 14 / 15',  width: 390, height: 760, engine: 'webkit',   device: 'iPhone 13' },
  { slug: 'iphone-17',           label: 'iPhone 17',            width: 393, height: 770, engine: 'webkit',   device: 'iPhone 13' },
  { slug: 'pixel-7',             label: 'Pixel 7 / 8',          width: 412, height: 915, engine: 'chromium', device: 'Pixel 7'   },
  { slug: 'iphone-17-pro-max',   label: 'iPhone 17 Pro Max',    width: 440, height: 870, engine: 'webkit',   device: 'iPhone 13' },
];

const engines = { chromium, webkit };

async function bootFixtureServer() {
  const portFile = join('/tmp', `kinowo-phone-screens-port-${Date.now()}.txt`);
  process.stdout.write(`Booting FixtureServerMain (port file ${portFile})… `);
  const proc = spawn('sbt', [`PageTest/runMain tools.FixtureServerMain ${portFile}`], {
    cwd: ROOT,
    stdio: ['ignore', 'ignore', 'pipe'],
  });
  for (let i = 0; i < 120; i++) {
    await new Promise(r => setTimeout(r, 1000));
    if (existsSync(portFile)) {
      const port = readFileSync(portFile, 'utf8').trim();
      if (port) {
        console.log(`up at http://127.0.0.1:${port}`);
        return { proc, port, portFile };
      }
    }
  }
  proc.kill();
  throw new Error('FixtureServerMain did not start within 120 s');
}

async function shootOne(baseUrl, phone, orientation) {
  const isPortrait = orientation === 'portrait';
  const viewportW  = isPortrait ? phone.width  : phone.height;
  const viewportH  = isPortrait ? phone.height : phone.width;
  const captureH   = viewportH * 2;

  const browser = await engines[phone.engine].launch();
  // Spread the device profile (UA, hasTouch, isMobile, DPR) so the
  // screenshot reflects what the device's Safari / Chrome actually
  // does — Mobile UA, touch handlers, scrollbar style. Override the
  // viewport with the phone's effective dimensions.
  const context = await browser.newContext({
    ...devices[phone.device],
    viewport: { width: viewportW, height: viewportH },
  });
  const page = await context.newPage();
  await page.goto(baseUrl, { waitUntil: 'domcontentloaded' });
  await page.waitForSelector('.col[data-title]', { state: 'attached', timeout: 30_000 });
  // applyFilters() runs on load, reorders the grid via
  // appendChild. Give it one settling frame so the screenshot
  // catches the final layout rather than a mid-shuffle frame.
  await page.waitForTimeout(500);

  const out = join(OUT_DIR, `${phone.slug}-${orientation}.png`);
  await page.screenshot({
    path: out,
    fullPage: true,
    clip: { x: 0, y: 0, width: viewportW, height: captureH },
  });
  console.log(`  ✔ ${phone.slug}-${orientation}.png  (${viewportW}×${captureH} CSS px)`);
  await browser.close();
}

// Main.
mkdirSync(OUT_DIR, { recursive: true });

let server = null;
let baseUrl = process.env.KINOWO_BASE_URL;
if (!baseUrl) {
  server = await bootFixtureServer();
  baseUrl = `http://127.0.0.1:${server.port}`;
}

try {
  for (const phone of PHONES) {
    console.log(`${phone.label}:`);
    await shootOne(baseUrl, phone, 'portrait');
    await shootOne(baseUrl, phone, 'landscape');
  }
} finally {
  if (server) {
    server.proc.kill();
    try { unlinkSync(server.portFile); } catch {}
  }
}

const files = PHONES.flatMap(p => [
  join(OUT_DIR, `${p.slug}-portrait.png`),
  join(OUT_DIR, `${p.slug}-landscape.png`),
]);
console.log(`\nOpening ${files.length} screenshots in Preview…`);
execSync(`open -a Preview ${files.map(f => `"${f}"`).join(' ')}`);
console.log('Done.');
