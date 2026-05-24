#!/usr/bin/env node
// Per-device screenshots of `/`, `/kina`, `/filmy`, and `/film` on
// phones spanning the size spectrum (small Android → large iPhone),
// in both portrait and landscape, on BOTH the native engine (Chromium
// for Android, WebKit for iPhone) AND Firefox.
//
// "Two screen heights" — the browser viewport stays at the device's
// actual dimensions so `@media` queries fire correctly, but the
// captured image extends 2 × past the viewport bottom via
// Playwright's `fullPage + clip` mode. Each shot shows the navbar
// + cards above the fold AND what's just below, in a single image
// — useful for spot-checking layout transitions without scrolling
// per shot.
//
// Android phones inject Roboto via Google Fonts so the rendering is
// deterministic regardless of the host's system font (Samsung OneUI
// uses One UI Sans, stock Android uses Roboto, macOS uses SF Pro).
//
// Boots a local `FixtureServerMain` so the screenshots reflect
// the committed CSS (not whatever's deployed on kinowo.fly.dev).
// Override with `KINOWO_BASE_URL=…` to point at prod or any other
// URL. Output dir defaults to `/tmp/phone-screens`, override with
// `OUT_DIR=…`.

import { chromium, webkit, firefox, devices } from '../page-tests-playwright/node_modules/playwright/index.mjs';
import { spawn, execSync } from 'node:child_process';
import { existsSync, mkdirSync, readFileSync, unlinkSync } from 'node:fs';
import { dirname, resolve as resolvePath, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT      = resolvePath(__dirname, '..');
const OUT_DIR   = process.env.OUT_DIR || '/tmp/phone-screens';

// Phones across the modern band. Widths span 360 → 440 px. Heights
// are the post-safe-area effective viewport for iPhones (Dynamic
// Island + home indicator subtracted). Each phone lists its native
// engine + device profile for Playwright emulation.
const PHONES = [
  // 3 Androids: narrowest → widest
  { slug: 'galaxy-s10',        label: 'Galaxy S10 / A50',       width: 360, height: 760, nativeEngine: 'chromium', device: 'Pixel 7',   android: true  },
  { slug: 'pixel-7',           label: 'Pixel 7 / 8',            width: 412, height: 915, nativeEngine: 'chromium', device: 'Pixel 7',   android: true  },
  { slug: 'pixel-9-pro',       label: 'Pixel 9 Pro',            width: 427, height: 952, nativeEngine: 'chromium', device: 'Pixel 7',   android: true  },
  // 3 iPhones: narrowest → widest
  { slug: 'iphone-se',         label: 'iPhone SE (3rd gen)',     width: 375, height: 667, nativeEngine: 'webkit',   device: 'iPhone 13', android: false },
  { slug: 'iphone-17',         label: 'iPhone 15 / 16 / 17',    width: 393, height: 770, nativeEngine: 'webkit',   device: 'iPhone 13', android: false },
  { slug: 'iphone-17-pro-max', label: 'iPhone 16/17 Pro Max',   width: 440, height: 870, nativeEngine: 'webkit',   device: 'iPhone 13', android: false },
  // 2 Windows Phones
  { slug: 'lumia-520',          label: 'Lumia 520',              width: 320, height: 533, nativeEngine: 'chromium', device: 'Pixel 7',   android: false },
  { slug: 'lumia-950',          label: 'Lumia 950',              width: 360, height: 640, nativeEngine: 'chromium', device: 'Pixel 7',   android: false },
];

// Every phone is shot on its native engine + Firefox.
const ENGINES = { chromium, webkit, firefox };

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

const ALL_PAGES = [
  { slug: 'index', path: '/',      selector: '.col[data-title]' },
  { slug: 'kina',  path: '/kina',  selector: '.cinema-section[data-cinema]' },
  { slug: 'filmy', path: '/filmy', selector: '#film-grid' },
  { slug: 'film',  path: null,     selector: '.poster-wrap' },
];

const requestedPage = process.argv[2];
if (requestedPage && !ALL_PAGES.some(p => p.slug === requestedPage)) {
  console.error(`Unknown page "${requestedPage}". Valid: ${ALL_PAGES.map(p => p.slug).join(', ')}`);
  process.exit(1);
}
const PAGES = requestedPage ? ALL_PAGES.filter(p => p.slug === requestedPage) : ALL_PAGES;

async function discoverFilmPath(baseUrl) {
  const browser = await chromium.launch();
  const context = await browser.newContext();
  const page    = await context.newPage();
  await page.goto(baseUrl, { waitUntil: 'domcontentloaded' });
  await page.waitForSelector('.col[data-title]', { state: 'attached', timeout: 30_000 });
  const title = await page.$eval('.col[data-title]', el => el.dataset.title);
  await browser.close();
  return `/film?title=${encodeURIComponent(title)}`;
}

/** Inject Roboto via Google Fonts so Android screenshots render with
 *  a known font regardless of the host OS. */
async function injectRoboto(page) {
  await page.evaluate(() => {
    const link = document.createElement('link');
    link.rel = 'stylesheet';
    link.href = 'https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap';
    document.head.appendChild(link);
    const style = document.createElement('style');
    style.textContent = '* { font-family: "Roboto", sans-serif !important; }';
    document.head.appendChild(style);
  });
  await page.waitForTimeout(1500);
}

// 150% display zoom → viewport shrinks to 2/3 of normal.
const ZOOM_LEVELS = [
  { tag: '',       scale: 1 },
  { tag: '-150pct', scale: 2 / 3 },
];

async function shootOne(url, phone, engine, orientation, zoom, pageSlug) {
  const isPortrait = orientation === 'portrait';
  const baseW  = isPortrait ? phone.width  : phone.height;
  const baseH  = isPortrait ? phone.height : phone.width;
  const viewportW = Math.round(baseW * zoom.scale);
  const viewportH = Math.round(baseH * zoom.scale);
  const captureH  = viewportH * 2;

  const browser = await ENGINES[engine].launch();
  const contextOpts = engine === 'firefox'
    ? { viewport: { width: viewportW, height: viewportH }, userAgent: devices[phone.device].userAgent, hasTouch: true }
    : { ...devices[phone.device], viewport: { width: viewportW, height: viewportH } };
  const context = await browser.newContext(contextOpts);
  const page = await context.newPage();
  await page.goto(url, { waitUntil: 'domcontentloaded' });
  const pg = PAGES.find(p => p.slug === pageSlug);
  await page.waitForSelector(pg.selector, { state: 'attached', timeout: 30_000 });

  if (phone.android) await injectRoboto(page);
  await page.waitForTimeout(500);

  const name = `${phone.slug}-${engine}${zoom.tag}-${pageSlug}-${orientation}.png`;
  const out = join(OUT_DIR, name);
  await page.screenshot({
    path: out,
    fullPage: true,
    clip: { x: 0, y: 0, width: viewportW, height: captureH },
  });
  console.log(`  ✔ ${name}  (${viewportW}×${captureH} CSS px)`);
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
  const filmPage = PAGES.find(p => p.slug === 'film');
  if (filmPage) {
    filmPage.path = await discoverFilmPath(baseUrl);
    console.log(`Film page: ${filmPage.path}`);
  }

  const CONCURRENCY = 10;
  const tasks = [];
  for (const pg of PAGES) {
    const url = `${baseUrl}${pg.path}`;
    for (const phone of PHONES) {
      const engineList = phone.android
        ? [phone.nativeEngine, 'firefox']
        : [phone.nativeEngine];
      for (const engine of engineList) {
        for (const zoom of ZOOM_LEVELS) {
          tasks.push({ url, phone, engine, zoom, orientation: 'portrait',  pageSlug: pg.slug });
          tasks.push({ url, phone, engine, zoom, orientation: 'landscape', pageSlug: pg.slug });
        }
      }
    }
  }

  console.log(`\n${tasks.length} screenshots, ${CONCURRENCY} workers…`);
  let done = 0;
  const t0 = Date.now();

  async function runTask(task) {
    await shootOne(task.url, task.phone, task.engine, task.orientation, task.zoom, task.pageSlug);
    done++;
    if (done % 10 === 0) console.log(`  … ${done}/${tasks.length}`);
  }

  const pending = tasks.slice();
  const active = new Set();
  await new Promise((resolve, reject) => {
    function pump() {
      while (active.size < CONCURRENCY && pending.length > 0) {
        const task = pending.shift();
        const p = runTask(task).then(() => {
          active.delete(p);
          pump();
        }, reject);
        active.add(p);
      }
      if (active.size === 0 && pending.length === 0) resolve();
    }
    pump();
  });

  const elapsed = ((Date.now() - t0) / 1000).toFixed(1);
  console.log(`\nDone in ${elapsed}s — ${tasks.length} screenshots, ~${(tasks.length / elapsed * 1).toFixed(1)} shots/s`);
} finally {
  if (server) {
    server.proc.kill();
    try { unlinkSync(server.portFile); } catch {}
  }
}

const files = PHONES.flatMap(p => {
  const engineList = p.android ? [p.nativeEngine, 'firefox'] : [p.nativeEngine];
  return engineList.flatMap(engine =>
    ZOOM_LEVELS.flatMap(zoom =>
      PAGES.flatMap(pg => [
        join(OUT_DIR, `${p.slug}-${engine}${zoom.tag}-${pg.slug}-portrait.png`),
        join(OUT_DIR, `${p.slug}-${engine}${zoom.tag}-${pg.slug}-landscape.png`),
      ])
    )
  );
});
console.log(`\nOpening ${files.length} screenshots in Preview…`);
execSync(`open -a Preview ${files.map(f => `"${f}"`).join(' ')}`);
console.log('Done.');
