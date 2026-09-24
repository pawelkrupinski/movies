// Render one real listing page in each named engine before the suite starts,
// so the per-user caches a FRESH Linux runner starts without are built once,
// serially, outside every test's 30s budget.
//
// The one that matters is Mesa's shader cache (~/.cache/mesa_shader_cache).
// The runners have no GPU, so WebKit composites through llvmpipe, which
// compiles every GL shader the page needs with LLVM on first use. With an
// empty cache, that compile lands on the first frame each worker paints. And
// all N workers paint theirs at the same moment, each compiling the same
// shaders again. Measured in a fresh `playwright:v1.60.0-noble` container
// (2 CPUs): the first rAF after loading /poznan/ took 1.7s cold vs 0.2s warm
// on its own, and 4.2s vs 0.9s with five concurrent. Under the suite's full
// first-batch load it took 8–10s, and any test whose first frame-bound action
// waited for it (a `tap`'s stability check waits on rAF) lost that time
// from its budget. On CI that was enough to time out
// `search-tap-dismiss.spec.ts` in the first batch of a shard.
//
// It has to be the REAL page. The shaders are compiled per variant the page
// actually draws, so a warm-up on a trivial `setContent` page left the
// first frame as slow as fully cold.
//
// Usage: KINOWO_BASE_URL=http://127.0.0.1:PORT node warm-browsers.mjs webkit [chromium chrome firefox]
import { chromium, firefox, webkit, devices } from '@playwright/test';

const baseURL = process.env.KINOWO_BASE_URL;
if (!baseURL) {
  console.error('warm-browsers: KINOWO_BASE_URL is not set');
  process.exit(1);
}

// Engine names as the CI `browsers` input spells them (the `npx playwright
// install` arguments). The contexts match the projects' own device bases in
// playwright.config.ts, so the warm-up draws at the same device scale factor
// as the tests.
const engines = {
  webkit:   { type: webkit,   context: devices['iPhone 13'] },
  chromium: { type: chromium, context: devices['Pixel 7'] },
  chrome:   { type: chromium, context: devices['Desktop Chrome'], launch: { channel: 'chrome' } },
  firefox:  { type: firefox,  context: { viewport: { width: 360, height: 760 } } },
};

for (const name of process.argv.slice(2)) {
  const engine = engines[name];
  if (!engine) {
    console.error(`warm-browsers: unknown engine "${name}" (known: ${Object.keys(engines).join(', ')})`);
    process.exit(1);
  }
  const started = Date.now();
  const browser = await engine.type.launch(engine.launch);
  try {
    const page = await browser.newPage(engine.context);
    await page.goto(new URL('/poznan/', baseURL).href, { waitUntil: 'load' });
    // Two frames: the first paints the page and compiles its shaders; the
    // second returning proves that paint finished.
    await page.evaluate(() => new Promise(done =>
      requestAnimationFrame(() => requestAnimationFrame(done))));
  } finally {
    await browser.close();
  }
  console.log(`warm-browsers: ${name} warmed in ${Date.now() - started}ms`);
}
