import { defineConfig, devices } from '@playwright/test';

// Two ways to point the suite at a backend:
//
//   1. `KINOWO_BASE_URL=http://127.0.0.1:<port>` — what CI does. The
//      port comes from `FixtureServerMain` (Scala) which boots
//      `FixtureTestWiring` + serves Twirl-rendered pages over the
//      same `TestHttpServer` the Scala spec uses. Fully fixture-
//      grounded: every page is deterministic, no network, no real
//      database.
//
//   2. Falls back to the live https://kinowo.fly.dev when no env
//      var is set, so a developer can `npx playwright test` against
//      production for smoke purposes.
//
// Tests tagged `@live-only` skip when running against the local
// fixture server (because they need real, mutable data) and run only
// against the live site. The vast majority of tests use the local
// fixture path so CI is deterministic; the `@live-only` band stays
// small to limit the flakiness that real data introduces.
const BASE_URL = process.env.KINOWO_BASE_URL ?? 'https://kinowo.fly.dev';
const IS_LOCAL_FIXTURE = BASE_URL.startsWith('http://127.0.0.1');

export default defineConfig({
  testDir: './tests',
  // One worker is enough for now — WebKit boot is a few seconds, and
  // each spec runs a small number of cheap assertions. If the suite
  // grows past 20 tests it's worth bumping to 2-4 (each gets its own
  // browser context anyway, no shared state).
  workers: 1,
  // The live site can occasionally hiccup on cold-start — one retry
  // smooths over a single 5xx without masking a real regression. The
  // local fixture server doesn't need retries.
  retries: IS_LOCAL_FIXTURE ? 0 : 1,
  timeout: 30_000,
  expect: { timeout: 5_000 },
  use: {
    baseURL: BASE_URL,
    // Trace on first retry only — gives us debug data when something
    // goes wrong without keeping a trace per pass.
    trace: 'on-first-retry',
  },
  // Tests can `test.skip(...IS_LIVE_ONLY)` to gate themselves; we
  // expose the flag here via the metadata so any spec that doesn't
  // care doesn't have to repeat the env-var dance.
  metadata: { isLocalFixture: IS_LOCAL_FIXTURE, baseURL: BASE_URL },
  projects: [
    // Chromium + Pixel 7 — closest to the median Android Chrome user.
    // Playwright bundles its own Chromium build, so this is engine
    // coverage in addition to the Scala matrix's Chrome-version
    // coverage (the Scala matrix uses installed Chrome; Playwright
    // uses bundled). Both checks reach a different surface area.
    {
      name: 'chromium',
      use: { ...devices['Pixel 7'] },
    },
    // WebKit + iPhone 13 — closest CI proxy for Mobile Safari short
    // of paid device-cloud services. Catches generic WebKit-engine
    // regressions in the same JS we ship to iPhone users.
    {
      name: 'webkit',
      use: { ...devices['iPhone 13'] },
    },
    // Firefox — desktop mobile-viewport sanity. Firefox on Android is
    // a smaller slice than Chrome / Safari, but the engine is far
    // enough from Blink / WebKit that mobile bugs frequently surface
    // here first.
    //
    // Playwright's Firefox doesn't support `isMobile` device emulation
    // (only `viewport` + `userAgent`), so we copy the Pixel 7 profile
    // selectively rather than spreading `devices['Pixel 7']`.
    {
      name: 'firefox',
      use: {
        browserName: 'firefox',
        viewport:    devices['Pixel 7'].viewport,
        userAgent:   devices['Pixel 7'].userAgent,
        hasTouch:    devices['Pixel 7'].hasTouch,
      },
    },
  ],
});
