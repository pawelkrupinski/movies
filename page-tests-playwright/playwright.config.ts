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
    // ─── Desktop projects ───────────────────────────────────────────────
    //
    // The three mobile projects above pin the phone-form-factor JS + CSS.
    // The desktop projects below cover the >= 992 px branch — different
    // navbar layout (one row, no row-break), `pointer: fine` truthy,
    // hover-driven UI affordances. smoke runs on every project; card-tap
    // and a11y are scoped via per-spec predicates (see those files).
    //
    // Desktop WebKit — Safari approximation on a Linux runner. Same
    // engine as the iPhone project but desktop viewport + mouse pointer
    // + no iOS-only CSS branches. Catches Safari-engine regressions in
    // the desktop chrome the iPhone project would miss.
    {
      name: 'webkit-desktop',
      use: { ...devices['Desktop Safari'] },
    },
    // Desktop Chromium. Re-covers Chrome engine through the Playwright
    // harness on top of the Scala-side page-tests-chrome matrix; this
    // is the project that hosts the axe-core a11y audit (one project
    // is enough since axe is DOM/CSS-driven, not engine-driven).
    {
      name: 'chromium-desktop',
      use: { ...devices['Desktop Chrome'] },
    },
    // Desktop Firefox — Gecko engine with full desktop viewport +
    // mouse pointer. Sole representation of the third major engine at
    // desktop size; routinely diverges from Blink/WebKit on flexbox
    // edge cases, focus rings, and CSS containment.
    {
      name: 'firefox-desktop',
      use: { ...devices['Desktop Firefox'] },
    },
    // Desktop Edge via Playwright's `msedge` channel. Same engine as
    // chromium-desktop but with Edge's defaults (smart-screen,
    // tracking-prevention) and UA. The Scala page-tests-edge job
    // already covers Edge desktop end-to-end; this is the cross-
    // harness consistency check. Requires `playwright install msedge`
    // in CI so the runner has the Edge binary.
    {
      name: 'msedge-desktop',
      use: { ...devices['Desktop Edge'], channel: 'msedge' },
    },
    // ─── Mobile landscape projects ──────────────────────────────────
    //
    // Phone landscape: width > height, height ≤ 500 px. The
    // `(max-height: 500px) and (orientation: landscape)` media
    // query keeps the desktop one-row navbar but shrinks card
    // chrome (`--mobile-scale: 0.85`) and packs 6 cards per row.
    // Three viewport sizes — Pixel 7 (915 px), iPhone 13 (844 px),
    // iPhone 17 Pro Max (956 px) — so the band of "wider than
    // 768 px md breakpoint but still height-constrained" is
    // covered end-to-end.
    {
      name: 'chromium-landscape',
      use: {
        ...devices['Pixel 7'],
        viewport: { width: devices['Pixel 7'].viewport.height, height: devices['Pixel 7'].viewport.width },
      },
    },
    {
      name: 'webkit-landscape',
      use: {
        ...devices['iPhone 13'],
        viewport: { width: devices['iPhone 13'].viewport.height, height: devices['iPhone 13'].viewport.width },
      },
    },
    {
      name: 'webkit-landscape-iphone-17-pro-max',
      use: {
        // Playwright's bundled `devices` table doesn't ship the
        // 17 Pro Max yet; copy iPhone 13's UA + touch hints, swap
        // the viewport in landscape to the 17 Pro Max's 956 × 440.
        ...devices['iPhone 13'],
        viewport: { width: 956, height: 440 },
      },
    },
    {
      name: 'firefox-landscape',
      use: {
        browserName: 'firefox',
        viewport: { width: devices['Pixel 7'].viewport.height, height: devices['Pixel 7'].viewport.width },
        userAgent: devices['Pixel 7'].userAgent,
        hasTouch: devices['Pixel 7'].hasTouch,
      },
    },
  ],
});
