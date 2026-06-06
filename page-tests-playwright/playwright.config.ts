import { defineConfig, devices, type Project } from '@playwright/test';

const BASE_URL = process.env.KINOWO_BASE_URL ?? 'https://kinowo.fly.dev';
const IS_LOCAL_FIXTURE = BASE_URL.startsWith('http://127.0.0.1');

// ─── Phone definitions ──────────────────────────────────────────────
//
// CSS-pixel viewports matching each device's full logical resolution.

interface Phone { slug: string; width: number; height: number }

const ANDROID_PHONES: Phone[] = [
  { slug: 'galaxy-s10',       width: 360, height: 760 },
  { slug: 'pixel-9',          width: 360, height: 808 },
  { slug: 'galaxy-s25-ultra', width: 412, height: 891 },
  { slug: 'pixel-9-pro',      width: 427, height: 952 },
];

const IPHONES: Phone[] = [
  { slug: 'iphone-se',           width: 375, height: 667 },
  { slug: 'iphone-13',           width: 390, height: 844 },
  { slug: 'iphone-17-pro-max',   width: 440, height: 956 },
];

const FIREFOX_PHONES: Phone[] = [
  ANDROID_PHONES[0],  // galaxy-s10  360 px
];

const ALL_PHONES = [...ANDROID_PHONES, ...IPHONES];

// 150% display zoom → viewport shrinks to 2/3 of normal.
function zoomed(p: Phone): Phone {
  return { slug: `${p.slug}-zoomed`, width: Math.round(p.width * 2 / 3), height: Math.round(p.height * 2 / 3) };
}

function landscape(p: Phone): Phone {
  return { slug: `${p.slug}-landscape`, width: p.height, height: p.width };
}

// All four orientation × zoom variants for a phone.
function variants(p: Phone): Phone[] {
  return [p, zoomed(p), landscape(p), landscape(zoomed(p))];
}

// ─── Engine helpers ─────────────────────────────────────────────────

function firefoxUse(p: Phone) {
  return {
    browserName: 'firefox' as const,
    viewport:  { width: p.width, height: p.height },
    userAgent: devices['Pixel 7'].userAgent,
    hasTouch:  devices['Pixel 7'].hasTouch,
  };
}

function webkitUse(p: Phone) {
  return {
    ...devices['iPhone 13'],
    viewport: { width: p.width, height: p.height },
  };
}

function chromiumUse(p: Phone) {
  return {
    ...devices['Pixel 7'],
    viewport: { width: p.width, height: p.height },
  };
}

// ─── Project generation ─────────────────────────────────────────────

const projects: Project[] = [

  // ─── Firefox — 3 Android phones (S10 / S25 Ultra / Pixel 9 Pro),
  // portrait + landscape, normal + 150% zoom.
  //
  // Subset of ANDROID_PHONES — narrowest, middle, widest — enough to
  // catch Gecko-specific rendering bugs. Full Android breadth is
  // covered by Chromium.
  ...FIREFOX_PHONES.flatMap(p => variants(p).map(v => ({
    name: `firefox-${v.slug}`,
    use: firefoxUse(v),
  }))),

  // ─── WebKit — all iPhones, portrait + landscape, normal + 150% zoom
  //
  // Closest CI proxy for Mobile Safari. Each iPhone viewport is tested
  // in both orientations and at 150% accessibility zoom.
  ...IPHONES.flatMap(p => variants(p).map(v => ({
    name: `webkit-${v.slug}`,
    use: webkitUse(v),
  }))),

  // ─── Chromium — all Android phones, portrait + landscape, normal + 150% zoom
  ...ANDROID_PHONES.flatMap(p => variants(p).map(v => ({
    name: `chromium-${v.slug}`,
    use: chromiumUse(v),
  }))),

  // ─── Desktop ──────────────────────────────────────────────────────
  { name: 'webkit-desktop',   use: { ...devices['Desktop Safari'] } },
  { name: 'chrome-desktop',   use: { ...devices['Desktop Chrome'], channel: 'chrome' } },
  { name: 'firefox-desktop',  use: { ...devices['Desktop Firefox'] } },
];

export default defineConfig({
  testDir: './tests',
  workers: '100%',
  retries: IS_LOCAL_FIXTURE ? 0 : 1,
  timeout: 30_000,
  expect: { timeout: 5_000 },
  // On CI emit a JUnit report (for the inline check-run) and an HTML
  // report (uploaded as a failure artifact) alongside the console list;
  // local runs stay as plain `list`.
  reporter: process.env.CI
    ? [['list'], ['junit', { outputFile: 'test-results/junit.xml' }], ['html', { open: 'never' }]]
    : 'list',
  use: {
    baseURL: BASE_URL,
    trace: 'on-first-retry',
    // Capture a screenshot on failure so a CI-only break is visible in
    // the uploaded report without re-running locally.
    screenshot: 'only-on-failure',
  },
  metadata: { isLocalFixture: IS_LOCAL_FIXTURE, baseURL: BASE_URL },
  projects,
});
