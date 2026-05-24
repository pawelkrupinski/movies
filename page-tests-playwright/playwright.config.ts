import { defineConfig, devices, type Project } from '@playwright/test';

const BASE_URL = process.env.KINOWO_BASE_URL ?? 'https://kinowo.fly.dev';
const IS_LOCAL_FIXTURE = BASE_URL.startsWith('http://127.0.0.1');

// ─── Phone definitions ──────────────────────────────────────────────
//
// Effective CSS-pixel viewports on real devices. iPhone heights are
// post-safe-area (Dynamic Island + home indicator subtracted).

interface Phone { slug: string; width: number; height: number }

const ANDROID_PHONES: Phone[] = [
  { slug: 'galaxy-s10',       width: 360, height: 760 },
  { slug: 'galaxy-s23',       width: 360, height: 780 },
  { slug: 'pixel-9',          width: 360, height: 808 },
  { slug: 'galaxy-s25-ultra', width: 412, height: 891 },
  { slug: 'pixel-7',          width: 412, height: 915 },
  { slug: 'pixel-9-pro',      width: 427, height: 952 },
];

const IPHONES: Phone[] = [
  { slug: 'iphone-se',           width: 375, height: 667 },
  { slug: 'iphone-13',           width: 390, height: 760 },
  { slug: 'iphone-17',           width: 393, height: 770 },
  { slug: 'iphone-16-pro',       width: 402, height: 790 },
  { slug: 'iphone-15-pro-max',   width: 430, height: 850 },
  { slug: 'iphone-17-pro-max',   width: 440, height: 870 },
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

  // ─── Firefox — Android phones only, portrait + landscape, normal + 150% zoom
  //
  // Gecko engine diverges enough from Blink / WebKit that mobile bugs
  // frequently surface here first. iPhone users don't run Firefox
  // (iOS forces WebKit for all browsers), so Firefox tests cover
  // Android viewports only.
  ...ANDROID_PHONES.flatMap(p => variants(p).map(v => ({
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
  { name: 'chromium-desktop', use: { ...devices['Desktop Chrome'] } },
  { name: 'firefox-desktop',  use: { ...devices['Desktop Firefox'] } },
  { name: 'msedge-desktop',   use: { ...devices['Desktop Edge'], channel: 'msedge' } },
];

export default defineConfig({
  testDir: './tests',
  workers: '50%',
  retries: IS_LOCAL_FIXTURE ? 0 : 1,
  timeout: 30_000,
  expect: { timeout: 5_000 },
  use: {
    baseURL: BASE_URL,
    trace: 'on-first-retry',
  },
  metadata: { isLocalFixture: IS_LOCAL_FIXTURE, baseURL: BASE_URL },
  projects,
});
