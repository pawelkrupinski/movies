import { defineConfig, devices } from '@playwright/test';

// Drive the live kinowo.fly.dev site rather than a local fixture build:
// the Scala spec covers fixture-grounded assertions, this suite is a
// real-traffic smoke test for the WebKit engine specifically. Setting
// the base URL here means individual specs can use relative paths.
const BASE_URL = process.env.KINOWO_BASE_URL ?? 'https://kinowo.fly.dev';

export default defineConfig({
  testDir: './tests',
  // One worker is enough — WebKit boot is a few seconds, and the
  // smoke set is small. Parallelism comes from the GH Actions matrix
  // layer, not from within Playwright.
  workers: 1,
  // The live site can occasionally hiccup on cold-start — one retry
  // smooths over a single 5xx without masking a real regression.
  retries: 1,
  timeout: 30_000,
  expect: { timeout: 5_000 },
  use: {
    baseURL: BASE_URL,
    // Trace on first retry only — gives us debug data when something
    // goes wrong without keeping a trace per pass.
    trace: 'on-first-retry',
  },
  projects: [
    // iPhone 13 device emulation — UA, viewport, devicePixelRatio,
    // and touch all match a real iPhone. Playwright's bundled WebKit
    // is the closest CI proxy for Mobile Safari short of paid
    // device-cloud services.
    {
      name: 'webkit',
      use: { ...devices['iPhone 13'] },
    },
  ],
});
