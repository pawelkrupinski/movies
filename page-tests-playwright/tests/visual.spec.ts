import { test, expect } from '@playwright/test';

// Visual regression on the stable UI chrome at the top of the page.
// Card-grid contents shift daily as the schedule changes and films
// come and go, so a full-page screenshot would be permanently red.
// A fixed-clip of the top viewport (header + filter controls) is
// data-independent — only CSS/layout/typography drift moves it.
//
// Baselines MUST be generated on Linux to match the CI runner. From
// the page-tests-playwright/ directory, with Docker Desktop running:
//
//   docker run --rm -v "$PWD":/work -w /work \
//     mcr.microsoft.com/playwright:v1.49.0-jammy \
//     bash -c "npm ci && npx playwright test visual --update-snapshots"
//
// then commit the generated `tests/visual.spec.ts-snapshots/` folder
// and remove the `.skip` below to enable the suite. Never regenerate
// from macOS — sub-pixel WebKit differences between macOS and Linux
// will make the baseline drift against CI forever.

test.describe.skip('visual regression', () => {
  test('home page top chrome', async ({ page }) => {
    await page.goto('/poznan/');
    // `state: 'attached'` — the home page's inline filter hides
    // out-of-window cards and shuffles them to the front of DOM order,
    // so Playwright's default `'visible'` check would never resolve.
    await page.waitForSelector('.col[data-title]', { state: 'attached' });
    // iPhone 13 viewport is 390 wide; 240 high clips header + filter
    // controls without reaching into the card grid.
    await expect(page).toHaveScreenshot('home-top-chrome.png', {
      clip: { x: 0, y: 0, width: 390, height: 240 },
      maxDiffPixelRatio: 0.01,
    });
  });
});
