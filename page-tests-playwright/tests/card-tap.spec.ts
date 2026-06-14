import { test, expect } from '@playwright/test';
import { pinDateFilterAnytime } from './helpers';

// Single-tap card navigation — tapping a poster or title link goes
// straight to the /film detail page on every browser. Icons (★, ✕)
// are always visible; no two-tap preview system.

test.describe('card poster link on WebKit (iPhone emulation)', () => {
  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(
      testInfo.project.name !== 'webkit',
      'webkit (iPhone 13) project only',
    );
    await page.goto('/poznan/');
    await pinDateFilterAnytime(page);
  });

  // `applyFilters` re-appends visible cards after hidden ones in the
  // DOM, so a plain `querySelector` lands on a hidden card. Different
  // engines also serialise `style.display = 'none'` slightly differently
  // (`display: none;` vs `display:none;`), making a CSS-attribute
  // selector brittle. Solve both at once by computing the visible
  // card's title in JS, then targeting it via `[data-title=…]`.
  //
  // Also skip cards whose poster `<img>` has been `display:none`-d by
  // the inline `onerror` fallback — the upstream poster URL (Multikino,
  // Helios, etc.) sometimes 404s from GH Actions Linux runners while
  // resolving from my laptop, leaving a real but visually-hidden img
  // that would fail `toBeVisible()` below.
  const firstVisibleTitle = async (page: import('@playwright/test').Page) =>
    page.evaluate(() => {
      const cols = [...document.querySelectorAll<HTMLElement>('.col[data-title]')];
      for (const c of cols) {
        if (c.style.display === 'none') continue;
        const image = c.querySelector<HTMLImageElement>('.poster-wrap > a img');
        if (!image || image.style.display === 'none') continue;
        return c.dataset.title ?? null;
      }
      return null;
    });

  // Target the `<img>` inside the poster-wrap, not the wrapping `<a>`.
  // `.poster-wrap` uses the `padding-top: 148%` aspect-ratio trick for
  // its visual height, so the `<a>` in normal flow has a zero-height
  // bounding box — Playwright's `toBeVisible()` reports it as hidden
  // even though real users see the poster image just fine. The `<img>`
  // inside is positioned absolutely (`inset: 0`) so it has the actual
  // poster dimensions; tapping it dispatches a click event that
  // bubbles up to the `<a>` exactly as a real-finger tap would.
  test('tap on a card poster image navigates to /film', async ({ page }) => {
    const title = await firstVisibleTitle(page);
    expect(title).toBeTruthy();

    const image = page.locator(`.col[data-title="${title}"] .card .poster-wrap > a img`);
    await expect(image).toBeVisible();
    await image.tap();

    // `domcontentloaded`: the `/film` page's `load` event is gated on
    // poster-proxy images + the trailer iframe and can stall a contended
    // runner; the URL we assert on flips at navigation commit.
    await page.waitForURL(/\/film\?title=/, { waitUntil: 'domcontentloaded' });
    // URLSearchParams handles both `+` (form-style space encoding, what
    // Play's `routes` reverse-router emits for spaces) and `%20`. A
    // plain `decodeURIComponent(url.search).contains(title)` would
    // silently fail for any film title containing a space.
    const params = new URLSearchParams(new URL(page.url()).search);
    expect(params.get('title')).toBe(title);
  });

  test('detail page renders without a JS error', async ({ page }) => {
    const errors: string[] = [];
    page.on('pageerror', (e) => errors.push(e.message));

    const title = await firstVisibleTitle(page);
    expect(title).toBeTruthy();
    const image = page.locator(`.col[data-title="${title}"] .card .poster-wrap > a img`);
    await expect(image).toBeVisible();
    await image.tap();
    // `domcontentloaded`: the inline boot scripts run at DCL, so the JS-error
    // assertion doesn't need the poster/iframe `load` that can stall a runner.
    await page.waitForURL(/\/film\?title=/, { waitUntil: 'domcontentloaded' });

    // film.scala.html's inline `toggleFavMovie` + `playTrailer` blocks
    // run on DOMContentLoaded — a syntax error or undefined reference
    // would surface here. Empty `errors` is the assertion.
    expect(errors).toEqual([]);
  });
});
