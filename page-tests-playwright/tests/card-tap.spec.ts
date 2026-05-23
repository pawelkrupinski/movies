import { test, expect } from '@playwright/test';
import { pinDateFilterAnytime } from './helpers';

// WebKit (Mobile Safari approximation) — regression coverage for the
// card-tap two-tap behaviour. iPhone Safari's native sticky-hover
// handles the first-tap preview without our JS, and `shared.js`
// explicitly DOESN'T install the Android two-tap listener on iOS UAs
// (running it there would force a three-tap flow). These tests pin
// the iPhone-parity contract from the WebKit side.

test.describe('card poster link on WebKit (iPhone emulation)', () => {
  // The iPhone UA + sticky-hover assertions are tied to the iPhone-13
  // emulation project. The describe-level `test.skip(callback)` form
  // only receives fixtures, not TestInfo, so reading project.name has
  // to happen from a beforeEach (where TestInfo is the 2nd arg). A
  // plain `browserName !== 'webkit'` predicate would also let this
  // run on webkit-desktop (Desktop Safari) which has a desktop UA and
  // no touch — every assertion in this file would fail there.
  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(
      testInfo.project.name !== 'webkit',
      'iPhone UA + sticky-hover assertions — webkit (iPhone 13) project only',
    );
    await page.goto('/');
    // The `/` page renders 100+ cards from the live database; pin the
    // date filter to "anytime" so the visible set isn't a function
    // of the runner's wall-clock relative to the live schedule.
    await pinDateFilterAnytime(page);
  });

  test('UA is iPhone-shaped so the Android listener stays unbound', async ({ page }) => {
    const ua = await page.evaluate(() => navigator.userAgent);
    expect(ua).toMatch(/iPhone/);
    // shared.js gates the two-tap listener on `/Android/.test(ua)`;
    // iPhone UA should not match.
    expect(ua).not.toMatch(/Android/);
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
        const img = c.querySelector<HTMLImageElement>('.poster-wrap > a img');
        if (!img || img.style.display === 'none') continue;
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

    const img = page.locator(`.col[data-title="${title}"] .card .poster-wrap > a img`);
    await expect(img).toBeVisible();
    await img.tap();

    await page.waitForURL(/\/film\?title=/);
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
    const img = page.locator(`.col[data-title="${title}"] .card .poster-wrap > a img`);
    await expect(img).toBeVisible();
    await img.tap();
    await page.waitForURL(/\/film\?title=/);

    // film.scala.html's inline `toggleFavMovie` + `playTrailer` blocks
    // run on DOMContentLoaded — a syntax error or undefined reference
    // would surface here. Empty `errors` is the assertion.
    expect(errors).toEqual([]);
  });
});
