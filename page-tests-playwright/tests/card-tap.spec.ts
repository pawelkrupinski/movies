import { test, expect } from '@playwright/test';
import { firstVisibleCard, pinDateFilterAnytime } from './helpers';

// Single-tap card navigation — tapping a poster or title link goes
// straight to the /movie detail page on every browser. Icons (★, ✕)
// are always visible; no two-tap preview system.

test.describe('card poster link on WebKit (iPhone emulation)', { tag: '@agnostic' }, () => {
  test.beforeEach(async ({ page }, testInfo) => {
    // Gate on the capability the test actually needs, not just the engine
    // name: `webkit-desktop` is Desktop Safari, which starts with "webkit" but
    // has no `hasTouch`, so `image.tap()` throws "The page does not support
    // tap" there. The iPhone-emulation projects inherit touch from
    // `devices['iPhone 13']`.
    test.skip(
      !testInfo.project.name.startsWith('webkit') || !testInfo.project.use.hasTouch,
      'webkit projects with touch (iPhone emulation) only',
    );
    await page.goto('/poznan/', { waitUntil: 'domcontentloaded' });
    await pinDateFilterAnytime(page);
  });

  // `firstVisibleTitle` (helpers) computes the visible card's title in
  // JS and skips broken-poster cards, so the `[data-title=…]` target
  // below resolves to a card whose `<img>` actually passes
  // `toBeVisible()` — see its doc for the full rationale.

  // Target the `<img>` inside the poster-wrap, not the wrapping `<a>`.
  // `.poster-wrap` uses the `padding-top: 148%` aspect-ratio trick for
  // its visual height, so the `<a>` in normal flow has a zero-height
  // bounding box — Playwright's `toBeVisible()` reports it as hidden
  // even though real users see the poster image just fine. The `<img>`
  // inside is positioned absolutely (`inset: 0`) so it has the actual
  // poster dimensions; tapping it dispatches a click event that
  // bubbles up to the `<a>` exactly as a real-finger tap would.
  test('tap on a card poster image navigates to /movie', async ({ page }) => {
    // Title AND slug in one read, while the listing is still on screen — after
    // the tap there are no cards left to read them from, and two reads could
    // name two different films (see `firstVisibleCard`).
    const card = await firstVisibleCard(page);
    expect(card).toBeTruthy();
    const { title, slug } = card!;

    const image = page.locator(`.col[data-title="${title}"] .card .poster-wrap > a img`);
    await expect(image).toBeVisible();
    await image.tap();

    // `domcontentloaded`: the `/movie` page's `load` event is gated on
    // poster-proxy images + the trailer iframe and can stall a contended
    // runner; the URL we assert on flips at navigation commit.
    await page.waitForURL(/\/movie\/[a-z0-9-]+$/, { waitUntil: 'domcontentloaded' });
    // The film's identity is the path's last segment now, not a query param.
    expect(new URL(page.url()).pathname).toBe(`/poznan/movie/${slug}`);
    expect(new URL(page.url()).search).toBe('');
  });

  test('detail page renders without a JS error', async ({ page }) => {
    const errors: string[] = [];
    page.on('pageerror', (e) => errors.push(e.message));

    const title = (await firstVisibleCard(page))?.title;
    expect(title).toBeTruthy();
    const image = page.locator(`.col[data-title="${title}"] .card .poster-wrap > a img`);
    await expect(image).toBeVisible();
    await image.tap();
    // `domcontentloaded`: the inline boot scripts run at DCL, so the JS-error
    // assertion doesn't need the poster/iframe `load` that can stall a runner.
    await page.waitForURL(/\/movie\/[a-z0-9-]+$/, { waitUntil: 'domcontentloaded' });

    // film.scala.html's inline `toggleFavMovie` + `playTrailer` blocks
    // run on DOMContentLoaded — a syntax error or undefined reference
    // would surface here. Empty `errors` is the assertion.
    expect(errors).toEqual([]);
  });
});
