import { test, expect } from '@playwright/test';
import { pinDateFilterAnytime, firstVisibleTitle } from './helpers';

// Mobile portrait: the search field is a floating pill at the bottom of the
// screen, so when its keyboard is up the cards sit right behind it. The first
// tap "away" from the focused field must ONLY dismiss the keyboard (blur the
// field) — it must NOT be captured by the card under the finger and navigate to
// /film. A second, deliberate tap then behaves normally.
//
// Only the portrait phone projects float the pill (the `(max-width: 575px) and
// (orientation: portrait)` block in `_sharedStyles`); desktop and landscape
// keep search inline, where an outside tap behaves normally and this guard is a
// no-op, so those projects are skipped.

const isFloatingPillProject = (name: string) =>
  !name.includes('desktop') && !name.includes('landscape');

test.describe('search-pill tap-away on mobile portrait', () => {
  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(
      !isFloatingPillProject(testInfo.project.name),
      'floating search pill is mobile-portrait only',
    );
    await page.goto('/poznan/');
    await pinDateFilterAnytime(page);
  });

  test('a tap on a card while search is focused only dismisses — no navigation', async ({ page }) => {
    const title = await firstVisibleTitle(page);
    expect(title).toBeTruthy();

    // Focus the search field, as a real finger-tap on the pill would.
    await page.locator('#search-input').focus();
    expect(await page.evaluate(() => document.activeElement?.id)).toBe('search-input');

    const image = page.locator(`.col[data-title="${title}"] .card .poster-wrap > a img`);
    await expect(image).toBeVisible();
    await image.tap();

    // The tap must be swallowed: the field blurs (its keyboard drops with it)
    // and we stay on the listing. Settle first so an async navigation commit —
    // which is what we're asserting did NOT happen — has time to land before we
    // read the URL, rather than racing it.
    await page.waitForTimeout(300);
    expect(new URL(page.url()).pathname).toBe('/poznan/');
    expect(await page.evaluate(() => document.activeElement?.id)).not.toBe('search-input');
  });

  test('a tap on a card with search NOT focused still navigates to /film', async ({ page }) => {
    const title = await firstVisibleTitle(page);
    expect(title).toBeTruthy();

    const image = page.locator(`.col[data-title="${title}"] .card .poster-wrap > a img`);
    await expect(image).toBeVisible();
    await image.tap();

    await page.waitForURL(/\/film\?title=/, { waitUntil: 'domcontentloaded' });
    const params = new URLSearchParams(new URL(page.url()).search);
    expect(params.get('title')).toBe(title);
  });
});
