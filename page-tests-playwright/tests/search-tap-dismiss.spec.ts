import { test, expect } from '@playwright/test';
import { firstVisibleCard, pinDateFilterAnytime } from './helpers';

// Mobile portrait: the search field is a floating pill at the bottom of the
// screen, so when its keyboard is up the cards sit right behind it. The first
// tap "away" from the focused field must ONLY dismiss the keyboard (blur the
// field) — it must NOT be captured by the card under the finger and navigate to
// /movie. A second, deliberate tap then behaves normally.
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
    await page.goto('/poznan/', { waitUntil: 'domcontentloaded' });
    await pinDateFilterAnytime(page);
  });

  test('a tap on a card while search is focused only dismisses — no navigation', async ({ page }) => {
    const title = (await firstVisibleCard(page))?.title;
    expect(title).toBeTruthy();

    // Focus the search field, as a real finger-tap on the pill would.
    await page.locator('#search-input').focus();
    expect(await page.evaluate(() => document.activeElement?.id)).toBe('search-input');

    const image = page.locator(`.col[data-title="${title}"] .card .poster-wrap > a img`);
    await expect(image).toBeVisible();
    // Whether the tap's click was cancelled, read once its dispatch is over: a capture listener on `window` runs
    // before shared.js's on `document` (which stops propagation), and its timeout after every listener.
    await page.evaluate(() => window.addEventListener('click', (e) => {
      setTimeout(() => { (window as unknown as { tapPrevented: boolean }).tapPrevented = e.defaultPrevented; });
    }, { capture: true, once: true }));
    await image.tap();

    // The tap must be swallowed: its click cancelled (so the link never navigates), the field blurred (its
    // keyboard drops with it), and we stay on the listing. Waiting on the cancellation itself replaces a 300ms
    // sleep that stood in for "long enough for a navigation to have started".
    await expect.poll(() => page.evaluate(() =>
      (window as unknown as { tapPrevented?: boolean }).tapPrevented)).toBe(true);
    expect(new URL(page.url()).pathname).toBe('/poznan/');
    expect(await page.evaluate(() => document.activeElement?.id)).not.toBe('search-input');
  });

  test('a tap on a card with search NOT focused still navigates to /movie', async ({ page }) => {
    // Captured before the tap — the film page has no cards to read them from,
    // and one read keeps the title and the slug on the SAME card.
    const card = await firstVisibleCard(page);
    expect(card).toBeTruthy();
    const { title, slug } = card!;

    const image = page.locator(`.col[data-title="${title}"] .card .poster-wrap > a img`);
    await expect(image).toBeVisible();
    await image.tap();

    await page.waitForURL(/\/movie\/[a-z0-9-]+$/, { waitUntil: 'domcontentloaded' });
    // The film's identity is the path's last segment now, not a query param.
    expect(new URL(page.url()).pathname).toBe(`/poznan/movie/${slug}`);
  });
});
