import { test, expect } from '@playwright/test';
import { gotoAndWaitForCards } from './helpers';

// The listing ships TODAY and fetches the rest of the week after first paint.
// Measured on the fixture corpus, that took Warszawa's document from 46,438 DOM
// tags to 9,320 — and `#film-grid` is cloaked until the first applyFilters pass
// has walked every card, so that count is what the visitor waits on.
//
// Everything here is about the seam that creates: a page that is briefly
// today-only, and a grid that is replaced underneath the filters and the swipe
// carousel while the visitor is already looking at it.
test.describe('today-first grid', { tag: '@agnostic' }, () => {

  test('the document carries today only, then widens to the week', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    // The server marks what it rendered; the client overwrites it after the swap.
    // Polling rather than asserting once: the fetch is deliberately deferred to
    // an idle callback, so "not yet" is a legal intermediate state.
    await expect.poll(
      () => page.locator('#film-grid').getAttribute('data-window'),
      { message: 'grid should widen to the 7-day window' },
    ).toBe('7');
  });

  test('widening ADDS films rather than replacing them with a different set', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/?date=anytime');
    const todayCards = await page.locator('#film-grid .col[data-title]').count();
    await expect.poll(() => page.locator('#film-grid').getAttribute('data-window')).toBe('7');
    const wideCards = await page.locator('#film-grid .col[data-title]').count();
    expect(wideCards).toBeGreaterThanOrEqual(todayCards);
  });

  test('every film stays reachable in server HTML, for a crawler and a no-JS visitor', async ({ page }) => {
    // The reason the later-films list exists: `/{city}/` is in the sitemap and
    // robots.txt does not disallow it, so a today-only document would drop
    // hundreds of film links from a page Google indexes. Asserted against the
    // RAW response, because that is what a crawler that does not run our JS sees.
    const res = await page.request.get('/poznan/');
    const html = await res.text();
    const links = new Set([...html.matchAll(/href="\/poznan\/movie\/[a-z0-9-]+"/g)].map(m => m[0]));
    expect(links.size).toBeGreaterThan(50);
    expect(html).toContain('id="later-films"');
  });

  test('the later-films list is removed once the real cards arrive', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    await expect.poll(() => page.locator('#film-grid').getAttribute('data-window')).toBe('7');
    // Otherwise the same films are on the page twice — as cards and as links.
    await expect(page.locator('#later-films')).toHaveCount(0);
  });

  test('a day tapped before the grid widens is honoured, not dropped', async ({ page }) => {
    // The queue. The fetch starts at boot so it has usually landed by the time
    // anybody reaches for a pill — but if it has not, the tap must survive
    // rather than showing an empty day or being swallowed.
    await page.route('**/movies/grid**', async route => {
      await new Promise(r => setTimeout(r, 1200));   // hold the window open
      await route.continue();
    });
    await gotoAndWaitForCards(page, '/poznan/');
    await page.locator('.day-pill[data-day="tomorrow"]').click();
    // Acknowledged immediately, even though the grid cannot answer it yet.
    await expect(page.locator('.day-pill[data-day="tomorrow"]')).toHaveClass(/\bactive\b/);
    await expect.poll(() => page.locator('#date-filter').inputValue()).toBe('tomorrow');
    // ...and still the chosen day once the window lands.
    await expect.poll(() => page.locator('#film-grid').getAttribute('data-window')).toBe('7');
    await expect.poll(() => page.locator('#date-filter').inputValue()).toBe('tomorrow');
    await expect(page.locator('.day-pill.active')).toHaveCount(1);
  });

  test('a failed fetch leaves a usable page rather than pills stuck loading', async ({ page }) => {
    await page.route('**/movies/grid**', route => route.abort());
    await gotoAndWaitForCards(page, '/poznan/');
    // Today still renders, and no pill is left claiming to be loading forever.
    await expect(page.locator('#film-grid .col[data-title]').first()).toBeVisible();
    await expect.poll(() => page.locator('.day-pill--loading').count()).toBe(0);
  });
});
