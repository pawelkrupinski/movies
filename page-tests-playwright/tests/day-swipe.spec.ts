import { test, expect } from '@playwright/test';
import type { Page } from '@playwright/test';
import { waitForCards, cdpSwipe } from './helpers';

// A horizontal swipe steps the selected day, WRAPPING the `#date-filter`
// option list: swipe LEFT = next day, RIGHT = previous. The gesture is driven
// with REAL touch events via CDP (not synthetic PointerEvents), so it exercises
// the same pointer/touch path the production handlers use — proving a committed
// swipe actually calls `stepDateWrap`, not just that the function works.
// CDP touch injection is chromium-only and the swipe is gated to coarse
// pointers (phones), so this runs on the mobile-chromium projects.
test.describe('day-swipe', () => {
  test.beforeEach(async ({ page, browserName }) => {
    test.skip(browserName !== 'chromium', 'CDP touch injection is chromium-only');
    await page.goto('/poznan/?date=anytime');   // anytime → cards present regardless of wall-clock
    const coarse = await page.evaluate(() => matchMedia('(pointer: coarse)').matches);
    test.skip(!coarse, 'day-swipe is gated to coarse pointers (phones)');
    await waitForCards(page);
  });

  const dayIndex = (page: Page) =>
    page.evaluate(() => (document.getElementById('date-filter') as HTMLSelectElement).selectedIndex);
  const dayCount = (page: Page) =>
    page.evaluate(() => (document.getElementById('date-filter') as HTMLSelectElement).options.length);
  // The commit animation clears `#view-root`'s transform once it settles.
  const settled = (page: Page) =>
    page.evaluate(() => {
      const t = getComputedStyle(document.getElementById('view-root')!).transform;
      return t === 'none' || t === 'matrix(1, 0, 0, 1, 0, 0)';
    });

  async function startOn(page: Page, value: string): Promise<void> {
    await page.locator('#date-filter').selectOption(value);
    await expect.poll(() => settled(page)).toBe(true);
  }

  test('swipe left advances to the next day; swipe right goes back', async ({ page }) => {
    await startOn(page, 'today');               // selectedIndex 0
    await cdpSwipe(page, 'left');                   // next day
    await expect.poll(() => dayIndex(page)).toBe(1);
    await expect.poll(() => settled(page)).toBe(true);
    await cdpSwipe(page, 'right');                  // previous day
    await expect.poll(() => dayIndex(page)).toBe(0);
  });

  test('swipe right on the first day wraps to the last', async ({ page }) => {
    await startOn(page, 'today');               // selectedIndex 0
    const last = (await dayCount(page)) - 1;
    await cdpSwipe(page, 'right');                  // previous, wraps past the start
    await expect.poll(() => dayIndex(page)).toBe(last);
  });
});
