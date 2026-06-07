import { test, expect } from '@playwright/test';
import type { Page } from '@playwright/test';
import { waitForCards } from './helpers';

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

  // Drag a real horizontal touch across `#film-grid`. The distance is 55% of
  // the grid WIDTH (> the handler's 40% commit threshold) and the start point
  // is offset so the whole drag stays on-screen — so the commit is decided by
  // position, not by the CDP touch's unreliable synthetic velocity, on any
  // viewport (narrow portrait through wide landscape).
  async function swipe(page: Page, dir: 'left' | 'right'): Promise<void> {
    const box = (await page.locator('#film-grid').boundingBox())!;
    const y    = box.y + Math.min(box.height / 2, 150);
    const dist = box.width * 0.55;
    const x0   = dir === 'left' ? box.x + box.width * 0.8 : box.x + box.width * 0.2;
    const dx   = dir === 'left' ? -dist : dist;   // left = next day, right = previous
    const client = await page.context().newCDPSession(page);
    await client.send('Input.dispatchTouchEvent', { type: 'touchStart', touchPoints: [{ x: x0, y }] });
    const steps = 12;
    for (let i = 1; i <= steps; i++) {
      await client.send('Input.dispatchTouchEvent',
        { type: 'touchMove', touchPoints: [{ x: x0 + (dx * i) / steps, y }] });
    }
    await client.send('Input.dispatchTouchEvent', { type: 'touchEnd', touchPoints: [] });
    await client.detach();
  }

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
    await swipe(page, 'left');                   // next day
    await expect.poll(() => dayIndex(page)).toBe(1);
    await expect.poll(() => settled(page)).toBe(true);
    await swipe(page, 'right');                  // previous day
    await expect.poll(() => dayIndex(page)).toBe(0);
  });

  test('swipe right on the first day wraps to the last', async ({ page }) => {
    await startOn(page, 'today');               // selectedIndex 0
    const last = (await dayCount(page)) - 1;
    await swipe(page, 'right');                  // previous, wraps past the start
    await expect.poll(() => dayIndex(page)).toBe(last);
  });
});
