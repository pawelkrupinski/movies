import { test, expect } from '@playwright/test';
import type { Page } from '@playwright/test';
import { cdpSwipe } from './helpers';

// The first-run swipe hint ("Przesuń, aby zmienić dzień"): shown once per
// calendar day on touch devices and retired for good after the first swipe —
// the same frequency rule the iOS / Android apps follow. Desktop never sees it.
test.describe('swipe hint', () => {
  const hint = (page: Page) => page.locator('#swipe-hint');

  test.describe('phones (coarse pointer)', () => {
    test.beforeEach(async ({ page, browserName }) => {
      test.skip(browserName !== 'chromium', 'CDP touch injection is chromium-only');
      await page.goto('/poznan/?date=anytime');
      const coarse = await page.evaluate(() => matchMedia('(pointer: coarse)').matches);
      test.skip(!coarse, 'the hint only shows on coarse pointers (phones)');
    });

    test('shows on a fresh device, with the day-swipe copy', async ({ page }) => {
      await page.evaluate(() => localStorage.clear());
      await page.reload();
      await expect(hint(page)).toBeVisible();
      await expect(hint(page)).toContainText('Przesuń, aby zmienić dzień');
    });

    test('does not show a second time the same day', async ({ page }) => {
      await page.evaluate(() => localStorage.clear());
      await page.reload();
      await expect(hint(page)).toBeVisible();   // first visit today
      await page.reload();
      await expect(hint(page)).toBeHidden();    // same-day reload → suppressed
    });

    test('a swipe retires it for good (survives the per-day reset)', async ({ page }) => {
      await page.evaluate(() => localStorage.clear());
      await page.reload();
      await expect(hint(page)).toBeVisible();
      await cdpSwipe(page, 'left');             // commit a day-swipe
      await expect(hint(page)).toBeHidden();
      // Even after clearing the once-a-day marker, the retired flag keeps it away.
      await page.evaluate(() => localStorage.removeItem('kinowoSwipeHintDay'));
      await page.reload();
      await expect(hint(page)).toBeHidden();
    });
  });

  test.describe('desktop (fine pointer)', () => {
    test('never shows', async ({ page }) => {
      await page.goto('/poznan/?date=anytime');
      const coarse = await page.evaluate(() => matchMedia('(pointer: coarse)').matches);
      test.skip(coarse, 'this case is for fine-pointer (desktop) projects');
      await page.evaluate(() => localStorage.clear());
      await page.reload();
      await expect(hint(page)).toBeHidden();
    });
  });
});
