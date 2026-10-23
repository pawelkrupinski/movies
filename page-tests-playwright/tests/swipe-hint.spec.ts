import { test, expect } from '@playwright/test';
import type { Page } from '@playwright/test';
import { cdpSwipe, reload } from './helpers';

// The first-run swipe hint ("Przesuń, aby zmienić dzień"): shown once per
// calendar day on touch devices and retired for good after the first swipe —
// the same frequency rule the iOS / Android apps follow. Desktop never sees it.
test.describe('swipe hint', () => {
  const hint = (page: Page) => page.locator('#swipe-hint');
  const visit = (page: Page) => page.goto('/poznan/?date=anytime', { waitUntil: 'domcontentloaded' });

  // The hint auto-hides 3 s after it shows (a page timer). On a starved CI box the
  // browser can stall past that between the page showing it and Playwright polling,
  // so a plain `toBeVisible()` sees it already gone. Pausing the page clock before the
  // load keeps that timer from firing, so "visible" holds until the test asserts it.
  const freezeHintTimer = async (page: Page) => {
    await page.clock.install();
    await page.clock.pauseAt(new Date(Date.now() + 1000));
    await page.clock.fastForward(1000);
  };

  // Touch + viewport-independent: once on mobile Chromium (the only engine with CDP
  // touch injection) is the whole answer, so it rides `@agnostic`. The desktop
  // describe below stays untagged — it only runs on fine-pointer projects.
  test.describe('phones (coarse pointer)', { tag: '@agnostic' }, () => {
    // Skips on the project's own touch flag, not a probe load: every test gets a fresh context, so its one
    // `visit` is already the first visit with nothing stored. The probe load plus a clear-and-reload spent two
    // listing loads (6–13s each at ci-runner load 100–180) of the 30s before the test began; three of these timed
    // out in Main runs 1545, 1586 and 1665.
    test.beforeEach(async ({ browserName, hasTouch }) => {
      test.skip(browserName !== 'chromium', 'CDP touch injection is chromium-only');
      test.skip(!hasTouch, 'the hint only shows on coarse pointers (phones)');
    });

    test('shows on a fresh device, with the day-swipe copy', async ({ page }) => {
      await freezeHintTimer(page);
      await visit(page);
      await expect(hint(page)).toBeVisible();
      await expect(hint(page)).toContainText('Przesuń, aby zmienić dzień');
    });

    test('does not show a second time the same day', async ({ page }) => {
      await freezeHintTimer(page);
      await visit(page);
      await expect(hint(page)).toBeVisible();   // first visit today
      await reload(page);
      await expect(hint(page)).toBeHidden();    // same-day reload → suppressed
    });

    test('a swipe retires it for good (survives the per-day reset)', async ({ page }) => {
      await freezeHintTimer(page);
      await visit(page);
      await expect(hint(page)).toBeVisible();
      await cdpSwipe(page, 'left');             // commit a day-swipe
      await expect(hint(page)).toBeHidden();
      // Even after clearing the once-a-day marker, the retired flag keeps it away.
      await page.evaluate(() => localStorage.removeItem('kinowoSwipeHintDay'));
      await reload(page);
      await expect(hint(page)).toBeHidden();
    });
  });

  test.describe('desktop (fine pointer)', () => {
    test('never shows', async ({ page, hasTouch }) => {
      test.skip(hasTouch, 'this case is for fine-pointer (desktop) projects');
      await visit(page);
      await expect(hint(page)).toBeHidden();
    });
  });
});
