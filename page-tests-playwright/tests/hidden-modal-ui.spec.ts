import { test, expect } from '@playwright/test';
import { setLocalStorageJson } from './helpers';

// Hidden-films modal open/close via UI:
//   - The `#hidden-row-count` badge (inside the Filtry button area)
//     opens the modal when there's at least one hidden film. Its
//     count reflects the number of hidden titles.
//   - The modal's ✕ button (`.login-modal-close`) closes it.
//   - Clicking the backdrop (`#hidden-modal-backdrop`) closes it.

test.describe('hidden films modal UI', () => {

  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await setLocalStorageJson(page, 'hiddenFilms', ['Avatar', 'Cars']);
    // `reload()` defaults to waiting for `load` — that needs every poster
    // (~190 cards, each routed through `images.weserv.nl`) to finish, which
    // on the CI webkit-iphone-13 runner regularly blows past the 30s test
    // timeout. We only need the page's `DOMContentLoaded`-bound init to
    // run so `updateNavbar()` picks up the localStorage we just set —
    // image fetches are irrelevant to anything this spec asserts.
    await page.reload({ waitUntil: 'domcontentloaded' });
  });

  test('the Ukryte filmy row opens the modal + the count badge reflects the set size', async ({ page }) => {
    // Open the Filtry dropdown first — the row + its count badge
    // live inside `#format-panel` (`display:none` at rest).
    await page.locator('#format-filter-btn').click();
    await expect(page.locator('#hidden-row-count')).toHaveText(/2/);

    // Modal hidden at rest (`.open` not on the backdrop).
    const openBefore = await page.evaluate(() =>
      document.getElementById('hidden-modal-backdrop')?.classList.contains('open') ?? null
    );
    expect(openBefore).toBe(false);

    // Click the whole hidden-row — its onclick is `openHiddenModal`.
    await page.locator('#hidden-row').click();

    const openAfter = await page.evaluate(() =>
      document.getElementById('hidden-modal-backdrop')?.classList.contains('open') ?? null
    );
    expect(openAfter).toBe(true);
  });

  test('the ✕ close button hides the modal again', async ({ page }) => {
    await page.evaluate(() =>
      (globalThis as { openHiddenModal?: (e?: Event) => void }).openHiddenModal?.()
    );
    await expect(page.locator('#hidden-modal-backdrop')).toHaveClass(/open/);

    await page.locator('.hidden-modal .login-modal-close').click();
    const stillOpen = await page.evaluate(() =>
      document.getElementById('hidden-modal-backdrop')?.classList.contains('open') ?? null
    );
    expect(stillOpen).toBe(false);
  });

  test('clicking the backdrop dismisses the modal', async ({ page }) => {
    await page.evaluate(() =>
      (globalThis as { openHiddenModal?: (e?: Event) => void }).openHiddenModal?.()
    );
    await expect(page.locator('#hidden-modal-backdrop')).toHaveClass(/open/);

    // Click the backdrop itself (NOT the inner modal). Use a fixed
    // top-left coordinate well outside the modal's content box.
    await page.locator('#hidden-modal-backdrop').click({ position: { x: 5, y: 5 } });

    const stillOpen = await page.evaluate(() =>
      document.getElementById('hidden-modal-backdrop')?.classList.contains('open') ?? null
    );
    expect(stillOpen).toBe(false);
  });
});
