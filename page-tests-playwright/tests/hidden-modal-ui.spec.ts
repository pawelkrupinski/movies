import { test, expect, type Page } from '@playwright/test';
import { setLocalStorageJson } from './helpers';

// Whether the hidden-films modal is currently open, read from the
// `.open` class on `#hidden-modal-backdrop`. Returns `null` if the
// backdrop element isn't present at all.
const isModalOpen = (page: Page): Promise<boolean | null> =>
  page.evaluate(() =>
    document.getElementById('hidden-modal-backdrop')?.classList.contains('open') ?? null,
  );

// Hidden-films modal open/close via UI:
//   - The `#hidden-row-count` badge (inside the Filtry button area)
//     opens the modal when there's at least one hidden film. Its
//     count reflects the number of hidden titles.
//   - The modal's ✕ button (`.login-modal-close`) closes it.
//   - Clicking the backdrop (`#hidden-modal-backdrop`) closes it.

test.describe('hidden films modal UI', { tag: '@agnostic' }, () => {

  test.beforeEach(async ({ page }) => {
    await page.goto('/poznan/');
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
    expect(await isModalOpen(page)).toBe(false);

    // Click the whole hidden-row — its onclick is `openHiddenModal`.
    await page.locator('#hidden-row').click();

    expect(await isModalOpen(page)).toBe(true);
  });

  test('the ✕ close button hides the modal again', async ({ page }) => {
    await page.evaluate(() =>
      (globalThis as { openHiddenModal?: (e?: Event) => void }).openHiddenModal?.()
    );
    await expect(page.locator('#hidden-modal-backdrop')).toHaveClass(/open/);

    await page.locator('.hidden-modal .login-modal-close').click();
    expect(await isModalOpen(page)).toBe(false);
  });

  test('clicking the backdrop dismisses the modal', async ({ page }) => {
    await page.evaluate(() =>
      (globalThis as { openHiddenModal?: (e?: Event) => void }).openHiddenModal?.()
    );
    await expect(page.locator('#hidden-modal-backdrop')).toHaveClass(/open/);

    // Click the backdrop itself (NOT the inner modal). Use a fixed
    // top-left coordinate well outside the modal's content box.
    await page.locator('#hidden-modal-backdrop').click({ position: { x: 5, y: 5 } });

    expect(await isModalOpen(page)).toBe(false);
  });
});
