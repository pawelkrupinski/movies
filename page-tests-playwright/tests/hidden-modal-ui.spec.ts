import { test, expect, type Page } from '@playwright/test';
import { gotoAndWaitForCards } from './helpers';

// The modal's card (`.hidden-modal`) — visible exactly while the modal is open.
const modalCard = (page: Page) => page.locator('.hidden-modal');

const openHiddenModal = (page: Page) =>
  page.evaluate(() =>
    (globalThis as { openHiddenModal?: (e?: Event) => void }).openHiddenModal?.()
  );

// Whether keyboard focus currently sits inside the modal's card.
const focusIsInsideModal = (page: Page): Promise<boolean> =>
  page.evaluate(() => !!document.activeElement?.closest('.hidden-modal'));

// Hidden-films modal open/close via UI:
//   - The `#hidden-row-count` badge (inside the Filtry button area)
//     opens the modal when there's at least one hidden film. Its
//     count reflects the number of hidden titles.
//   - The modal's ✕ button (`.login-modal-close`), Escape and a click on the
//     backdrop close it; a click on the card itself does not.
//   - While open it is modal: focus moves into it and the page behind can't
//     take focus.

test.describe('hidden films modal UI', { tag: '@agnostic' }, () => {

  test.beforeEach(async ({ page }) => {
    // Seed BEFORE the first navigation rather than loading, writing
    // localStorage and reloading: the page's `DOMContentLoaded` init reads
    // `hiddenFilms:pl` once at boot, so an init script gets it there in time and
    // the listing — 2.3 MB of HTML over ~190 cards — is parsed once instead of
    // twice. The second parse was costing Firefox the whole test budget.
    await page.addInitScript(() => {
      localStorage.setItem('hiddenFilms:pl', JSON.stringify(['Avatar', 'Cars']));
    });
    await gotoAndWaitForCards(page, '/poznan/');
  });

  test('the Ukryte filmy row opens the modal + the count badge reflects the set size', async ({ page }) => {
    // Open the Filtry dropdown first — the row + its count badge
    // live inside `#format-panel` (`display:none` at rest).
    await page.locator('#format-filter-btn').click();
    await expect(page.locator('#hidden-row-count')).toHaveText(/2/);

    await expect(modalCard(page)).toBeHidden();

    // Click the whole hidden-row — its onclick is `openHiddenModal`.
    await page.locator('#hidden-row').click();

    await expect(modalCard(page)).toBeVisible();
  });

  test('the ✕ close button hides the modal again', async ({ page }) => {
    await openHiddenModal(page);
    await expect(modalCard(page)).toBeVisible();

    await page.locator('.hidden-modal .login-modal-close').click();
    await expect(modalCard(page)).toBeHidden();
  });

  test('clicking the backdrop dismisses the modal; clicking the card does not', async ({ page }) => {
    await openHiddenModal(page);
    await expect(modalCard(page)).toBeVisible();

    // A click on the card's own title keeps it open.
    await page.locator('.hidden-modal .login-modal-title').click();
    await expect(modalCard(page)).toBeVisible();

    // Click the backdrop (NOT the inner card): a fixed top-left viewport
    // coordinate well outside the card's content box.
    await page.mouse.click(5, 5);
    await expect(modalCard(page)).toBeHidden();
  });

  test('Escape closes the modal', async ({ page }) => {
    await openHiddenModal(page);
    await expect(modalCard(page)).toBeVisible();

    await page.keyboard.press('Escape');
    await expect(modalCard(page)).toBeHidden();
  });

  test('opening moves focus into the modal and the page behind it cannot take focus', async ({ page }) => {
    await page.locator('#format-filter-btn').click();
    await page.locator('#hidden-row').click();
    await expect(modalCard(page)).toBeVisible();

    expect(await focusIsInsideModal(page)).toBe(true);

    // The page behind is inert: focusing one of its controls is refused and
    // focus stays in the modal.
    await page.evaluate(() => (document.getElementById('format-filter-btn') as HTMLElement).focus());
    expect(await focusIsInsideModal(page)).toBe(true);
  });
});
