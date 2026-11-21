import { test, expect } from '@playwright/test';
import { gotoAndWaitForCards } from './helpers';

// The login modal, opened the way a signed-out visitor opens it, and closed each
// way they close it: its ✕, a tap outside its card, and Escape.
test.describe('login modal', { tag: '@agnostic' }, () => {
  test('opens, and closes on its ✕, on a tap outside the card, and on Escape', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    const dialog = page.locator('#login-dialog');
    const card = page.locator('#login-dialog .login-modal');
    const open = () => page.evaluate(() => (globalThis as { openLoginModal?: () => void }).openLoginModal?.());

    await open();
    await expect(card).toBeVisible();
    await page.locator('#login-dialog .login-modal-close').click();
    await expect(card).toBeHidden();

    await open();
    await expect(card).toBeVisible();
    // The dialog fills the viewport around its card: its corner is outside the card.
    await dialog.click({ position: { x: 2, y: 2 } });
    await expect(card).toBeHidden();

    await open();
    await expect(card).toBeVisible();
    await page.keyboard.press('Escape');
    await expect(card).toBeHidden();
  });
});
