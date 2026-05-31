import { test, expect } from '@playwright/test';

// Navbar tab highlighting: `_navbar.scala.html` emits `.active` on
// the tab matching the current page (Filmy / Kina).

test.describe('navbar tab .active class', () => {

  test('Filmy is active on /', async ({ page }) => {
    await page.goto('/');
    await expect(page.locator('.navbar .nav-tab.active')).toContainText('Filmy');
  });

  test('Kina is active on /kina', async ({ page }) => {
    await page.goto('/kina');
    await expect(page.locator('.navbar .nav-tab.active')).toContainText('Kina');
  });

  test('Filmy → Kina via tab link switches the active tab', async ({ page }) => {
    await page.goto('/');
    await page.locator('.navbar .nav-tab', { hasText: 'Kina' }).click();
    await page.waitForURL(/\/kina$/);
    await expect(page.locator('.navbar .nav-tab.active')).toContainText('Kina');
  });
});

// The full in-place slide-swap behaviour (no reload, DOM swap, swipe, popstate)
// lives in view-swap.spec.ts; this file only guards the .active highlighting.
