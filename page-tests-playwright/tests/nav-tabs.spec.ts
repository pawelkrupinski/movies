import { test, expect } from '@playwright/test';

// Navbar tab highlighting: `_navbar.scala.html` emits `.active` on the Filmy
// tab (the only content tab; "Debug" shows only in dev mode).

test.describe('navbar tab .active class', () => {

  test('Filmy is active on /', async ({ page }) => {
    await page.goto('/poznan/');
    await expect(page.locator('.navbar .nav-tab.active')).toContainText('Filmy');
  });
});
