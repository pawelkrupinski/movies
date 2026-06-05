import { test, expect } from '@playwright/test';
import { setDateFilter, waitForCards } from './helpers';

// Document-level keydown handler in shared.js maps ArrowLeft / Right
// to `stepDate(-1 / 1)` — the same function the navbar's date-arrow
// buttons call. Keyboard-only users can cycle the date filter
// without reaching for the on-screen controls.

test.describe('keyboard arrow date navigation', () => {

  test.beforeEach(async ({ page }) => {
    await page.goto('/poznan/');
    await waitForCards(page);
    // Land on a known starting value so the cycle is deterministic.
    await setDateFilter(page, 'today');
  });

  test('ArrowRight advances the date filter', async ({ page }) => {
    const before = await page.locator('#date-filter').inputValue();
    expect(before).toBe('today');

    await page.keyboard.press('ArrowRight');

    const after = await page.locator('#date-filter').inputValue();
    expect(after).not.toBe('today');
  });

  test('ArrowLeft returns to the previous filter', async ({ page }) => {
    await page.keyboard.press('ArrowRight');
    const advanced = await page.locator('#date-filter').inputValue();
    await page.keyboard.press('ArrowLeft');
    const back = await page.locator('#date-filter').inputValue();
    expect(back).toBe('today');
    expect(back).not.toBe(advanced);
  });

  test('arrow keys do nothing when focus is inside the search input', async ({ page }) => {
    const search = page.locator('#search-input');
    const hidden = !(await search.isVisible());
    test.skip(hidden, 'search input is display:none on ultra-narrow viewports');

    await search.focus();
    const before = await page.locator('#date-filter').inputValue();
    await page.keyboard.press('ArrowRight');
    const after = await page.locator('#date-filter').inputValue();
    expect(after).toBe(before);
  });
});
