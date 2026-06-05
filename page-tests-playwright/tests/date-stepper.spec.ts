import { test, expect } from '@playwright/test';
import { setDateFilter, waitForCards } from './helpers';

// The navbar's ← / → buttons call `stepDate(±1)` which cycles the
// `#date-filter` <select> through its options. Visible-card count
// follows the select-value change because `stepDate` ends with
// `applyFilters()`.

test.describe('date stepper buttons', () => {

  test.beforeEach(async ({ page }) => {
    await page.goto('/poznan/');
    await waitForCards(page);
    // Land on a known starting value so cycling is deterministic.
    await setDateFilter(page, 'today');
  });

  test('the → button advances the select to the next option', async ({ page }) => {
    const before = await page.locator('#date-filter').inputValue();
    expect(before).toBe('today');

    await page.locator('.date-nav-btn').nth(1).click();

    const after = await page.locator('#date-filter').inputValue();
    expect(after).not.toBe('today');
    // The next value in the select after 'today' is one of the
    // remaining presets — not pinning the exact string because the
    // template's option order may evolve.
    expect(after.length).toBeGreaterThan(0);
  });

  test('the ← button returns to the previous option', async ({ page }) => {
    await page.locator('.date-nav-btn').nth(1).click();
    const advanced = await page.locator('#date-filter').inputValue();
    await page.locator('.date-nav-btn').nth(0).click();
    const back = await page.locator('#date-filter').inputValue();
    expect(back).toBe('today');
    expect(back).not.toBe(advanced);
  });
});
