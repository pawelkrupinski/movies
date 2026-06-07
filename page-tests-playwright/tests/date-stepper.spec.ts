import { test, expect } from '@playwright/test';
import { setDateFilter, waitForCards } from './helpers';

// The navbar's ← / → buttons call `stepDate(±1)`, which now routes through the
// carousel's `animateToDay`: it slides to the adjacent day's column and only
// commits the `#date-filter` <select> value once the slide settles. So the
// value change is asserted with `expect.poll`, not read synchronously after the
// click.

const dayValue = (page: import('@playwright/test').Page) =>
  page.locator('#date-filter').inputValue();

test.describe('date stepper buttons', () => {

  test.beforeEach(async ({ page }) => {
    await page.goto('/poznan/');
    await waitForCards(page);
    // Land on a known starting value so cycling is deterministic.
    await setDateFilter(page, 'today');
  });

  test('the → button advances the select to the next option', async ({ page }) => {
    expect(await dayValue(page)).toBe('today');

    await page.locator('.date-nav-btn').nth(1).click();

    // The slide commits the next preset after 'today'. Not pinning the exact
    // string because the template's option order may evolve.
    await expect.poll(() => dayValue(page)).not.toBe('today');
    expect((await dayValue(page)).length).toBeGreaterThan(0);
  });

  test('the ← button returns to the previous option', async ({ page }) => {
    await page.locator('.date-nav-btn').nth(1).click();
    await expect.poll(() => dayValue(page)).not.toBe('today');
    const advanced = await dayValue(page);

    await page.locator('.date-nav-btn').nth(0).click();
    await expect.poll(() => dayValue(page)).toBe('today');
    expect(advanced).not.toBe('today');
  });
});
