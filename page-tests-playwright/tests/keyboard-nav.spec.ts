import { test, expect } from '@playwright/test';
import { gotoAndWaitForCards, setDateFilter, waitForCards } from './helpers';

// Document-level keydown handler in shared.js maps ArrowLeft / Right
// to `stepDate(-1 / 1)` — the same function the navbar's date-arrow
// buttons call. Keyboard-only users can cycle the date filter
// without reaching for the on-screen controls.

test.describe('keyboard arrow date navigation', { tag: '@agnostic' }, () => {

  test.beforeEach(async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    // Land on a known starting value so the cycle is deterministic.
    await setDateFilter(page, 'today');
  });

  // The arrow keys now route through the carousel's `animateToDay`, which
  // commits the `#date-filter` value only after the slide settles — so the
  // value change is asserted with `expect.poll`, not read synchronously.
  test('ArrowRight advances the date filter', async ({ page }) => {
    expect(await page.locator('#date-filter').inputValue()).toBe('today');

    await page.keyboard.press('ArrowRight');

    await expect.poll(() => page.locator('#date-filter').inputValue()).not.toBe('today');
  });

  test('ArrowLeft returns to the previous filter', async ({ page }) => {
    await page.keyboard.press('ArrowRight');
    await expect.poll(() => page.locator('#date-filter').inputValue()).not.toBe('today');
    const advanced = await page.locator('#date-filter').inputValue();
    await page.keyboard.press('ArrowLeft');
    await expect.poll(() => page.locator('#date-filter').inputValue()).toBe('today');
    expect(advanced).not.toBe('today');
  });

  test('arrow keys do nothing when focus is inside the search input', async ({ page }) => {
    const search = page.locator('#search-input');
    const hidden = !(await search.isVisible());
    test.skip(hidden, 'search input is display:none on ultra-narrow viewports');

    await search.focus();
    const before = await page.locator('#date-filter').inputValue();
    await page.keyboard.press('ArrowRight');   // inside the field: must not step

    // No sleep-then-compare (a 400ms beat was too short on a loaded host and wasted on an idle one). A step queues
    // from the day the carousel is HEADING to (`stepDate`), so one press outside the field and one back must land on
    // `before` exactly; had the press in the field stepped, they land a day past it.
    await page.evaluate(() => (document.activeElement as HTMLElement | null)?.blur());
    await page.keyboard.press('ArrowRight');
    await expect.poll(() => page.locator('#date-filter').inputValue()).not.toBe(before);
    await page.keyboard.press('ArrowLeft');
    await expect.poll(() => page.locator('#date-filter').inputValue()).toBe(before);
  });
});
