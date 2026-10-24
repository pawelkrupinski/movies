import { test, expect, type Page } from '@playwright/test';
import { gotoAndWaitForCards } from './helpers';

// A search looks through every day: typing moves the listing to "All", the days the
// search brought in are drawn lighter, and clearing it goes back to the day it began on
// -- or stays on a day picked while it ran. It also matches a film's original title.
// Runs on every engine: the search box and the pills, no touch or CDP.
const pillDay = (page: Page) => page.locator('.day-pill.active').getAttribute('data-day');
const background = (page: Page, selector: string) =>
  page.locator(selector).first().evaluate(element => getComputedStyle(element).backgroundColor);

test.describe('searching every day', { tag: '@agnostic' }, () => {
  test('a search shows every day, the added ones lighter, and clearing it goes back', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    await page.locator('#search-input').fill('straszny film');
    await expect.poll(() => pillDay(page)).toBe('anytime');
    // `:visible`: a card folded behind its "+N" link hides its later days.
    await expect(page.locator('.date-group.outside-day:visible').first()).toBeVisible();
    expect(await background(page, '.date-group.outside-day:visible .badge-time')).toBe('rgb(77, 77, 130)');
    expect(await background(page, '.date-group:not(.outside-day):visible .badge-time')).toBe('rgb(58, 58, 110)');

    await page.locator('#search-input').fill('');
    await expect.poll(() => pillDay(page)).toBe('today');
    await expect(page.locator('.date-group.outside-day')).toHaveCount(0);
  });

  test('a day picked during the search stays when it is cleared', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/?date=tomorrow');
    await page.locator('#search-input').fill('straszny film');
    await expect.poll(() => pillDay(page)).toBe('anytime');
    // A tap while the box has focus only dismisses the keyboard (search-tap-dismiss.spec.ts):
    // leave the box first, as a person does.
    await page.locator('#search-input').blur();
    await page.locator('.day-pill[data-day="today"]').click();
    await expect.poll(() => page.locator('#date-filter').inputValue()).toBe('today');
    await expect(page.locator('.date-group.outside-day')).toHaveCount(0);

    await page.locator('#search-input').fill('');
    await expect.poll(() => pillDay(page)).toBe('today');
  });

  test('a film is found by its original title', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    await page.locator('#search-input').fill('silence of the lambs');
    await expect(page.locator('#film-grid > .col[data-title="Milczenie owiec"]')).toBeVisible();
  });
});
