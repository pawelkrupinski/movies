import { test, expect } from '@playwright/test';
import { setDateFilter, waitForCards } from './helpers';

// `?date=` is the round-trip URL representation of the navbar's
// `#date-filter`. Selecting a day rewrites the URL via
// `history.replaceState`; loading a URL with the param applies that
// day before the first `applyFilters()` pass so the visible cards
// already reflect the chosen state.

test.describe('date selector ↔ URL', () => {

  test('selecting a non-default day adds ?date= to the URL', async ({ page }) => {
    await page.goto('/');
    await waitForCards(page);

    await setDateFilter(page, 'tomorrow');

    expect(new URL(page.url()).searchParams.get('date')).toBe('tomorrow');
  });

  test('returning to "today" strips ?date= from the URL', async ({ page }) => {
    await page.goto('/?date=tomorrow');
    await waitForCards(page);
    expect(new URL(page.url()).searchParams.get('date')).toBe('tomorrow');

    await setDateFilter(page, 'today');

    expect(new URL(page.url()).searchParams.get('date')).toBeNull();
  });

  test('opening /?date=anytime applies the filter on first paint', async ({ page }) => {
    await page.goto('/?date=anytime');
    await waitForCards(page);

    await expect(page.locator('#date-filter')).toHaveValue('anytime');
  });

  test('an unrecognised ?date= value is ignored — default stays as "today"', async ({ page }) => {
    await page.goto('/?date=tomorrowish');
    await waitForCards(page);

    await expect(page.locator('#date-filter')).toHaveValue('today');
  });

  test('?date= works the same on /kina', async ({ page }) => {
    await page.goto('/kina?date=week');
    await waitForCards(page);

    await expect(page.locator('#date-filter')).toHaveValue('week');

    await setDateFilter(page, 'tomorrow');
    expect(new URL(page.url()).searchParams.get('date')).toBe('tomorrow');
    // Path preserved — query change shouldn't move us off /kina.
    expect(new URL(page.url()).pathname).toBe('/kina');
  });

  test('selecting a cinema on /kina preserves ?date=', async ({ page }) => {
    await page.goto('/kina?date=tomorrow');
    await waitForCards(page);

    // Pick the first real cinema (option index 1; index 0 is "Wszystkie kina").
    const select = page.locator('#cinema-select');
    const firstCinema = await select.locator('option').nth(1).getAttribute('value');
    await select.selectOption(firstCinema!);

    const url = new URL(page.url());
    expect(url.pathname).toMatch(/^\/kina\//);
    expect(url.searchParams.get('date')).toBe('tomorrow');
  });
});
