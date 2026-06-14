import { test, expect } from '@playwright/test';
import { setDateFilter, gotoAndWaitForCards } from './helpers';

// `?date=` is the round-trip URL representation of the navbar's
// `#date-filter`. Selecting a day rewrites the URL via
// `history.replaceState`; loading a URL with the param applies that
// day before the first `applyFilters()` pass so the visible cards
// already reflect the chosen state.

test.describe('date selector ↔ URL', { tag: '@agnostic' }, () => {

  test('selecting a non-default day adds ?date= to the URL', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');

    await setDateFilter(page, 'tomorrow');

    expect(new URL(page.url()).searchParams.get('date')).toBe('tomorrow');
  });

  test('returning to "today" strips ?date= from the URL', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/?date=tomorrow');
    expect(new URL(page.url()).searchParams.get('date')).toBe('tomorrow');

    await setDateFilter(page, 'today');

    expect(new URL(page.url()).searchParams.get('date')).toBeNull();
  });

  test('opening /?date=anytime applies the filter on first paint', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/?date=anytime');

    await expect(page.locator('#date-filter')).toHaveValue('anytime');
  });

  test('an unrecognised ?date= value is ignored — default stays as "today"', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/?date=tomorrowish');

    await expect(page.locator('#date-filter')).toHaveValue('today');
  });

});
