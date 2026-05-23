import { test, expect } from '@playwright/test';
import { pinDateFilterAnytime } from './helpers';

// "Wyczyść" button in the Ukryte filmy modal — `showAllFilms` empties
// the `hiddenFilms` localStorage set in one shot.

test.describe('hidden films bulk unhide', () => {

  test('Wyczyść clears every hidden title at once', async ({ page }) => {
    const seeded = ['Avatar', 'Cars', 'Diabeł ubiera się u Prady 2'];
    await page.goto('/');
    await page.evaluate((titles) => {
      localStorage.setItem('hiddenFilms', JSON.stringify(titles));
    }, seeded);
    await page.reload();
    await pinDateFilterAnytime(page);

    // Open modal — sanity check it has the seeded entries.
    await page.evaluate(() =>
      (globalThis as { openHiddenModal?: () => void }).openHiddenModal?.(),
    );
    await expect(page.locator('#hidden-modal-list .panel-item')).toHaveCount(3);

    // `showAllFilms` is what the Wyczyść button's onclick calls.
    await page.evaluate(() =>
      (globalThis as { showAllFilms?: () => void }).showAllFilms?.(),
    );

    const remaining = await page.evaluate(() => {
      const raw = localStorage.getItem('hiddenFilms');
      return raw ? (JSON.parse(raw) as string[]) : [];
    });
    expect(remaining).toEqual([]);
  });
});
