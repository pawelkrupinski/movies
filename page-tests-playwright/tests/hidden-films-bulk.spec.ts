import { test, expect } from '@playwright/test';
import { getLocalStorageJson, pinDateFilterAnytime, setLocalStorageJson } from './helpers';

// "Wyczyść" button in the Ukryte filmy modal — `showAllFilms` empties
// the `hiddenFilms` localStorage set in one shot.

test.describe('hidden films bulk unhide', () => {

  test('Wyczyść clears every hidden title at once', async ({ page }) => {
    const seeded = ['Avatar', 'Cars', 'Diabeł ubiera się u Prady 2'];
    await page.goto('/');
    await setLocalStorageJson(page, 'hiddenFilms', seeded);
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

    const remaining = (await getLocalStorageJson<string[]>(page, 'hiddenFilms')) ?? [];
    expect(remaining).toEqual([]);
  });
});
