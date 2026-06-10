import { test, expect } from '@playwright/test';
import { getLocalStorageJson, pinDateFilterAnytime } from './helpers';

// Filtry > Kina section: per-cinema checkboxes + the master
// "Wszystkie kina" checkbox. Unchecking a cinema hides its
// `.cinema-group`s across the grid; re-checking restores them.
// `buildCinemaPanel` renders the per-cinema checkboxes into
// `#cinema-list` from `ALL_CINEMAS` at boot.

const visibleCinemaGroups = (page: import('@playwright/test').Page) =>
  page.evaluate(
    () =>
      [...document.querySelectorAll<HTMLElement>('.cinema-group')].filter(
        (g) => g.style.display !== 'none',
      ).length,
  );

test.describe('Filtry > Kina checkboxes', () => {

  test.beforeEach(async ({ page }) => {
    await page.goto('/poznan/');
    await pinDateFilterAnytime(page);
  });

  test('unchecking a cinema hides its .cinema-group elements + writes localStorage', async ({ page }) => {
    // `ALL_CINEMAS` is the canonical list; localStorage stores these
    // raw names, not the pill display names from `CINEMA_PILLS`.
    const firstCinema = await page.evaluate<string>(`ALL_CINEMAS[0]`);
    expect(firstCinema).toBeTruthy();

    const beforeVisible = await visibleCinemaGroups(page);
    expect(beforeVisible).toBeGreaterThan(0);

    // Unchecking the first cinema's checkbox runs the inline
    // `onchange` which writes to `disabledCinemas` + reapplies.
    await page.evaluate(() => {
      const cb = document.querySelector<HTMLInputElement>(
        '#cinema-list .panel-label input[type="checkbox"]',
      );
      cb?.click();
    });

    const stored = (await getLocalStorageJson<string[]>(page, 'disabledCinemas')) ?? [];
    expect(stored).toContain(firstCinema);

    const afterVisible = await visibleCinemaGroups(page);
    expect(afterVisible).toBeLessThan(beforeVisible);

    // None of the visible cinema-groups belong to the disabled cinema.
    const lingering = await page.evaluate((c) =>
      [...document.querySelectorAll<HTMLElement>('.cinema-group')]
        .filter((g) => g.style.display !== 'none')
        .some((g) => g.dataset.cinema === c),
      firstCinema!,
    );
    expect(lingering).toBe(false);
  });

  test('re-checking the cinema brings its groups back', async ({ page }) => {
    const beforeVisible = await visibleCinemaGroups(page);

    // Toggle off then on.
    await page.evaluate(() => {
      const cb = document.querySelector<HTMLInputElement>(
        '#cinema-list .panel-label input[type="checkbox"]',
      );
      cb?.click();
      cb?.click();
    });

    const afterVisible = await visibleCinemaGroups(page);
    expect(afterVisible).toBe(beforeVisible);

    const stored = (await getLocalStorageJson<string[]>(page, 'disabledCinemas')) ?? [];
    expect(stored).toEqual([]);
  });

  test('disabling a cinema lights the Filtry funnel; Wyczyść re-enables cinemas + clears it', async ({ page }) => {
    const btn = page.locator('#format-filter-btn');
    // Neutral funnel to start — nothing filtered yet.
    await expect(btn).not.toHaveClass(/filters-active/);

    // Disabling one cinema is a filter "Wyczyść" clears, so the funnel lights.
    await page.evaluate(() => {
      const cb = document.querySelector<HTMLInputElement>(
        '#cinema-list .panel-label input[type="checkbox"]',
      );
      cb?.click();
    });
    await expect(btn).toHaveClass(/filters-active/);

    // Wyczyść re-enables every cinema in this city (matching iOS/Android) and
    // returns the funnel to neutral.
    const beforeVisible = await visibleCinemaGroups(page);
    await page.evaluate(() =>
      (globalThis as { resetFormatFilter?: () => void }).resetFormatFilter?.(),
    );
    const stored = (await getLocalStorageJson<string[]>(page, 'disabledCinemas')) ?? [];
    expect(stored).toEqual([]);
    expect(await visibleCinemaGroups(page)).toBeGreaterThan(beforeVisible);
    await expect(btn).not.toHaveClass(/filters-active/);
  });

  test('Wszystkie kina master checkbox un-disables everything', async ({ page }) => {
    // Pre-seed every cinema as disabled. `ALL_CINEMAS` is a `const`
    // in `_sharedJsConfig`'s script — script-scoped, not on
    // `globalThis` — so string-form `evaluate` is the way to read it
    // back through `Runtime.evaluate`'s lexical scope.
    await page.evaluate<void>(
      `localStorage.setItem('disabledCinemas', JSON.stringify(ALL_CINEMAS))`,
    );
    await page.reload();
    await pinDateFilterAnytime(page);

    // With every cinema disabled, no .cinema-group is visible.
    const allDisabledVisible = await visibleCinemaGroups(page);
    expect(allDisabledVisible).toBe(0);

    // Click the master checkbox — `onchange` for `#cinema-all` clears
    // the disabledCinemas set and rebuilds the per-cinema checkboxes.
    await page.evaluate(() => {
      (document.getElementById('cinema-all') as HTMLInputElement).click();
    });

    const stored = (await getLocalStorageJson<string[]>(page, 'disabledCinemas')) ?? [];
    expect(stored).toEqual([]);

    const afterMasterVisible = await visibleCinemaGroups(page);
    expect(afterMasterVisible).toBeGreaterThan(0);
  });
});
