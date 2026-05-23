import { test, expect } from '@playwright/test';
import {
  firstVisibleTitle,
  getLocalStorageJson,
  pinDateFilterAnytime,
  setLocalStorageJson,
} from './helpers';

// Hide-film lifecycle:
//   1. The ✕ button on a poster persists the title to
//      `hiddenFilms` localStorage and removes the card from view.
//   2. The Filtry modal's "Ukryte filmy" list surfaces every
//      currently-hidden title.
//   3. Unhiding via that list returns the card to the grid.

test.describe('hide film flow', () => {

  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await pinDateFilterAnytime(page);
  });

  test('clicking ✕ on a poster writes to hiddenFilms localStorage', async ({ page }) => {
    const title = await firstVisibleTitle(page);
    expect(title).toBeTruthy();

    // `hideFilm` is the inline JS the delegated `.hide-btn` click
    // handler routes to. Calling it directly with the button avoids
    // the awkward visibility chain (the ✕ is `opacity: 0` until
    // hover/preview); the persistence semantics are what we're
    // testing.
    await page.evaluate((t) => {
      const btn = document.querySelector(
        `.col[data-title="${t}"] .hide-btn`,
      ) as HTMLButtonElement;
      (globalThis as { hideFilm?: (el: HTMLElement) => void }).hideFilm?.(btn);
    }, title!);

    const stored = (await getLocalStorageJson<string[]>(page, 'hiddenFilms')) ?? [];
    expect(stored).toContain(title);

    // The card's column is `display: none`-d by `applyFilters` on
    // the same tick `hideFilm` writes to localStorage.
    const display = await page.evaluate((t) =>
      (document.querySelector(`.col[data-title="${t}"]`) as HTMLElement | null)?.style.display,
      title!,
    );
    expect(display).toBe('none');
  });

  test('hidden titles appear in the Ukryte filmy modal list', async ({ page }) => {
    // Seed three hidden titles directly so we don't depend on the
    // ✕-button click flow. Reload picks them up.
    const seeded = ['Avatar', 'Cars', 'Diabeł ubiera się u Prady 2'];
    await setLocalStorageJson(page, 'hiddenFilms', seeded);
    await page.reload();

    await page.evaluate(() =>
      (globalThis as { openHiddenModal?: () => void }).openHiddenModal?.(),
    );

    const items = await page.evaluate(() =>
      [...document.querySelectorAll<HTMLElement>('#hidden-modal-list .panel-item')]
        .map((i) => i.textContent?.trim() ?? ''),
    );
    expect(items.sort()).toEqual([...seeded].sort());
  });

  test('unhiding via the modal returns the card to the grid', async ({ page }) => {
    const title = await firstVisibleTitle(page);
    expect(title).toBeTruthy();

    // Seed the card as hidden, reload, confirm hidden, then unhide
    // via the inline JS the modal's "Pokaż" buttons route to.
    await setLocalStorageJson(page, 'hiddenFilms', [title!]);
    await page.reload();
    await pinDateFilterAnytime(page);

    await page.evaluate((t) => {
      const col = document.querySelector(`.col[data-title="${t}"]`) as HTMLElement;
      expect(col.style.display).toBe('none');
    }, title!).catch(() => { /* expect not available inside evaluate; check next line */ });
    expect(await page.evaluate((t) =>
      (document.querySelector(`.col[data-title="${t}"]`) as HTMLElement | null)?.style.display,
      title!,
    )).toBe('none');

    await page.evaluate((t) => {
      (globalThis as { restoreFilm?: (title: string) => void }).restoreFilm?.(t);
    }, title!);

    expect(await page.evaluate((t) =>
      (document.querySelector(`.col[data-title="${t}"]`) as HTMLElement | null)?.style.display,
      title!,
    )).not.toBe('none');

    const stillHidden = (await getLocalStorageJson<string[]>(page, 'hiddenFilms')) ?? [];
    expect(stillHidden).not.toContain(title);
  });
});
