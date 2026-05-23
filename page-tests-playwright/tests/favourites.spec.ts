import { test, expect } from '@playwright/test';
import {
  firstVisibleTitle,
  getLocalStorageJson,
  getVisibleTitles,
  pinDateFilterAnytime,
  setLocalStorageJson,
  waitForCards,
} from './helpers';

// Favourites flow:
//   - Toggling the ★ on a poster persists to `localStorage`
//     under `favouriteMovies` and lights the button up.
//   - /ulubione filters the visible cards to only the favourited
//     ones (plus any whose individual screenings are favourited
//     under `favouriteScreenings`).

test.describe('favourites', () => {

  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await pinDateFilterAnytime(page);
  });

  test('toggling poster ★ writes to favouriteMovies localStorage', async ({ page }) => {
    const title = await firstVisibleTitle(page);
    expect(title).toBeTruthy();

    // The poster ★ button isn't a normal click target — it's small
    // and overlaid on the link. Easiest path: call the inline JS
    // `toggleFavMovie` directly with the card's ★ button as the
    // argument, same as the delegated click handler does.
    await page.evaluate((t) => {
      const card = document.querySelector(`.col[data-title="${t}"] .card`)!;
      const btn = card.querySelector('.fav-poster-btn') as HTMLButtonElement;
      (globalThis as { toggleFavMovie?: (el: HTMLElement) => void }).toggleFavMovie?.(btn);
    }, title!);

    const favs = (await getLocalStorageJson<string[]>(page, 'favouriteMovies')) ?? [];
    expect(favs).toContain(title);

    // Re-paint after the toggle adds the `.is-fav` class on the
    // poster button.
    const isFav = await page.evaluate((t) =>
      document
        .querySelector(`.col[data-title="${t}"] .fav-poster-btn`)
        ?.classList.contains('is-fav'),
      title!,
    );
    expect(isFav).toBe(true);
  });

  test('a second toggle removes the film from favouriteMovies', async ({ page }) => {
    const title = await firstVisibleTitle(page);
    expect(title).toBeTruthy();

    await page.evaluate((t) => {
      const btn = document.querySelector(
        `.col[data-title="${t}"] .fav-poster-btn`,
      ) as HTMLButtonElement;
      const tog = (globalThis as { toggleFavMovie?: (el: HTMLElement) => void }).toggleFavMovie;
      tog?.(btn);
      tog?.(btn);
    }, title!);

    const favs = (await getLocalStorageJson<string[]>(page, 'favouriteMovies')) ?? [];
    expect(favs).not.toContain(title);
  });
});

test.describe('/ulubione page', () => {

  test('renders only the favourited cards when localStorage is seeded', async ({ page }) => {
    // Seed two random titles from the fixture corpus as favourites,
    // then navigate to /ulubione. The page's inline `applyFilters`
    // with `IS_FAVOURITES_PAGE = true` will hide non-favourited cards.
    await page.goto('/');
    const titles = (await getVisibleTitles(page)).slice(0, 2);
    expect(titles).toHaveLength(2);
    await setLocalStorageJson(page, 'favouriteMovies', titles);

    await page.goto('/ulubione');
    await waitForCards(page);

    const visibleTitles = await getVisibleTitles(page);
    // `/ulubione` should narrow to the seeded set (order may differ).
    expect(visibleTitles.sort()).toEqual([...titles].sort());
  });
});
