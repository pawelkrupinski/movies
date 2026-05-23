import { test, expect } from '@playwright/test';
import {
  firstVisibleTitle,
  getLocalStorageJson,
  pinDateFilterAnytime,
  setLocalStorageJson,
} from './helpers';

// `/film?title=…` carries its own ★ button (the same `.fav-poster-btn`
// shape as the listing card). The page has an inline `toggleFavMovie`
// reading the title from `data-title` on `.poster-wrap`; localStorage
// uses the same `favouriteMovies` key as the listing flow, so a film
// starred from /film shows up on /ulubione round-trip.

test.describe('/film detail page favourite ★', () => {

  async function gotoFirstFilm(page: import('@playwright/test').Page): Promise<string> {
    await page.goto('/');
    await pinDateFilterAnytime(page);
    const title = await firstVisibleTitle(page);
    expect(title).toBeTruthy();
    await page.goto(`/film?title=${encodeURIComponent(title!)}`);
    return title!;
  }

  test('clicking the ★ writes the title to favouriteMovies localStorage', async ({ page }) => {
    const title = await gotoFirstFilm(page);

    await page.evaluate(() => {
      const btn = document.querySelector('.fav-poster-btn') as HTMLButtonElement;
      (globalThis as { toggleFavMovie?: (el: HTMLElement) => void }).toggleFavMovie?.(btn);
    });

    const favs = (await getLocalStorageJson<string[]>(page, 'favouriteMovies')) ?? [];
    expect(favs).toContain(title);
    await expect(page.locator('.fav-poster-btn')).toHaveClass(/is-fav/);
  });

  test('reloading /film with a seeded favourite paints .is-fav on boot', async ({ page }) => {
    const title = await gotoFirstFilm(page);

    // Seed BEFORE the reload — the DOMContentLoaded handler on /film
    // reads `favouriteMovies` and paints the button.
    await setLocalStorageJson(page, 'favouriteMovies', [title]);
    await page.reload();

    await expect(page.locator('.fav-poster-btn')).toHaveClass(/is-fav/);
  });
});
