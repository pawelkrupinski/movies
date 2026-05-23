import { test, expect } from '@playwright/test';
import { firstVisibleTitle, pinDateFilterAnytime } from './helpers';

// `/film?title=...` detail page. Walks from a card on `/` to its
// detail screen and asserts the page's content blocks render +
// trailer interaction works.

test.describe('/film detail page', () => {

  // Helper: navigate to /film for the first visible card on /
  async function gotoFirstFilm(page: import('@playwright/test').Page): Promise<string> {
    await page.goto('/');
    await pinDateFilterAnytime(page);
    const title = await firstVisibleTitle(page);
    expect(title).toBeTruthy();
    await page.goto(`/film?title=${encodeURIComponent(title!)}`);
    return title!;
  }

  test('renders the film title and Seanse heading', async ({ page }) => {
    const title = await gotoFirstFilm(page);

    // Title shows up in two places: og:title meta and a visible
    // `.film-title` block. We pin the visible one to avoid a meta-
    // tag-only happy path on a broken render.
    await expect(page.locator('.film-title')).toContainText(title);
    await expect(page.locator('.showtimes-section h2')).toContainText('Seanse');
  });

  test('cinema-link buttons (when present) point at external cinema pages', async ({ page }) => {
    await gotoFirstFilm(page);

    const links = page.locator('a.cinema-link');
    const count = await links.count();
    if (count === 0) {
      // Some films don't have any cinemaFilmUrls in the fixture
      // (`MovieRecord.cinemaFilmUrls` empty). Smoke-pass without
      // asserting on a missing block — the absence is itself valid.
      test.info().annotations.push({ type: 'note', description: 'No .cinema-link buttons on this film' });
      return;
    }
    // Every cinema-link opens in a new tab + points at an external
    // (non-relative) URL. Cinema CDN URLs aren't on our origin.
    for (let i = 0; i < count; i++) {
      const a = links.nth(i);
      await expect(a).toHaveAttribute('target', '_blank');
      const href = await a.getAttribute('href');
      expect(href).toMatch(/^https?:\/\//);
    }
  });

  test('clicking a trailer link reveals the iframe; clicking again hides it', async ({ page }) => {
    await gotoFirstFilm(page);

    const trailer = page.locator('button.trailer-link').first();
    if (await trailer.count() === 0) {
      test.info().annotations.push({ type: 'note', description: 'No trailer buttons on this film' });
      return;
    }

    await expect(page.locator('#trailer-frame')).toHaveCSS('display', 'none');
    await trailer.click();
    await expect(page.locator('#trailer-frame')).not.toHaveCSS('display', 'none');
    await expect(trailer).toHaveClass(/active/);

    // The src of the iframe is set + carries `autoplay=1` per
    // `playTrailer`'s logic.
    const src = await page.locator('#trailer-iframe').getAttribute('src');
    expect(src).toMatch(/autoplay=1/);

    // Click the active button again — frame hides, src clears.
    await trailer.click();
    await expect(page.locator('#trailer-frame')).toHaveCSS('display', 'none');
    expect(await page.locator('#trailer-iframe').getAttribute('src')).toBe('');
  });

  test('the ← Repertuar back link navigates to /', async ({ page }) => {
    await gotoFirstFilm(page);
    await page.locator('a.back-link').click();
    await page.waitForURL(/.*\/$/);
    // Sanity that we landed on the listing rather than a 404 stub.
    await page.waitForSelector('.col[data-title]', { state: 'attached' });
  });

  test('detail page renders without a JS error', async ({ page }) => {
    const errors: string[] = [];
    page.on('pageerror', (e) => errors.push(e.message));
    await gotoFirstFilm(page);
    // film.scala.html's inline `toggleFavMovie` + `playTrailer` blocks
    // run on DOMContentLoaded; an undefined reference there would
    // surface in `errors`.
    expect(errors).toEqual([]);
  });
});
