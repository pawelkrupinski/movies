import { test, expect } from '@playwright/test';
import { firstVisibleTitle, gotoAndWaitForCards, pinDateFilterAnytime } from './helpers';

// `/poznan/film?title=...` detail page. Walks from a card on `/` to its
// detail screen and asserts the page's content blocks render +
// trailer interaction works.

test.describe('/film detail page', () => {

  // Helper: navigate to /film for the first visible card on /
  async function gotoFirstFilm(page: import('@playwright/test').Page): Promise<string> {
    await gotoAndWaitForCards(page, '/poznan/');
    await pinDateFilterAnytime(page);
    const title = await firstVisibleTitle(page);
    expect(title).toBeTruthy();
    // `waitUntil: 'domcontentloaded'` for the same reason `gotoAndWaitForCards`
    // does it: the detail page's content blocks + inline boot scripts are all
    // present at DCL, but the default `'load'` wait blocks `goto` on the poster
    // proxy images (and any trailer iframe). On a contended CI runner that
    // image-load stall eats the whole 30s budget and times the navigation out.
    await page.goto(`/poznan/film?title=${encodeURIComponent(title!)}`, { waitUntil: 'domcontentloaded' });
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

  test('the ← back link navigates to the page the user came from', async ({ page }) => {
    await gotoFirstFilm(page);
    await page.locator('a.back-link').click();
    // gotoFirstFilm started at `/` (with the date pin applied), so the
    // referrer-driven rewrite sends the user back to `/` — any preserved
    // query string is fine, we just need the pathname to be the listing.
    await page.waitForURL((u) => new URL(u).pathname === '/poznan/');
    await page.waitForSelector('.col[data-title]', { state: 'attached' });
  });

  // Parameterised across the navbar destinations so the label + href
  // pair stays in lockstep — adding a new section that links into
  // /film means adding a row here AND the path → label entry in
  // film.scala.html's `LABELS` map.
  for (const { from, label } of [
    { from: '/poznan/',      label: 'Filmy' },
    { from: '/poznan/filmy', label: 'Filmy' },
    { from: '/poznan/kina',  label: 'Kina'  },
    { from: '/poznan/plan',  label: 'Plan'  },
  ]) {
    test(`the ← back link reads "${label}" and returns to ${from} when that was the referrer`, async ({ page }) => {
      // Land on the source page first so document.referrer is set when
      // /film loads. Pull a title off the embedded grid (homepage cards
      // OR /plan picker cards both carry `data-title`) and navigate via
      // `window.location.href = …` so the browser writes a real
      // Referer header — `page.goto(referer: …)` would also work but
      // this exercises the same code path a user's card-tap takes.
      await page.goto(from, { waitUntil: 'domcontentloaded' });
      const filmHref = await page.evaluate(() => {
        const col = document.querySelector('[data-title]');
        if (!col) return null;
        const title = (col as HTMLElement).dataset.title;
        return title ? `/poznan/film?title=${encodeURIComponent(title)}` : null;
      });
      expect(filmHref).not.toBeNull();
      await Promise.all([
        // `waitUntil: 'domcontentloaded'` — same reasoning as `gotoFirstFilm`:
        // the `/film` page's `load` event is gated on poster-proxy images and
        // the trailer iframe, which can stall the full 30s on a contended
        // runner. The back-link markup we assert on is server-rendered, so DCL
        // is all we need.
        page.waitForURL((u) => new URL(u).pathname === '/poznan/film', { waitUntil: 'domcontentloaded' }),
        page.evaluate((href) => { window.location.href = href; }, filmHref!),
      ]);

      await expect(page.locator('#back-link-label')).toHaveText(label);
      const href = await page.locator('#back-link').getAttribute('href');
      expect(href).not.toBeNull();
      expect(new URL(href!, page.url()).pathname).toBe(from);
    });
  }

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
