import { test, expect } from '@playwright/test';
import { firstVisibleSlug, firstVisibleTitle, gotoAndWaitForCards, pinDateFilterAnytime } from './helpers';

// `/poznan/movie/{slug}` detail page. Walks from a card on `/` to its
// detail screen and asserts the page's content blocks render +
// trailer interaction works.

test.describe('/movie detail page', { tag: '@agnostic' }, () => {

  // Helper: navigate to /movie for the first visible card on /
  async function gotoFirstFilm(page: import('@playwright/test').Page): Promise<string> {
    await gotoAndWaitForCards(page, '/poznan/');
    await pinDateFilterAnytime(page);
    const title = await firstVisibleTitle(page);
    const slug  = await firstVisibleSlug(page);
    expect(title).toBeTruthy();
    expect(slug).toBeTruthy();
    // `waitUntil: 'domcontentloaded'` for the same reason `gotoAndWaitForCards`
    // does it: the detail page's content blocks + inline boot scripts are all
    // present at DCL, but the default `'load'` wait blocks `goto` on the poster
    // proxy images (and any trailer iframe). On a contended CI runner that
    // image-load stall eats the whole 30s budget and times the navigation out.
    await page.goto(`/poznan/movie/${slug}`, { waitUntil: 'domcontentloaded' });
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
    const source = await page.locator('#trailer-iframe').getAttribute('src');
    expect(source).toMatch(/autoplay=1/);

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
  // /movie means adding a row here AND the path → label entry in
  // film.scala.html's `LABELS` map.
  for (const { from, label } of [
    { from: '/poznan/',      label: 'Filmy' },
    { from: '/poznan/movies', label: 'Filmy' },
  ]) {
    test(`the ← back link reads "${label}" and returns to ${from} when that was the referrer`, async ({ page }) => {
      // Land on the source page first so document.referrer is set when
      // /movie loads. Pull a title off the embedded grid (the homepage
      // cards carry `data-title`) and navigate via
      // `window.location.href = …` so the browser writes a real
      // Referer header — `page.goto(referer: …)` would also work but
      // this exercises the same code path a user's card-tap takes.
      await page.goto(from, { waitUntil: 'domcontentloaded' });
      const filmHref = await page.evaluate(() => {
        const col = document.querySelector('[data-title]');
        if (!col) return null;
        const slug = (col as HTMLElement).dataset.slug;
        return slug ? `/poznan/movie/${slug}` : null;
      });
      expect(filmHref).not.toBeNull();
      await Promise.all([
        // `waitUntil: 'domcontentloaded'` — same reasoning as `gotoFirstFilm`:
        // the `/movie` page's `load` event is gated on poster-proxy images and
        // the trailer iframe, which can stall the full 30s on a contended
        // runner. The back-link markup we assert on is server-rendered, so DCL
        // is all we need.
        page.waitForURL((u) => new URL(u).pathname.startsWith('/poznan/movie/'), { waitUntil: 'domcontentloaded' }),
        page.evaluate((href) => { window.location.href = href; }, filmHref!),
      ]);

      await expect(page.locator('#back-link-label')).toHaveText(label);
      const href = await page.locator('#back-link').getAttribute('href');
      expect(href).not.toBeNull();
      expect(new URL(href!, page.url()).pathname).toBe(from);
    });
  }

  test('a long unbreakable token in the synopsis keeps the details beside the poster', async ({ page }) => {
    // Force a desktop-width viewport so the poster/details flex row is
    // side-by-side (below 575px it stacks by design). Reproduces the reported
    // "Orły Republiki synopsis renders under the poster" bug: a 300+ char
    // URL-like run with no break opportunities used to set a min-content width
    // on the `.col` details column wider than the whole row, wrapping it below
    // the poster. `.meta-value { overflow-wrap: anywhere }` lets it break.
    await page.setViewportSize({ width: 1280, height: 900 });
    await gotoFirstFilm(page);

    await page.evaluate(() => {
      const col = document.querySelector('.film-row > .col');
      const div = document.createElement('div');
      div.className = 'meta-value';
      div.id = 'injected-synopsis';
      div.textContent = 'https://www.youtube.com/watch?v=' + 'A'.repeat(320);
      col!.prepend(div);
    });

    const poster  = await page.locator('.film-row > .col-auto').boundingBox();
    const details = await page.locator('.film-row > .col').boundingBox();
    expect(poster).not.toBeNull();
    expect(details).not.toBeNull();
    // Side-by-side: the details column starts to the right of the poster. When
    // the column wraps below (the bug) its x collapses back to the row's left
    // edge, ≈ the poster's x, so this guard fails.
    expect(details!.x).toBeGreaterThan(poster!.x + poster!.width - 1);
  });

  // A big-city film plays 60+ venues a day, so both of the page's cinema lists
  // — the link pills under the title and each date's cinemas in the showings
  // tree — open at ten and fold the rest behind a button. `/movie-many` is the
  // fixture server's 12-cinema render; no corpus film reaches the threshold.
  test('cinema-link pills past the tenth fold away until the button is clicked', async ({ page }) => {
    await page.goto('/poznan/movie-many', { waitUntil: 'domcontentloaded' });

    await expect(page.locator('.cinema-link')).toHaveCount(12);
    expect(await page.locator('.cinema-link:visible').count()).toBe(10);

    const more = page.locator('.cinema-links .cinemas-more');
    await expect(more).toBeVisible();
    await more.click();

    expect(await page.locator('.cinema-link:visible').count()).toBe(12);
    await expect(more).toBeHidden();
    // The pill fold owns `.folded`; the filter owns inline display. Unfolding
    // the pills leaves the showings tree exactly as the filter left it.
    expect(await page.locator('.cinema-group:visible').count()).toBe(12);
  });

  // The showings tree does NOT fold — it renders every cinema, and what
  // narrows it is the visitor's own Filtry selection, applied by the page's
  // `applyFilters` off shared.js's `disabledCinemas` (a list of DISPLAY names).
  // The pills answer to Filtry too, and the fold counts only what survives it:
  // "the first ten" must mean ten a visitor can actually use, not slots 1-10 of
  // a list they have mostly switched off.
  test('the cinema-link row shows only cinemas that are filtered in', async ({ page }) => {
    await page.goto('/poznan/movie-many', { waitUntil: 'domcontentloaded' });

    const off = (await page.locator('.cinema-link[data-cinema]')
      .evaluateAll((els) => els.map((e) => (e as HTMLElement).dataset.cinema))).slice(0, 4);
    await page.evaluate((names) => {
      localStorage.setItem('disabledCinemas', JSON.stringify(names));
      (window as unknown as { applyFilters: () => void }).applyFilters();
    }, off);

    // Eight left, all under the ten-pill cap, so nothing folds and the button
    // retires rather than offering rows that are no longer there.
    expect(await page.locator('.cinema-link:visible').count()).toBe(8);
    await expect(page.locator('.cinema-links .cinemas-more')).toBeHidden();
    const shown = await page.locator('.cinema-link:visible')
      .evaluateAll((els) => els.map((e) => (e as HTMLElement).dataset.cinema));
    for (const name of off) expect(shown).not.toContain(name);
  });

  test('the showings tree shows every cinema, minus the ones switched off in Filtry', async ({ page }) => {
    await page.goto('/poznan/movie-many', { waitUntil: 'domcontentloaded' });

    await expect(page.locator('.cinema-group')).toHaveCount(12);
    expect(await page.locator('.cinema-group:visible').count()).toBe(12);
    await expect(page.locator('.date-group .cinemas-more')).toHaveCount(0);

    const off = (await page.locator('.cinema-group[data-cinema]')
      .evaluateAll((els) => els.map((e) => (e as HTMLElement).dataset.cinema))).slice(0, 4);
    await page.evaluate((names) => {
      localStorage.setItem('disabledCinemas', JSON.stringify(names));
      (window as unknown as { applyFilters: () => void }).applyFilters();
    }, off);

    expect(await page.locator('.cinema-group:visible').count()).toBe(8);
    await expect(page.locator('#showings-empty')).toBeHidden();
  });

  test('switching every cinema off empties the showings section, date headers and all', async ({ page }) => {
    await page.goto('/poznan/movie-many', { waitUntil: 'domcontentloaded' });

    const all = await page.locator('.cinema-group[data-cinema]')
      .evaluateAll((els) => els.map((e) => (e as HTMLElement).dataset.cinema));
    await page.evaluate((names) => {
      localStorage.setItem('disabledCinemas', JSON.stringify(names));
      (window as unknown as { applyFilters: () => void }).applyFilters();
    }, all);

    expect(await page.locator('.cinema-group:visible').count()).toBe(0);
    expect(await page.locator('.date-group:visible').count()).toBe(0);
    await expect(page.locator('#showings-empty')).toBeVisible();
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
