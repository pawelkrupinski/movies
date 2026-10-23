import { test, expect } from '@playwright/test';
import { firstVisibleCard, gotoAndWaitForCards, pinDateFilterAnytime, setLocalStorageJson } from './helpers';
import type { Page } from '@playwright/test';
import { readFileSync } from 'node:fs';
import { join } from 'node:path';

// A real 2:3 poster (TMDB w185) the held poster requests are answered with.
const FixturePoster = join(__dirname, '..', '..', 'test', 'resources', 'fixtures', 'identity-posters', 'tmdb-w185-5HArseDNgLicwmq5V2YmnCWE5eo.jpg');

// `/poznan/movie/{slug}` detail page. Walks from a card on `/` to its
// detail screen and asserts the page's content blocks render +
// trailer interaction works.

test.describe('/movie detail page', { tag: '@agnostic' }, () => {

  // Helper: navigate to /movie for the first visible card on /
  async function gotoFirstFilm(page: Page): Promise<string> {
    await gotoAndWaitForCards(page, '/poznan/');
    await pinDateFilterAnytime(page);
    const card  = await firstVisibleCard(page);
    const title = card?.title;
    const slug  = card?.slug;
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

  // Switch `names` (display names) off in Filtry the way the visitor's own
  // selection does — `disabledCinemas` in localStorage — and re-run the page's
  // filter pass so the showings tree and the cinema-link row follow.
  async function disableCinemas(page: Page, names: Array<string | undefined>): Promise<void> {
    await setLocalStorageJson(page, 'disabledCinemas', names);
    await page.evaluate(() => (window as unknown as { applyFilters: () => void }).applyFilters());
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

  test('clicking the poster opens it full-screen; ✕ or a click anywhere closes it', async ({ page }) => {
    await gotoFirstFilm(page);

    const poster   = page.locator('.poster-img');
    const lightbox = page.locator('#poster-lightbox');
    if (await poster.count() === 0) {
      test.info().annotations.push({ type: 'note', description: 'No poster on this film' });
      return;
    }
    await expect(lightbox).toBeHidden();

    await poster.click();
    await expect(lightbox).toBeVisible();
    // The overlay covers the whole viewport, phone or desktop alike.
    const viewport = page.viewportSize()!;
    const box = (await lightbox.boundingBox())!;
    expect(box.width).toBeGreaterThanOrEqual(viewport.width - 1);
    expect(box.height).toBeGreaterThanOrEqual(viewport.height - 1);

    await page.locator('.poster-lightbox-close').click();
    await expect(lightbox).toBeHidden();

    await poster.click();
    await expect(lightbox).toBeVisible();
    // A click on the backdrop corner, well away from the ✕ and the image.
    await lightbox.click({ position: { x: 5, y: box.height - 5 } });
    await expect(lightbox).toBeHidden();
  });

  test('the full-screen poster is modal: focus moves into it, the page behind is inert, Escape closes it', async ({ page }) => {
    await gotoFirstFilm(page);

    const poster   = page.locator('.poster-img');
    const lightbox = page.locator('#poster-lightbox');
    if (await poster.count() === 0) {
      test.info().annotations.push({ type: 'note', description: 'No poster on this film' });
      return;
    }
    const focusIsInside = () => page.evaluate(() => !!document.activeElement?.closest('#poster-lightbox'));

    await poster.click();
    await expect(lightbox).toBeVisible();
    expect(await focusIsInside()).toBe(true);
    await page.evaluate(() => (document.querySelector('a.back-link') as HTMLElement).focus());
    expect(await focusIsInside()).toBe(true);

    await page.keyboard.press('Escape');
    await expect(lightbox).toBeHidden();
  });

  test('the other-cities popup is modal: focus moves in, returns to its trigger on close, Escape and the backdrop close it', async ({ page }) => {
    // The fixture server's film page with two sibling-city links (only Poznań
    // carries a corpus, so no corpus film shows anywhere else).
    await page.goto('/poznan/movie-other-cities', { waitUntil: 'domcontentloaded' });

    const trigger = page.locator('.other-cities-trigger');
    const card    = page.locator('.other-cities-modal');
    const focusIsInside = () => page.evaluate(() => !!document.activeElement?.closest('.other-cities-modal'));
    await expect(card).toBeHidden();

    // Opened from the keyboard: focus can only go back to a trigger that had
    // it, and WebKit (Safari) never focuses a button on a mouse click.
    await trigger.focus();
    await page.keyboard.press('Enter');
    await expect(card).toBeVisible();
    expect(await focusIsInside()).toBe(true);
    await page.evaluate(() => (document.querySelector('a.back-link') as HTMLElement).focus());
    expect(await focusIsInside()).toBe(true);

    await page.keyboard.press('Escape');
    await expect(card).toBeHidden();
    await expect(trigger).toBeFocused();

    await trigger.click();
    await expect(card).toBeVisible();
    await card.locator('.login-modal-title').click();
    await expect(card).toBeVisible();
    await page.mouse.click(5, 5);
    await expect(card).toBeHidden();
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
  // /movie means adding a row here AND its path → label branch in
  // film.scala.html's back-link rewrite.
  for (const { from, label } of [
    { from: '/poznan/',      label: 'Poznań' },
    { from: '/poznan/movies', label: 'Poznań' },
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
    await expect(page.locator('.cinema-link:visible')).toHaveCount(10);

    const more = page.locator('.cinema-links .cinemas-more');
    await expect(more).toBeVisible();
    await more.click();

    await expect(page.locator('.cinema-link:visible')).toHaveCount(12);
    await expect(more).toBeHidden();
    // The pill fold owns `.folded`; the filter owns inline display. Unfolding
    // the pills leaves the showings tree exactly as the filter left it.
    await expect(page.locator('.cinema-group:visible')).toHaveCount(12);
  });

  // The poster sits ABOVE the cinema-link pills on a phone, and the page is
  // usable from DOMContentLoaded — long before a weserv-proxied poster lands.
  // Its box has to be reserved from the start: an `<img>` with no size takes
  // 0px until it decodes, then shoves everything below it ~450px down. A tap
  // in flight at that moment lands on whatever slid under the finger — the CI
  // flake where the fold button took focus on mousedown but the click
  // never reached it, leaving the pills folded (Main run 38422471456).
  //
  // The same slot has to hold when the poster NEVER lands: the onerror chain
  // running dry hides the `<img>` and shows the `.no-poster` placeholder, which
  // must take the poster's place — not collapse the slot (the pills jumping UP
  // ~450px) nor overlay the title and pills (it is absolutely positioned).
  //
  // Every off-site image request (the poster, proxied or not, and each of its
  // fallbacks) is held until `release`, then answered from a poster on disk —
  // or failed — so the test never reaches a real image host.
  async function holdPosters(page: Page, outcome: 'load' | 'fail'): Promise<() => void> {
    let release!: () => void;
    const held = new Promise<void>((resolve) => { release = resolve; });
    const site = new URL(test.info().project.use.baseURL!).origin;
    await page.route((url) => url.origin !== site, async (route) => {
      if (route.request().resourceType() !== 'image') return route.continue();
      await held;
      if (outcome === 'fail') return route.abort();
      return route.fulfill({ contentType: 'image/jpeg', body: readFileSync(FixturePoster) });
    });
    return release;
  }

  // Where the poster slot's boxes sit, and whether the visible one (the image,
  // or the placeholder standing in for it) overlaps the title or the pills.
  async function posterLayout(page: Page) {
    return page.evaluate(() => {
      const box = (sel: string) => {
        const el = document.querySelector(sel) as HTMLElement | null;
        return el && getComputedStyle(el).display !== 'none' ? el.getBoundingClientRect() : null;
      };
      const wrap  = box('.poster-wrap')!;
      const shown = box('.poster-wrap img') ?? box('.poster-wrap .no-poster');
      const title = box('.film-title')!;
      const pills = box('.cinema-links')!;
      const overlaps = (a: DOMRect, b: DOMRect) =>
        a.left < b.right - 1 && b.left < a.right - 1 && a.top < b.bottom - 1 && b.top < a.bottom - 1;
      return {
        wrapWidth: wrap.width, wrapHeight: wrap.height,
        shown: shown && { width: shown.width, height: shown.height },
        titleTop: title.top, pillsTop: pills.top,
        overlapsTitle: !!shown && overlaps(shown, title),
        overlapsPills: !!shown && overlaps(shown, pills),
      };
    });
  }

  function expectReservedSlot(layout: Awaited<ReturnType<typeof posterLayout>>) {
    expect(layout.wrapWidth).toBeGreaterThan(100);
    expect(layout.wrapHeight).toBeCloseTo(layout.wrapWidth * 1.5, 0);
    expect(layout.overlapsTitle).toBe(false);
    expect(layout.overlapsPills).toBe(false);
  }

  test('the cinema-link pills do not move when the poster arrives', async ({ page }) => {
    const release = await holdPosters(page, 'load');

    await page.goto('/poznan/movie-many', { waitUntil: 'domcontentloaded' });
    const poster = page.locator('.poster-img');
    const before = await posterLayout(page);
    expectReservedSlot(before);

    release();
    await expect.poll(() => poster.evaluate((img: HTMLImageElement) => img.complete && img.naturalWidth > 0)).toBe(true);
    const after = await posterLayout(page);

    expectReservedSlot(after);
    expect(after.shown).toEqual({ width: after.wrapWidth, height: after.wrapHeight });
    expect(after.pillsTop).toBeCloseTo(before.pillsTop, 0);
    expect(after.titleTop).toBeCloseTo(before.titleTop, 0);
  });

  test('a poster that fails to load leaves its placeholder in the slot, over neither the title nor the pills', async ({ page }) => {
    const release = await holdPosters(page, 'fail');

    await page.goto('/poznan/movie-many', { waitUntil: 'domcontentloaded' });
    const before = await posterLayout(page);
    expectReservedSlot(before);

    release();
    // The whole fallback chain fails in turn, then the placeholder shows.
    await expect(page.locator('.poster-wrap .no-poster')).toBeVisible();
    const after = await posterLayout(page);

    expectReservedSlot(after);
    expect(after.shown).toEqual({ width: after.wrapWidth, height: after.wrapHeight });
    expect(after.pillsTop).toBeCloseTo(before.pillsTop, 0);
    expect(after.titleTop).toBeCloseTo(before.titleTop, 0);
  });

  test('a film with no poster shows its placeholder in the slot, over neither the title nor the pills', async ({ page }) => {
    await page.goto('/poznan/movie-no-poster', { waitUntil: 'domcontentloaded' });
    await expect(page.locator('.poster-wrap .no-poster')).toBeVisible();
    await expect(page.locator('.poster-wrap img')).toHaveCount(0);
    const layout = await posterLayout(page);

    expectReservedSlot(layout);
    expect(layout.shown).toEqual({ width: layout.wrapWidth, height: layout.wrapHeight });
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
    await disableCinemas(page, off);

    // Eight left, all under the ten-pill cap, so nothing folds and the button
    // retires rather than offering rows that are no longer there.
    await expect(page.locator('.cinema-link:visible')).toHaveCount(8);
    await expect(page.locator('.cinema-links .cinemas-more')).toBeHidden();
    const shown = await page.locator('.cinema-link:visible')
      .evaluateAll((els) => els.map((e) => (e as HTMLElement).dataset.cinema));
    for (const name of off) expect(shown).not.toContain(name);
  });

  test('the showings tree shows every cinema, minus the ones switched off in Filtry', async ({ page }) => {
    await page.goto('/poznan/movie-many', { waitUntil: 'domcontentloaded' });

    await expect(page.locator('.cinema-group')).toHaveCount(12);
    await expect(page.locator('.cinema-group:visible')).toHaveCount(12);
    await expect(page.locator('.date-group .cinemas-more')).toHaveCount(0);

    const off = (await page.locator('.cinema-group[data-cinema]')
      .evaluateAll((els) => els.map((e) => (e as HTMLElement).dataset.cinema))).slice(0, 4);
    await disableCinemas(page, off);

    await expect(page.locator('.cinema-group:visible')).toHaveCount(8);
    await expect(page.locator('#showings-empty')).toBeHidden();
  });

  test('switching every cinema off empties the showings section, date headers and all', async ({ page }) => {
    await page.goto('/poznan/movie-many', { waitUntil: 'domcontentloaded' });

    const all = await page.locator('.cinema-group[data-cinema]')
      .evaluateAll((els) => els.map((e) => (e as HTMLElement).dataset.cinema));
    await disableCinemas(page, all);

    await expect(page.locator('.cinema-group:visible')).toHaveCount(0);
    await expect(page.locator('.date-group:visible')).toHaveCount(0);
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
  // A showtime tag says only what separates that slot from the rest of the
  // film's card — a screen format and a language version alike. Six pills all
  // saying `2D DUB` distinguish nothing, so a film shown only dubbed here has
  // bare times; the moment two slots differ — two times, two cinemas or two
  // days — the version is what tells them apart and stays on every pill.
  test('a showtime tag says only what separates it from the rest of its card', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    await pinDateFilterAnytime(page);

    const { wrong, versionsDropped, versionsShown, mixedFilms } = await page.evaluate(() => {
      const VERSIONS = new Set(['NAP', 'DUB', 'LEK', 'ORG']);
      const wrong: string[] = [];
      let versionsDropped = 0;
      let versionsShown = 0;
      let mixedFilms = 0;
      for (const card of document.querySelectorAll<HTMLElement>('.col[data-title]')) {
        const badges = [...card.querySelectorAll<HTMLElement>('.badge-time')];
        const formats = badges
          .map((b) => (b.getAttribute('data-format') || '').split(' ').filter(Boolean))
          .filter((f) => f.length > 0);
        if (formats.length === 0) continue;
        // The tokens EVERY slot of this film carries — what a pill may drop.
        const common = formats.reduce((acc, f) => acc.filter((t) => f.includes(t)));
        const filmVersions = new Set(formats.flatMap((f) => f.filter((t) => VERSIONS.has(t))));
        if (filmVersions.size > 1) mixedFilms++;
        for (const badge of badges) {
          const format = (badge.getAttribute('data-format') || '').split(' ').filter(Boolean);
          const shown = (badge.querySelector('.badge-fmt')?.textContent || '').split(' ').filter(Boolean);
          const expected = format.filter((t) => !common.includes(t));
          if (shown.join(' ') !== expected.join(' ')) {
            wrong.push(`${card.getAttribute('data-title')} ${(badge.firstChild?.nodeValue ?? '').trim()}: shown "${shown.join(' ')}" want "${expected.join(' ')}"`);
          }
          // A film screened more than one way must say so on every pill that
          // carries a version — that is the whole point of keeping the tag.
          if (filmVersions.size > 1) {
            for (const t of format) {
              if (VERSIONS.has(t) && !shown.includes(t)) {
                wrong.push(`${card.getAttribute('data-title')} ${(badge.firstChild?.nodeValue ?? '').trim()}: lost ${t} on a mixed film`);
              }
            }
          }
          versionsShown += shown.filter((t) => VERSIONS.has(t)).length;
        }
        versionsDropped += common.filter((t) => VERSIONS.has(t)).length;
      }
      return { wrong: wrong.slice(0, 10), versionsDropped, versionsShown, mixedFilms };
    });
    expect(wrong).toEqual([]);

    // The check above is only worth anything if the page exercises both halves
    // of the rule: films uniform in their version, and films that are not.
    expect(versionsDropped).toBeGreaterThan(10);
    expect(versionsShown).toBeGreaterThan(10);
    expect(mixedFilms).toBeGreaterThan(0);
  });

});
