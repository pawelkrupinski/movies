import type { Page } from '@playwright/test';

/**
 * Drag a real horizontal touch across `#film-grid` via CDP — the same path a
 * finger takes, so it actually drives the production swipe handlers. The
 * distance is 55% of the grid WIDTH (above the handler's 40% commit threshold)
 * and the start point is offset so the whole drag stays on-screen, so the
 * commit is decided by position rather than the CDP touch's unreliable
 * synthetic velocity, on any viewport. Chromium-only (CDP touch injection);
 * `dir` is 'left' (next day) / 'right' (previous day).
 */
export async function cdpSwipe(page: Page, dir: 'left' | 'right'): Promise<void> {
  const box = (await page.locator('#film-grid').boundingBox())!;
  const y    = box.y + Math.min(box.height / 2, 150);
  const dist = box.width * 0.55;
  const x0   = dir === 'left' ? box.x + box.width * 0.8 : box.x + box.width * 0.2;
  const dx   = dir === 'left' ? -dist : dist;
  const client = await page.context().newCDPSession(page);
  await client.send('Input.dispatchTouchEvent', { type: 'touchStart', touchPoints: [{ x: x0, y }] });
  const steps = 12;
  for (let i = 1; i <= steps; i++) {
    await client.send('Input.dispatchTouchEvent',
      { type: 'touchMove', touchPoints: [{ x: x0 + (dx * i) / steps, y }] });
  }
  await client.send('Input.dispatchTouchEvent', { type: 'touchEnd', touchPoints: [] });
  await client.detach();
}

/**
 * Pin the date filter to "anytime" so the visible-card set isn't a
 * function of the runner's wall-clock relative to the fixture's
 * recorded dates. Mirrors the Scala spec's `pinDateFilterAnytime`
 * helper for the same reason.
 */
export async function pinDateFilterAnytime(page: Page): Promise<void> {
  await setDateFilter(page, 'anytime');
}

/**
 * Drive the `#date-filter` `<select>` and trigger the page's
 * `onDateChange()` so the visible-cards set AND the URL's `?date=`
 * param reflect the new value before assertions run. Used by every
 * spec that needs deterministic showtime visibility —
 * `pinDateFilterAnytime` is just this with `value = 'anytime'`.
 */
export async function setDateFilter(page: Page, value: string): Promise<void> {
  await page.evaluate((v) => {
    const sel = document.getElementById('date-filter') as HTMLSelectElement | null;
    if (sel) {
      sel.value = v;
      const g = globalThis as unknown as { onDateChange?: () => void; applyFilters?: () => void };
      (g.onDateChange ?? g.applyFilters)?.();
    }
  }, value);
}

/**
 * Wait for the home listing to have rendered at least
 * one `.col[data-title]` card into the DOM. `state: 'attached'` is
 * deliberate: the page's inline `applyFilters` hides out-of-window
 * cards with `display:none` and shuffles them to the front of DOM
 * order, so the default `'visible'` check times out even with cards
 * present.
 */
export async function waitForCards(page: Page): Promise<void> {
  await page.waitForSelector('.col[data-title]', { state: 'attached' });
}

/**
 * Navigate to `url` and wait for the listing's cards to attach.
 *
 * `waitUntil: 'domcontentloaded'` is deliberate and load-bearing for
 * stability: the `.col[data-title]` cards and the navbar's `#date-filter`
 * are all SERVER-rendered, so they're in the DOM at DOMContentLoaded, and
 * the inline boot script that reads `?date=` runs during parse — none of
 * it needs the poster images. The default `waitUntil: 'load'` instead
 * blocks `goto` until every image has loaded; on a slow/contended CI
 * runner (seen on webkit-iphone-se-landscape) that stall can eat the whole
 * 30s test budget, so the *next* call — `waitForCards` — is the one that
 * trips the test timeout even though the cards are long since present.
 * Settling at DCL sidesteps the image-load wait entirely.
 */
export async function gotoAndWaitForCards(page: Page, url: string): Promise<void> {
  await page.goto(url, { waitUntil: 'domcontentloaded' });
  await waitForCards(page);
}

/**
 * Reload the page, settling at DOMContentLoaded.
 *
 * `page.reload()` defaults to `waitUntil: 'load'`, which blocks until every
 * poster image has loaded — the same stall `gotoAndWaitForCards` documents for
 * `goto`, and it bites harder here because a reload re-requests the whole
 * grid's posters at once. On a contended runner that ate the entire 30s test
 * budget (seen on `firefox-galaxy-s10-zoomed` › filtry-cinemas), and the
 * timeout points at the reload rather than at anything the test is asserting.
 *
 * Everything a spec reads after a reload — the cards, the navbar, the inline
 * boot script that restores `localStorage` state — is server-rendered or runs
 * during parse, so DCL is the honest settle point. Follow with `waitForCards`
 * where the cards themselves are the subject.
 */
export async function reload(page: Page): Promise<void> {
  await page.reload({ waitUntil: 'domcontentloaded' });
}

/**
 * Read a JSON value out of `localStorage`. Returns `null` if the key
 * is absent or holds invalid JSON. Generic so callers can narrow the
 * return type without an `as` cast at the call site.
 */
export async function getLocalStorageJson<T = unknown>(page: Page, key: string): Promise<T | null> {
  return page.evaluate((k) => {
    const raw = localStorage.getItem(k);
    if (!raw) return null;
    try {
      return JSON.parse(raw);
    } catch {
      return null;
    }
  }, key);
}

/**
 * Write `value` into `localStorage[key]` as JSON. Equivalent to
 * `localStorage.setItem(key, JSON.stringify(value))` inside a
 * `page.evaluate`.
 */
export async function setLocalStorageJson(page: Page, key: string, value: unknown): Promise<void> {
  await page.evaluate(
    ([k, v]) => localStorage.setItem(k as string, v as string),
    [key, JSON.stringify(value)],
  );
}

/**
 * Titles of every `.col[data-title]` card the page is currently
 * showing — `style.display !== 'none'` is the same predicate the
 * page's own filter pipeline applies. Order matches DOM order, which
 * the inline `applyFilters` keeps as "visible cards first, hidden
 * cards moved to the end".
 */
export async function getVisibleTitles(page: Page): Promise<string[]> {
  return page.evaluate(() =>
    [...document.querySelectorAll<HTMLElement>('.col[data-title]')]
      .filter((c) => c.style.display !== 'none')
      .map((c) => c.dataset.title!),
  );
}

/**
 * Ratio of one card column's width to the full grid width. Returns
 * ~0.5 for a 2-column layout, ~0.25 for 4 columns, ~1.0 for a single
 * column, etc. Temporarily forces `display: block` on the first `.col`
 * so the measurement is valid even if `applyFilters` hid it.
 */
export async function measureGridRatio(page: Page): Promise<number> {
  return page.evaluate(() => {
    const grid = document.querySelector('#film-grid') as HTMLElement;
    const col = grid?.querySelector(':scope > .col') as HTMLElement;
    if (!grid || !col) return -1;
    const prev = col.style.display;
    col.style.display = 'block';
    const r = col.getBoundingClientRect().width / grid.getBoundingClientRect().width;
    col.style.display = prev;
    return r;
  });
}

/** One listing card's identity, as the page itself spells both halves. */
export interface VisibleCard {
  /** `data-title` — the film's display title. */
  title: string;
  /** `data-slug` — the server's own `Slugify` output, and the path segment of
   *  the card's canonical `/{city}/movie/{slug}` address. Read from the DOM
   *  rather than re-implementing the fold in TypeScript: the fold handles
   *  Polish and German diacritics, ß, and Cyrillic, and a second copy of those
   *  rules would drift from the Scala one. */
  slug: string;
}

/**
 * In-page pick of the first card worth targeting: inline `style.display` isn't
 * `none`, and its `<img>` is still visible (i.e. the `onerror` fallback chain
 * didn't `display:none` it). `applyFilters` re-appends visible cards after
 * hidden ones, so `querySelector` lands on a hidden one — explicitly walking +
 * checking is engine-agnostic.
 *
 * With `settled`, the poster must also have FINISHED loading — see
 * `firstVisibleCard`.
 */
const firstVisibleCardIn = (settled: boolean): VisibleCard | null => {
  for (const c of document.querySelectorAll<HTMLElement>('.col[data-title]')) {
    if (c.style.display === 'none') continue;
    const img = c.querySelector<HTMLImageElement>('.poster-wrap > a img');
    if (!img || img.style.display === 'none') continue;
    if (settled && !(img.complete && img.naturalWidth > 0)) continue;
    const { title, slug } = c.dataset;
    if (!title || !slug) continue;
    return { title, slug };
  }
  return null;
};

/** How long to wait for SOME card's poster to finish loading, and how often to
 *  look.
 *
 *  Both are deliberately modest. The budget is a small slice of the 30s test
 *  timeout, so a runner with no route to the poster proxy still reaches the
 *  caller's own assertion instead of tripping the timeout in here. And the
 *  polling is on an INTERVAL rather than `waitForFunction`'s default `raf`:
 *  the predicate walks every `.col[data-title]` on the page, of which a city
 *  listing has ~1000, and re-running that on every animation frame is enough
 *  CPU on a contended runner to starve the very page loads it is waiting for. */
const PosterSettleTimeoutMs = 5_000;
const PosterSettlePollMs = 250;

/**
 * The first visible card, preferring one whose poster has SETTLED — finished
 * loading, successfully (`complete && naturalWidth > 0`).
 *
 * Callers use the answer to build a `[data-title=…] … img` locator and `tap()`
 * it, and a poster that is still in flight — or walking its `onerror` fallback
 * chain through `data-fallbacks` — keeps re-laying-out. Playwright's tap
 * actionability check wants "visible, enabled and stable", and a moving image
 * never gives it two consecutive frames with the same box, so `tap()` spends the
 * whole 30s test budget on an element `toBeVisible()` passed a line earlier.
 * That is the CI flake seen as `webkit-iphone-13-zoomed` › search-tap-dismiss,
 * on a weserv-proxied poster: the failure reads as a mystery timeout precisely
 * because visibility was never the problem.
 *
 * BOTH halves of the identity come back from ONE evaluation, and that is
 * load-bearing rather than a convenience: which card is "first settled" moves as
 * posters land, so reading the title and the slug in two calls could name two
 * different films — the test then taps one card and asserts the other's slug.
 */
export async function firstVisibleCard(page: Page): Promise<VisibleCard | null> {
  const settled = await page
    .waitForFunction(firstVisibleCardIn, true, {
      timeout: PosterSettleTimeoutMs,
      polling: PosterSettlePollMs,
    })
    .then((handle) => handle.jsonValue() as Promise<VisibleCard | null>)
    .catch(() => null);
  if (settled) return settled;
  // The page can be gone by now — a test that blew its budget is torn down
  // while this is in flight, and an "already closed" rejection here would
  // replace the real timeout with a confusing one.
  return page.evaluate(firstVisibleCardIn, false).catch(() => null);
}
