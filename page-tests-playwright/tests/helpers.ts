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

/**
 * First card whose inline `style.display` isn't `none` AND whose
 * `<img>` is still visible (i.e. the `onerror` fallback chain didn't
 * `display:none` it). `applyFilters` re-appends visible cards after
 * hidden ones, so `querySelector` lands on a hidden one — explicitly
 * walking + checking is engine-agnostic.
 */
export async function firstVisibleTitle(page: Page): Promise<string | null> {
  return page.evaluate(() => {
    const cols = [...document.querySelectorAll<HTMLElement>('.col[data-title]')];
    for (const c of cols) {
      if (c.style.display === 'none') continue;
      const img = c.querySelector<HTMLImageElement>('.poster-wrap > a img');
      if (!img || img.style.display === 'none') continue;
      return c.dataset.title ?? null;
    }
    return null;
  });
}
