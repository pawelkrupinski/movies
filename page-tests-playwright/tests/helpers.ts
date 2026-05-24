import type { Page } from '@playwright/test';

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
 * Drive the `#date-filter` `<select>` and trigger the page's inline
 * `applyFilters()` so the visible-cards set reflects the new value
 * before assertions run. Used by every spec that needs deterministic
 * showtime visibility — `pinDateFilterAnytime` is just this with
 * `value = 'anytime'`.
 */
export async function setDateFilter(page: Page, value: string): Promise<void> {
  await page.evaluate((v) => {
    const sel = document.getElementById('date-filter') as HTMLSelectElement | null;
    if (sel) {
      sel.value = v;
      (globalThis as unknown as { applyFilters?: () => void }).applyFilters?.();
    }
  }, value);
}

/**
 * Wait for the home / favourites listing to have rendered at least
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
