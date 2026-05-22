import { test, expect } from '@playwright/test';

// WebKit (Mobile Safari approximation) — regression coverage for the
// card-tap two-tap behaviour. iPhone Safari's native sticky-hover
// handles the first-tap preview without our JS, and `shared.js`
// explicitly DOESN'T install the Android two-tap listener on iOS UAs
// (running it there would force a three-tap flow). These tests pin
// the iPhone-parity contract from the WebKit side.

test.describe('card poster link on WebKit (iPhone emulation)', () => {

  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    // The `/` page renders 100+ cards from the live database; pin the
    // date filter to "anytime" so the visible set isn't a function
    // of the runner's wall-clock relative to the live schedule.
    await page.evaluate(() => {
      const sel = document.getElementById('date-filter') as HTMLSelectElement | null;
      if (sel) {
        sel.value = 'anytime';
        // applyFilters is defined globally in the inline script.
        (globalThis as unknown as { applyFilters?: () => void }).applyFilters?.();
      }
    });
  });

  test('UA is iPhone-shaped so the Android listener stays unbound', async ({ page }) => {
    const ua = await page.evaluate(() => navigator.userAgent);
    expect(ua).toMatch(/iPhone/);
    // shared.js gates the two-tap listener on `/Android/.test(ua)`;
    // iPhone UA should not match.
    expect(ua).not.toMatch(/Android/);
  });

  // `applyFilters` re-appends visible cards after hidden ones in the
  // DOM, so a plain `querySelector` lands on a hidden card. Different
  // engines also serialise `style.display = 'none'` slightly differently
  // (`display: none;` vs `display:none;`), making a CSS-attribute
  // selector brittle. Solve both at once by computing the visible
  // card's title in JS, then targeting it via `[data-title=…]`.
  const firstVisibleTitle = async (page: import('@playwright/test').Page) =>
    page.evaluate(() => {
      const cols = [...document.querySelectorAll<HTMLElement>('.col[data-title]')];
      return cols.find((c) => c.style.display !== 'none')?.dataset.title ?? null;
    });

  test('tap on a card poster link navigates to /film', async ({ page }) => {
    const title = await firstVisibleTitle(page);
    expect(title).toBeTruthy();

    const link = page.locator(`.col[data-title="${title}"] .card .poster-wrap > a`);
    await expect(link).toBeVisible();
    await link.tap();

    await page.waitForURL(/\/film\?title=/);
    const url = new URL(page.url());
    expect(decodeURIComponent(url.search)).toContain(`title=${title}`);
  });

  test('detail page renders without a JS error', async ({ page }) => {
    const errors: string[] = [];
    page.on('pageerror', (e) => errors.push(e.message));

    const title = await firstVisibleTitle(page);
    expect(title).toBeTruthy();
    const link = page.locator(`.col[data-title="${title}"] .card .poster-wrap > a`);
    await expect(link).toBeVisible();
    await link.tap();
    await page.waitForURL(/\/film\?title=/);

    // film.scala.html's inline `toggleFavMovie` + `playTrailer` blocks
    // run on DOMContentLoaded — a syntax error or undefined reference
    // would surface here. Empty `errors` is the assertion.
    expect(errors).toEqual([]);
  });
});
