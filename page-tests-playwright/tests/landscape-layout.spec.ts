import { test, expect } from '@playwright/test';
import { waitForCards } from './helpers';

test.describe('mobile landscape navbar layout', () => {
  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(!testInfo.project.name.includes('landscape'),
      'landscape layout only applies to landscape projects');
    await page.goto('/');
    await waitForCards(page);
  });

  test('logo and tab links are hidden', async ({ page }) => {
    const logoTabs = page.locator('.navbar > .d-flex').first();
    await expect(logoTabs).toBeHidden();
  });

  test('row 1 has search then login, row 2 has date then filtry', async ({ page }) => {
    const boxes = await page.evaluate(() => {
      const q = (sel: string) => {
        const el = document.querySelector(sel);
        if (!el) return null;
        const r = el.getBoundingClientRect();
        return { top: r.top, bottom: r.bottom, left: r.left, right: r.right };
      };
      return {
        search: q('.navbar-search'),
        auth:   q('.navbar-auth'),
        date:   q('.navbar-date'),
        filtry: q('.navbar-filtry'),
      };
    });

    expect(boxes.search).toBeTruthy();
    expect(boxes.auth).toBeTruthy();
    expect(boxes.date).toBeTruthy();
    expect(boxes.filtry).toBeTruthy();

    const { search, auth, date, filtry } = boxes as Record<string, { top: number; bottom: number; left: number; right: number }>;

    // Row 1: search and auth share the same vertical band
    expect(Math.abs(search.top - auth.top)).toBeLessThan(10);
    // Search is left of auth
    expect(search.left).toBeLessThan(auth.left);

    // Row 2: date and filtry share the same vertical band
    expect(Math.abs(date.top - filtry.top)).toBeLessThan(10);
    // Date is left of filtry
    expect(date.left).toBeLessThan(filtry.left);

    // Row 2 is below row 1
    expect(date.top).toBeGreaterThan(search.top);
  });

  test('search input spans most of row width', async ({ page }) => {
    const ratio = await page.evaluate(() => {
      const navbar = document.querySelector('.navbar') as HTMLElement;
      const search = document.querySelector('.navbar-search') as HTMLElement;
      if (!navbar || !search) return 0;
      return search.getBoundingClientRect().width / navbar.getBoundingClientRect().width;
    });
    expect(ratio).toBeGreaterThan(0.4);
  });

  test('film grid packs 6 cards per row', async ({ page }) => {
    // Landscape phones at 844–915 px wide land on Bootstrap's md
    // breakpoint by default (`row-cols-md-4` = 4 cards/row). The
    // landscape media query in `_sharedStyles` overrides each
    // `.col` to `flex: 0 0 calc(100% / 6)`. Reads the column's
    // computed width against its parent — `getComputedStyle`
    // works whether or not the date filter has hidden the row,
    // so this is stable against the default "today" filter
    // dropping the whole fixture corpus (which is dated to
    // 2026-05-17, not the wall-clock day the CI runs).
    const ratio = await page.evaluate(() => {
      const grid = document.querySelector('#film-grid') as HTMLElement;
      const col  = grid?.querySelector(':scope > .col') as HTMLElement;
      if (!grid || !col) return -1;
      // Force the .col into the layout flow long enough to measure
      // (display:none would give a zero width). We restore the
      // previous style after reading.
      const prevDisplay = col.style.display;
      col.style.display = 'block';
      const colWidth  = col.getBoundingClientRect().width;
      const gridWidth = grid.getBoundingClientRect().width;
      col.style.display = prevDisplay;
      return colWidth / gridWidth;
    });
    // 100% / 6 ≈ 0.1667. Bootstrap gutters take ~12 px out of each
    // column, so the visible ratio lands a hair below the math.
    // Band tolerates that without false positives at 5- or 7-col
    // breakpoints.
    expect(ratio).toBeGreaterThan(0.15);
    expect(ratio).toBeLessThan(0.18);
  });

  test('card chrome shrinks to mobile-scale floor', async ({ page }) => {
    // `--mobile-scale` is pinned to 0.85 in landscape, so a card
    // title's font-size collapses to 0.95rem × 0.85 ≈ 12.92 px on
    // a 16 px-root document. Tolerance band covers sub-pixel
    // rounding across engines.
    const titleFontPx = await page.evaluate(() => {
      const a = document.querySelector('#film-grid .card-title a') as HTMLElement;
      if (!a) return -1;
      return parseFloat(getComputedStyle(a).fontSize);
    });
    expect(titleFontPx).toBeGreaterThan(12.5);
    expect(titleFontPx).toBeLessThan(13.5);
  });
});
