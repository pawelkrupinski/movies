import { test, expect } from '@playwright/test';
import { waitForCards, pinDateFilterAnytime } from './helpers';

// Galaxy S10 (360×760 CSS viewport) is narrower than the existing
// landscape test matrix (844–956 px). At 760 px wide with only 360 px
// of vertical space, the current 6-column grid cramps card titles and
// the desktop-height navbar eats too much of the scarce vertical room.

test.describe('narrow landscape (760×360)', () => {
  test.use({ viewport: { width: 760, height: 360 } });

  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(testInfo.project.name.includes('desktop'),
      'narrow landscape only relevant for mobile-class projects');
    await page.goto('/');
    await waitForCards(page);
  });

  test('navbar is compact — height ≤ 42px', async ({ page }) => {
    const height = await page.evaluate(() => {
      const nav = document.querySelector('.navbar') as HTMLElement;
      return nav ? nav.getBoundingClientRect().height : -1;
    });
    expect(height).toBeGreaterThan(0);
    expect(height).toBeLessThanOrEqual(42);
  });

  test('film grid shows 4 cards per row, not 6', async ({ page }) => {
    await pinDateFilterAnytime(page);
    const ratio = await page.evaluate(() => {
      const grid = document.querySelector('#film-grid') as HTMLElement;
      const col = grid?.querySelector(':scope > .col') as HTMLElement;
      if (!grid || !col) return -1;
      const prev = col.style.display;
      col.style.display = 'block';
      const r = col.getBoundingClientRect().width / grid.getBoundingClientRect().width;
      col.style.display = prev;
      return r;
    });
    // 4 columns → ~25%. Must NOT be 6 columns (~16.7%).
    expect(ratio).toBeGreaterThan(0.23);
    expect(ratio).toBeLessThan(0.27);
  });

  test('card gutter is reduced from default 1.5rem', async ({ page }) => {
    const gutterPx = await page.evaluate(() => {
      const grid = document.querySelector('#film-grid') as HTMLElement;
      if (!grid) return -1;
      const raw = getComputedStyle(grid).getPropertyValue('--bs-gutter-x').trim();
      // Parse rem to px (1rem = root font-size, typically 16px)
      const root = parseFloat(getComputedStyle(document.documentElement).fontSize);
      const val = parseFloat(raw);
      if (raw.endsWith('rem')) return val * root;
      if (raw.endsWith('px')) return val;
      return val;
    });
    // Default Bootstrap gutter is 1.5rem = 24px. In landscape it
    // should be tighter — ≤ 16px.
    expect(gutterPx).toBeGreaterThan(0);
    expect(gutterPx).toBeLessThanOrEqual(16);
  });
});

test.describe('landscape navbar uniform height', () => {
  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(!testInfo.project.name.includes('landscape'),
      'landscape-only test');
    await page.goto('/');
    await waitForCards(page);
  });

  test('all visible navbar controls share the same height', async ({ page }) => {
    const heights = await page.evaluate(() => {
      const nav = document.querySelector('.navbar');
      if (!nav) return {};
      const sels = [
        '.nav-tab', '.nav-tab-fav', '.nav-tab-login',
        '.refresh-btn', '.date-nav-btn',
        '.filter-select', '.search-input',
      ];
      const result: Record<string, number> = {};
      for (const sel of sels) {
        const el = nav.querySelector(sel) as HTMLElement | null;
        if (!el || el.offsetParent === null) continue;
        const h = el.getBoundingClientRect().height;
        if (h > 0) result[sel] = h;
      }
      return result;
    });
    const values = Object.values(heights);
    expect(values.length).toBeGreaterThan(3);
    const target = values[0];
    for (const [sel, h] of Object.entries(heights)) {
      expect(h, `${sel} height ${h} ≠ ${target}`).toBeCloseTo(target, 0);
    }
  });
});

test.describe('portrait filtry button (360×760)', () => {
  test.use({ viewport: { width: 360, height: 760 } });

  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(testInfo.project.name.includes('desktop'),
      'portrait filtry only relevant for mobile-class projects');
    await page.goto('/');
    await waitForCards(page);
  });

  test('Filtry button right edge stays inside viewport', async ({ page }) => {
    const right = await page.evaluate(() => {
      const btn = document.querySelector('#format-filter-btn');
      return btn ? btn.getBoundingClientRect().right : -1;
    });
    expect(right).toBeGreaterThan(0);
    // At least 4px of breathing room from the viewport edge.
    expect(right).toBeLessThanOrEqual(360 - 4);
  });
});
