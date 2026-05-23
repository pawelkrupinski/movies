import { test, expect } from '@playwright/test';
import { waitForCards, pinDateFilterAnytime } from './helpers';

// iPhone 17 Pro Max dimensions.
const PORTRAIT  = { width: 440, height: 956 };
const LANDSCAPE = { width: 956, height: 440 };

// Measure every visible navbar control's height + the grid column ratio.
async function measureNavbar(page: import('@playwright/test').Page) {
  return page.evaluate(() => {
    const nav = document.querySelector('.navbar');
    if (!nav) return { heights: {} as Record<string, number>, navHeight: 0 };
    const sels = [
      '.nav-tab', '.nav-tab-fav', '.nav-tab-login',
      '.refresh-btn', '.date-nav-btn',
      '.filter-select', '.search-input',
    ];
    const heights: Record<string, number> = {};
    for (const sel of sels) {
      const el = nav.querySelector(sel) as HTMLElement | null;
      if (!el || el.offsetParent === null) continue;
      const h = el.getBoundingClientRect().height;
      if (h > 0) heights[sel] = h;
    }
    return { heights, navHeight: nav.getBoundingClientRect().height };
  });
}

async function measureGridRatio(page: import('@playwright/test').Page) {
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

function assertUniformHeight(heights: Record<string, number>, expected: number) {
  const entries = Object.entries(heights);
  expect(entries.length).toBeGreaterThan(3);
  for (const [sel, h] of entries) {
    expect(h, `${sel}: ${h} should be ~${expected}`).toBeCloseTo(expected, 0);
  }
}

test.describe('orientation flip: portrait → landscape', () => {
  test.use({ viewport: PORTRAIT });

  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(testInfo.project.name.includes('desktop'),
      'orientation flip only relevant for mobile projects');
    await page.goto('/');
    await waitForCards(page);
  });

  test('navbar controls become uniform 28px after rotating to landscape', async ({ page }) => {
    // Start in portrait — base height 35px.
    const before = await measureNavbar(page);
    assertUniformHeight(before.heights, 35);

    // Rotate to landscape.
    await page.setViewportSize(LANDSCAPE);
    await page.waitForTimeout(200);

    const after = await measureNavbar(page);
    assertUniformHeight(after.heights, 28);
    expect(after.navHeight).toBeLessThanOrEqual(42);
  });

  test('grid switches to 6 columns after rotating to landscape', async ({ page }) => {
    await pinDateFilterAnytime(page);

    // Portrait: 2 columns (50%).
    const before = await measureGridRatio(page);
    expect(before).toBeGreaterThan(0.45);
    expect(before).toBeLessThan(0.55);

    // Rotate to landscape (956px ≥ 900px → 6 columns).
    await page.setViewportSize(LANDSCAPE);
    await page.waitForTimeout(200);

    const after = await measureGridRatio(page);
    expect(after).toBeGreaterThan(0.15);
    expect(after).toBeLessThan(0.18);
  });
});

test.describe('orientation flip: landscape → portrait', () => {
  test.use({ viewport: LANDSCAPE });

  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(testInfo.project.name.includes('desktop'),
      'orientation flip only relevant for mobile projects');
    await page.goto('/');
    await waitForCards(page);
  });

  test('navbar controls return to uniform 35px after rotating to portrait', async ({ page }) => {
    // Start in landscape — compact 28px.
    const before = await measureNavbar(page);
    assertUniformHeight(before.heights, 28);

    // Rotate to portrait.
    await page.setViewportSize(PORTRAIT);
    await page.waitForTimeout(200);

    const after = await measureNavbar(page);
    assertUniformHeight(after.heights, 35);
  });

  test('grid switches to 2 columns after rotating to portrait', async ({ page }) => {
    await pinDateFilterAnytime(page);

    // Landscape: 6 columns (~16.7%).
    const before = await measureGridRatio(page);
    expect(before).toBeGreaterThan(0.15);
    expect(before).toBeLessThan(0.18);

    // Rotate to portrait (440px < 575px → 2 columns).
    await page.setViewportSize(PORTRAIT);
    await page.waitForTimeout(200);

    const after = await measureGridRatio(page);
    expect(after).toBeGreaterThan(0.45);
    expect(after).toBeLessThan(0.55);
  });
});
