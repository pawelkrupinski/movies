import { test, expect } from '@playwright/test';
import { waitForCards } from './helpers';

// Locks landscape mobile to the one-row desktop layout. A previous
// iteration hid the logo and reflowed the navbar into two rows; on
// real landscape phones (iPhone 17 Pro Max → 956 px viewport,
// Pixel 7 → 915 px) that was worse than the unchanged desktop bar.
// The card-density part of the landscape media query (6 cols, 0.85
// `--mobile-scale`) stays — those tests live here too.
test.describe('mobile landscape layout', () => {
  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(!testInfo.project.name.includes('landscape'),
      'landscape layout only applies to landscape projects');
    await page.goto('/poznan/');
    await waitForCards(page);
  });

  // ── Navbar: same as portrait/desktop on one row ─────────────

  test('logo cluster is visible', async ({ page }) => {
    // The logo sits in the first `.d-flex` child of the navbar.
    // The two-row landscape reflow used to set `display: none` on
    // this element — keep a guard so future "tidy-up" changes
    // can't reintroduce that.
    const logoCluster = page.locator('.navbar > .d-flex').first();
    await expect(logoCluster).toBeVisible();
  });

  test('all navbar items share a single row', async ({ page }) => {
    const boxes = await page.evaluate(() => {
      const q = (sel: string) => {
        const el = document.querySelector(sel);
        if (!el) return null;
        const r = el.getBoundingClientRect();
        return { top: r.top, bottom: r.bottom, left: r.left, right: r.right };
      };
      return {
        logoTabs: q('.navbar > .d-flex'),
        search:   q('.navbar-search'),
        date:     q('.navbar-date'),
        filtry:   q('.navbar-filtry'),
        auth:     q('.navbar-auth'),
      };
    });

    const { logoTabs, search, date, filtry, auth } = boxes as Record<
      string,
      { top: number; bottom: number; left: number; right: number }
    >;
    expect(logoTabs).toBeTruthy();
    expect(search).toBeTruthy();
    expect(date).toBeTruthy();
    expect(filtry).toBeTruthy();
    expect(auth).toBeTruthy();

    const baseline = logoTabs.top;
    for (const [, box] of Object.entries({ search, date, filtry, auth })) {
      // Skip empty containers (e.g. .navbar-auth when no OAuth providers are
      // configured — the div exists in the DOM but has zero height and its
      // getBoundingClientRect().top falls at the flex row's midpoint rather
      // than at the top of any real control).  The 'navbar stays on one row'
      // test already uses this same guard.
      if (box.bottom === box.top) continue;
      expect(Math.abs(box.top - baseline)).toBeLessThan(12);
      expect(box.bottom - box.top).toBeGreaterThan(0);
    }
  });

  test('search sits immediately left of Filtry, day pills further left', async ({ page }) => {
    // The whole point of the landscape contract: the right cluster
    // packs together as day pills → search → Filtry on one row, so
    // the user reaches every control without jumping cross-screen.
    // Same row, search.right ≤ filtry.left, and the day pills (the
    // only other neighbour at this width) sit further left, outside
    // the search→Filtry band. Auth may have wrapped onto a second
    // row on narrower viewports — irrelevant to this check.
    const layout = await page.evaluate(() => {
      const r = (el: Element | null) => {
        if (!el) return null;
        const b = el.getBoundingClientRect();
        return { top: b.top, bottom: b.bottom, left: b.left, right: b.right };
      };
      return {
        date:   r(document.querySelector('.navbar-date')),
        filtry: r(document.querySelector('.navbar-filtry')),
        search: r(document.querySelector('.navbar-search')),
      };
    });

    expect(layout.date).toBeTruthy();
    expect(layout.filtry).toBeTruthy();
    expect(layout.search).toBeTruthy();
    const { date, filtry, search } = layout as Record<
      string,
      { top: number; bottom: number; left: number; right: number }
    >;

    // Same row.
    expect(Math.abs(search.top - filtry.top)).toBeLessThan(12);
    // Search is immediately to the left of Filtry.
    expect(search.right).toBeLessThanOrEqual(filtry.left + 1);
    // The day pills sit further left, outside the search→Filtry
    // band: date.right ≤ search.left.
    expect(date.right).toBeLessThanOrEqual(search.left + 1);
  });

  test('the navbar stays on one row (no second navbar row)', async ({ page }) => {
    // `flex-wrap: nowrap` keeps every navbar control on a single row. The logo
    // is taller (35px) than the landscape controls (28px), so they centre at
    // different top edges on the SAME row — assert one row by vertical overlap
    // (the whole stack fits inside the tallest item's height) rather than by
    // distinct top edges, which a centred-but-taller logo would trip.
    const oneRow = await page.evaluate(() => {
      const nav = document.querySelector('.navbar')!;
      let minTop = Infinity, maxBottom = -Infinity, maxH = 0;
      for (const c of Array.from(nav.children)) {
        const r = c.getBoundingClientRect();
        if (r.width === 0 || r.height === 0) continue;
        minTop = Math.min(minTop, r.top);
        maxBottom = Math.max(maxBottom, r.bottom);
        maxH = Math.max(maxH, r.height);
      }
      return maxBottom - minTop <= maxH + 2;
    });
    expect(oneRow, 'navbar wrapped to a second row').toBe(true);
  });

  test('rightmost navbar item is flush with the right edge', async ({ page }) => {
    // On wide landscape viewports (≥ 700 px), margin-left:auto on
    // .navbar-date absorbs remaining space and pushes the
    // date+search+filtry+auth cluster to the right edge. On narrow
    // zoomed-landscape viewports (< 600 px) the items fill the row
    // naturally, so there's no slack — the cluster sits flush-right
    // simply because it spans the full width. Either way, the
    // rightmost visible item should be within 20 px of the navbar's
    // right edge.
    const layout = await page.evaluate(() => {
      const r = (sel: string) => {
        const el = document.querySelector(sel);
        return el ? el.getBoundingClientRect() : null;
      };
      return {
        navbar: r('.navbar'),
        auth:   r('.navbar-auth') || r('.nav-tab-login'),
        filtry: r('.navbar-filtry'),
      };
    });
    expect(layout.navbar).toBeTruthy();
    const navbar = layout.navbar!;
    const navRight = navbar.left + navbar.width;
    const rightmost = layout.auth || layout.filtry;
    expect(rightmost).toBeTruthy();
    expect(navRight - (rightmost!.left + rightmost!.width)).toBeLessThan(20);
  });

  // ── Card density: separate concern, stays in landscape ────────

  test('film grid column count matches viewport width', async ({ page }) => {
    // Wide landscape (≥ 800 px): 6 columns (100%/6 ≈ 0.167).
    // Narrow landscape (< 800 px): 4 columns (25% = 0.25).
    // The 800 cutoff sits above the narrow-landscape 760 spec and
    // below every iPhone Pro / Plus landscape after the Dynamic-
    // Island safe-area inset bites (956 → ~830 on 17 Pro Max).
    const { ratio, vpWidth } = await page.evaluate(() => {
      const grid = document.querySelector('#film-grid') as HTMLElement;
      const col  = grid?.querySelector(':scope > .col') as HTMLElement;
      if (!grid || !col) return { ratio: -1, vpWidth: 0 };
      const prevDisplay = col.style.display;
      col.style.display = 'block';
      const colWidth  = col.getBoundingClientRect().width;
      const gridWidth = grid.getBoundingClientRect().width;
      col.style.display = prevDisplay;
      return { ratio: colWidth / gridWidth, vpWidth: window.innerWidth };
    });
    if (vpWidth >= 800) {
      expect(ratio).toBeGreaterThan(0.15);
      expect(ratio).toBeLessThan(0.18);
    } else {
      expect(ratio).toBeGreaterThan(0.23);
      expect(ratio).toBeLessThan(0.27);
    }
  });

  test('card chrome shrinks to mobile-scale floor', async ({ page }) => {
    // `--mobile-scale` is pinned to 0.85 in landscape, so a card
    // title's font-size collapses to 0.95rem × 0.85 ≈ 12.92 px
    // on a 16 px-root document. Tolerance band covers sub-pixel
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
