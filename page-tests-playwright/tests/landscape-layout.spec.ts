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

  test('logo and tab links are visible', async ({ page }) => {
    // Logo + tabs sit in the first `.d-flex` child of the navbar.
    // The two-row landscape reflow used to set `display: none` on
    // this element — keep a guard so future "tidy-up" changes
    // can't reintroduce that.
    const logoTabs = page.locator('.navbar > .d-flex').first();
    await expect(logoTabs).toBeVisible();
    await expect(page.locator('.navbar a.nav-tab', { hasText: 'Filmy' })).toBeVisible();
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
      expect(Math.abs(box.top - baseline)).toBeLessThan(12);
      expect(box.bottom - box.top).toBeGreaterThan(0);
    }
  });

  test('date selector sits immediately left of Filtry', async ({ page }) => {
    // The whole point of the landscape contract: the user can
    // reach the date stepper without jumping cross-screen — it
    // hugs the Filtry pill. Same row, date.right ≤ filtry.left,
    // and search (the only other neighbour at this width) sits
    // further left. Auth may have wrapped onto a second row on
    // narrower viewports — irrelevant to this check.
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
    expect(Math.abs(date.top - filtry.top)).toBeLessThan(12);
    // Date is to the left of Filtry.
    expect(date.right).toBeLessThanOrEqual(filtry.left + 1);
    // Adjacent — nothing in the navbar sits between date.right
    // and filtry.left. Search must be OUTSIDE that horizontal
    // band: search.right ≤ date.left (further left).
    expect(search.right).toBeLessThanOrEqual(date.left + 1);
  });

  test('row-break is suppressed (no second navbar row)', async ({ page }) => {
    // `.navbar-row-break` flips to `display: block` in the
    // portrait (max-width: 575px) media query to force a hard
    // wrap. In landscape we want the row-break invisible so the
    // navbar stays one line. The element is in the DOM either
    // way; this assertion is on the computed display.
    const display = await page.evaluate(() => {
      const el = document.querySelector('.navbar-row-break') as HTMLElement | null;
      if (!el) return 'missing';
      return getComputedStyle(el).display;
    });
    expect(display).toBe('none');
  });

  test('rightmost navbar item is flush with the right edge', async ({ page }) => {
    // On wide landscape viewports (≥ 700 px), margin-left:auto on
    // .navbar-search absorbs remaining space and pushes the
    // search+date+filtry+auth cluster to the right edge. On narrow
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
