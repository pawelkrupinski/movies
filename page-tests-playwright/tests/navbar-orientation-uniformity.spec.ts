import { test, expect, Page } from '@playwright/test';
import { waitForCards } from './helpers';

// Locks the contract that on every mobile browser the navbar's
// row + each interactive control inside it renders at the same
// height in portrait AND landscape — so rotating the device
// doesn't shift the row metrics around. Drives a single page
// load and then rotates the viewport via `setViewportSize`, so
// it runs against the matrix's bare portrait mobile projects
// (chromium, webkit, firefox) — landscape and desktop projects
// already have orientation-fixed viewports, so they're skipped.
//
// Why this exists: the previous CSS had 35 px control heights in
// portrait (from the base desktop rule) and 28 px heights in
// landscape (from the compact-landscape media query). Rotating
// from portrait to landscape compressed the row by 7 px, which
// looked like the navbar was "jumping" mid-rotation on real
// devices. The shared mobile-heights rule in
// `_sharedStyles.scala.html` now keys on
// `(max-width: 575px), (max-height: 500px) and (orientation:
// landscape)` so both orientations pin every control to 28 px.

// The visible interactive controls on `/`. `.nav-tab-fav` is the
// ★, `.date-nav-btn` the ‹ ›, `.refresh-btn` includes Filtry; we
// disambiguate the overlapping classes via `:not()` so each
// control is counted once.
const CONTROLS = [
  '.nav-tab:not(.nav-tab-fav)',
  '.nav-tab-fav',
  '.nav-tab-login',
  '.refresh-btn:not(.date-nav-btn)',
  '.date-nav-btn',
  '#date-filter',
  '.search-input',
];

async function measureHeights(page: Page): Promise<Record<string, number>> {
  return page.evaluate((sels) => {
    const nav = document.querySelector('.navbar');
    if (!nav) return {};
    const out: Record<string, number> = {};
    for (const sel of sels) {
      const candidates = Array.from(nav.querySelectorAll(sel)) as HTMLElement[];
      // First visible match — the hidden Filtry-panel inputs and
      // off-screen wraps shouldn't drive the measurement.
      const el = candidates.find(
        (e) => e.offsetParent !== null && e.getBoundingClientRect().height > 0,
      );
      if (el) out[sel] = el.getBoundingClientRect().height;
    }
    return out;
  }, CONTROLS);
}

test.describe('navbar orientation uniformity', () => {
  test.beforeEach(async ({ page }, testInfo) => {
    const name = testInfo.project.name;
    test.skip(
      name.includes('landscape') || name.includes('desktop'),
      'this spec rotates the viewport itself; the matrix\'s bare mobile portrait projects (chromium / webkit / firefox) drive both orientations from one page load',
    );
    await page.goto('/');
    await waitForCards(page);
  });

  test('every navbar control keeps its portrait height after rotating to landscape', async ({ page }) => {
    const portrait = page.viewportSize();
    expect(portrait, 'project should have a viewport set').toBeTruthy();
    const { width, height } = portrait!;

    const portraitHeights = await measureHeights(page);
    expect(
      Object.keys(portraitHeights).length,
      'portrait pass should locate at least three visible navbar controls',
    ).toBeGreaterThanOrEqual(3);

    // Swap to landscape by transposing width / height. CSS media
    // queries pick up the new dimensions and the navbar reflows
    // without needing a reload (no JS depends on orientation).
    await page.setViewportSize({ width: height, height: width });
    const landscapeHeights = await measureHeights(page);

    // Every selector measured in portrait should also be present
    // in landscape (or at least one of them — the auth pill might
    // not render if the project's emulated UA is anonymous and
    // OAuth providers are empty). For each shared selector,
    // heights must match within 1 px sub-pixel tolerance.
    const diffs: { sel: string; portrait: number; landscape: number }[] = [];
    for (const [sel, p] of Object.entries(portraitHeights)) {
      const l = landscapeHeights[sel];
      if (l === undefined) continue;
      if (Math.abs(p - l) > 1) diffs.push({ sel, portrait: p, landscape: l });
    }
    expect(
      diffs,
      `controls whose height changed across rotation:\n${JSON.stringify(diffs, null, 2)}`,
    ).toEqual([]);

    // And the absolute height should be the compact 28 px target
    // — guards against the contract silently flipping to 35 px
    // (both orientations) which would also pass the equal-height
    // assertion above. 1 px tolerance for sub-pixel rendering.
    for (const [sel, h] of Object.entries(landscapeHeights)) {
      expect(
        Math.abs(h - 28),
        `${sel} (${h.toFixed(2)} px) should be 28 px compact-mobile target`,
      ).toBeLessThanOrEqual(1);
    }
  });
});
