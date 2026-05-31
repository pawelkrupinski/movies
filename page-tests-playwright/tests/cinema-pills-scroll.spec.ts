import { test, expect } from '@playwright/test';
import { waitForCards } from './helpers';

// The /kina cinema-picker pills wrap to multiple rows on desktop but
// become a single horizontally scrollable strip on phones — wrapping
// them on a narrow screen would eat the whole viewport. The behaviour
// is pure CSS keyed off the `max-width: 575px` breakpoint.
//
// Pinned to chrome-desktop and driven by explicit `setViewportSize`:
// on a DPR-1 desktop engine CSS px == viewport width, so the CSS
// `@media` and the layout agree. On the mobile-emulation projects they
// don't (device-scale + meta-viewport make `matchMedia` and the CSS
// `@media` evaluate against different widths), which would make the
// breakpoint assertion unreliable. The rule is engine-independent, so
// one clean desktop engine is enough.
test.describe('/kina cinema-pill row layout', () => {
  test.beforeEach(({}, testInfo) => {
    test.skip(
      testInfo.project.name !== 'chrome-desktop',
      'pure-CSS breakpoint — one DPR-1 desktop engine is enough',
    );
  });

  /** Computed layout facts for `#cinema-pills` that decide wrap vs scroll. */
  async function pillRowLayout(page: import('@playwright/test').Page) {
    return page.evaluate(() => {
      const row = document.getElementById('cinema-pills')!;
      const cs = getComputedStyle(row);
      return {
        flexWrap: cs.flexWrap,
        overflowX: cs.overflowX,
        // > 1px slack: a single-row `nowrap` strip wider than its box is
        // the proof the user can actually scroll through it.
        overflows: row.scrollWidth - row.clientWidth > 1,
      };
    });
  }

  test('phones get a single horizontally scrollable strip', async ({ page }) => {
    await page.setViewportSize({ width: 400, height: 900 });
    await page.goto('/kina');
    await waitForCards(page);
    await page.waitForSelector('#cinema-pills .cinema-pill', { state: 'attached' });

    const layout = await pillRowLayout(page);
    expect(layout.flexWrap).toBe('nowrap');
    expect(layout.overflowX).toMatch(/auto|scroll/);
    // Every cinema on one line is wider than a 400px phone, so the strip
    // scrolls horizontally instead of clipping.
    expect(layout.overflows).toBe(true);
  });

  test('desktop wraps the pills to as many rows as needed', async ({ page }) => {
    await page.setViewportSize({ width: 1280, height: 900 });
    await page.goto('/kina');
    await waitForCards(page);
    await page.waitForSelector('#cinema-pills .cinema-pill', { state: 'attached' });

    const layout = await pillRowLayout(page);
    expect(layout.flexWrap).toBe('wrap');
  });
});
