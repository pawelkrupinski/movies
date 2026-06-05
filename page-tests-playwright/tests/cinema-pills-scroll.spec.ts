import { test, expect } from '@playwright/test';
import { waitForCards } from './helpers';

// The /kina cinema-picker pills are a single, non-wrapping row that
// scrolls horizontally whenever they don't fit the width — at EVERY
// viewport, not just on phones. It's pure CSS, no breakpoint involved.
//
// Pinned to chrome-desktop and driven by explicit `setViewportSize`:
// on a DPR-1 desktop engine CSS px == viewport width, so the layout and
// any `@media` agree. The mobile-emulation projects diverge (device
// scale + meta-viewport make `matchMedia` and CSS `@media` evaluate
// against different widths), which would make width-based assertions
// unreliable. The behaviour is engine-independent, so one clean desktop
// engine is enough.
test.describe('/kina cinema-pill row layout', () => {
  test.beforeEach(({}, testInfo) => {
    test.skip(
      testInfo.project.name !== 'chrome-desktop',
      'pure-CSS scroll strip — one DPR-1 desktop engine is enough',
    );
  });

  /** Computed layout facts for `#cinema-pills` that decide wrap vs scroll. */
  async function pillRowLayout(page: import('@playwright/test').Page) {
    return page.evaluate(() => {
      const row = document.getElementById('cinema-pills')!;
      const cs = getComputedStyle(row);
      // True laid-out width of the pills (right edge of the last one).
      // `scrollWidth` floors at `clientWidth`, so it can't reveal the
      // content width when the row isn't overflowing; the children's
      // extent can, and is viewport-independent above the 575px scale
      // breakpoint.
      let contentWidth = 0;
      for (const c of Array.from(row.children) as HTMLElement[]) {
        contentWidth = Math.max(contentWidth, c.offsetLeft + c.offsetWidth);
      }
      return {
        flexWrap: cs.flexWrap,
        overflowX: cs.overflowX,
        contentWidth,
        // > 1px slack: a single-row `nowrap` strip wider than its box is
        // the proof the user can actually scroll through it.
        overflows: row.scrollWidth - row.clientWidth > 1,
      };
    });
  }

  test('the row never wraps and is a scroll container at any width', async ({ page }) => {
    await page.goto('/poznan/kina');
    await waitForCards(page);
    await page.waitForSelector('#cinema-pills .cinema-pill', { state: 'attached' });

    for (const width of [400, 800, 1280]) {
      await page.setViewportSize({ width, height: 900 });
      const layout = await pillRowLayout(page);
      expect(layout.flexWrap, `flex-wrap at ${width}px`).toBe('nowrap');
      expect(layout.overflowX, `overflow-x at ${width}px`).toMatch(/auto|scroll/);
    }
  });

  test('scrolls horizontally when the pills overflow — above the phone breakpoint too', async ({ page }) => {
    await page.goto('/poznan/kina');
    await waitForCards(page);
    await page.waitForSelector('#cinema-pills .cinema-pill', { state: 'attached' });

    // Measure the pills' natural laid-out width on a roomy viewport
    // (where they fit and aren't `--mobile-scale`-shrunk), then shrink to
    // a still-desktop width narrower than that — proving the scroll kicks
    // in purely from "doesn't fit", not from any mobile breakpoint.
    await page.setViewportSize({ width: 1600, height: 900 });
    const content = (await pillRowLayout(page)).contentWidth;

    const narrow = Math.max(600, Math.round(content) - 80);
    expect(narrow, 'test width must sit above the 575px phone breakpoint').toBeGreaterThan(575);
    expect(narrow, 'fixture must have enough cinemas to overflow a desktop width').toBeLessThan(content);

    await page.setViewportSize({ width: narrow, height: 900 });
    const layout = await pillRowLayout(page);
    expect(layout.flexWrap).toBe('nowrap');
    expect(layout.overflows).toBe(true);
  });
});
