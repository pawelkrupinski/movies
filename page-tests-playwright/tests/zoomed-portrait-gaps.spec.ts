import { test, expect } from '@playwright/test';
import { waitForCards } from './helpers';

// At 150% display zoom the CSS viewport shrinks to 240×507 (Galaxy S10).
// The `--ms` scale variable clamps to its 0.85 floor, and Bootstrap's
// `--bs-gutter-x` and card-body padding must still produce visible
// spacing. A regression that breaks `--ms` (e.g. Firefox's inability to
// evaluate <length>/<length> in calc) collapses these to zero because
// the CSS properties fall back to `initial`.

test.describe('zoomed portrait card gaps', () => {
  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(!testInfo.project.name.includes('zoomed')
              || testInfo.project.name.includes('landscape'),
      'zoomed portrait only');
    await page.goto('/poznan/');
    await waitForCards(page);
  });

  test('cards have visible horizontal gap, body padding, and column gutter', async ({ page }) => {
    // Scroll the grid into view to force content-visibility:auto to
    // render the first row's subtree (Firefox defers more aggressively
    // than Chromium).
    await page.evaluate(() => {
      document.querySelector('#film-grid')?.scrollIntoView();
    });
    await page.waitForTimeout(200);

    const result = await page.evaluate(() => {
      const cols = Array.from(document.querySelectorAll('#film-grid > .col'));
      let pair: [Element, Element] | null = null;
      for (let i = 0; i < cols.length - 1 && !pair; i++) {
        const a = cols[i].querySelector('.card');
        const b = cols[i + 1].querySelector('.card');
        if (!a || !b) continue;
        const ar = a.getBoundingClientRect();
        const br = b.getBoundingClientRect();
        if (ar.width > 0 && br.width > 0 && Math.abs(ar.top - br.top) < 20) {
          pair = [cols[i], cols[i + 1]];
        }
      }
      if (!pair) return null;
      const [c0, c1] = pair;
      // Scroll pair into view and force a style recalc.
      c0.scrollIntoView();
      void (c0 as HTMLElement).offsetHeight;

      const card0 = c0.querySelector('.card')!.getBoundingClientRect();
      const card1 = c1.querySelector('.card')!.getBoundingClientRect();
      const cb = c0.querySelector('.card-body');
      return {
        cardGap: card1.left - card0.right,
        cardBodyPad: cb ? parseFloat(getComputedStyle(cb).paddingLeft) : -1,
        colPad: parseFloat(getComputedStyle(c0).paddingLeft),
      };
    });

    expect(result).toBeTruthy();
    expect(result!.cardGap).toBeGreaterThanOrEqual(8);
    expect(result!.colPad).toBeGreaterThanOrEqual(4);

    // card-body padding: measure via a probe outside content-visibility
    // containers — Firefox defers getComputedStyle inside
    // content-visibility:auto subtrees even after scrollIntoView.
    const bodyPad = await page.evaluate(() => {
      const el = document.createElement('div');
      el.className = 'card-body';
      el.style.position = 'fixed';
      el.style.visibility = 'hidden';
      document.body.appendChild(el);
      const v = parseFloat(getComputedStyle(el).paddingLeft);
      el.remove();
      return v;
    });
    expect(bodyPad).toBeGreaterThanOrEqual(8);
  });

  test('navbar rows have visible vertical gap between them', async ({ page }) => {
    const gap = await page.evaluate(() => {
      const nav = document.querySelector('.navbar') as HTMLElement;
      if (!nav) return null;

      const row1Sels = ['.navbar-logo', '.nav-tab', '.navbar-auth'];
      const row2Sels = ['.navbar-date', '.navbar-filtry'];

      let row1Bottom = 0;
      for (const sel of row1Sels) {
        for (const el of Array.from(nav.querySelectorAll(sel)) as HTMLElement[]) {
          const r = el.getBoundingClientRect();
          if (r.height > 0) row1Bottom = Math.max(row1Bottom, r.bottom);
        }
      }

      let row2Top = Infinity;
      for (const sel of row2Sels) {
        for (const el of Array.from(nav.querySelectorAll(sel)) as HTMLElement[]) {
          const r = el.getBoundingClientRect();
          if (r.height > 0) row2Top = Math.min(row2Top, r.top);
        }
      }

      if (row1Bottom === 0 || row2Top === Infinity) return null;
      return row2Top - row1Bottom;
    });
    expect(gap, 'could not measure navbar row gap').not.toBeNull();
    expect(gap!, `navbar row gap is ${gap!.toFixed(1)}px; expected > 0`).toBeGreaterThan(0);
    expect(gap!, `navbar row gap is ${gap!.toFixed(1)}px; expected < 10`).toBeLessThan(10);
  });
});
