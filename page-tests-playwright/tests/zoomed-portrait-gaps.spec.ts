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
      const checkbox = c0.querySelector('.card-body');
      return {
        cardGap: card1.left - card0.right,
        cardBodyPad: checkbox ? parseFloat(getComputedStyle(checkbox).paddingLeft) : -1,
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
      const element = document.createElement('div');
      element.className = 'card-body';
      element.style.position = 'fixed';
      element.style.visibility = 'hidden';
      document.body.appendChild(element);
      const v = parseFloat(getComputedStyle(element).paddingLeft);
      element.remove();
      return v;
    });
    expect(bodyPad).toBeGreaterThanOrEqual(8);
  });

  // The navbar is now a single `flex-wrap: nowrap` row at every width (it used
  // to wrap to two rows on mobile), so the old "visible vertical gap between
  // navbar rows" assertion no longer applies — the one-row invariant is covered
  // by navbar-layout / landscape-layout / PageJsBehaviourSpec.

  // The active day-pill is a filled capsule; below ~290px the `@media
  // (max-width: 290px)` block used to zero ALL navbar padding (to reclaim
  // horizontal width for the logo), which collapsed the row to exactly the
  // 28px pill height — so the capsule sat flush against the navbar's top and
  // bottom edges and its flat pill top/bottom read as "cut off". Zeroing only
  // the HORIZONTAL padding keeps the vertical breathing room, so the capsule
  // never touches the row edge.
  test('the active day-pill is not flush against the navbar top/bottom', async ({ page }) => {
    const gaps = await page.evaluate(() => {
      const pill = document.querySelector('.day-pill.active') as HTMLElement;
      const navbar = document.querySelector('.navbar') as HTMLElement;
      const pr = pill.getBoundingClientRect();
      const nr = navbar.getBoundingClientRect();
      return { top: pr.top - nr.top, bottom: nr.bottom - pr.bottom };
    });
    expect(gaps.top).toBeGreaterThanOrEqual(2);
    expect(gaps.bottom).toBeGreaterThanOrEqual(2);
  });
});
