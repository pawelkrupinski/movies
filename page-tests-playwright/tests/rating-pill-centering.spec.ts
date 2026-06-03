import { test, expect } from '@playwright/test';
import { gotoAndWaitForCards, pinDateFilterAnytime } from './helpers';

// The rating-pill digits rode high in their coloured box because the line-box
// leading sits a descender's worth below the baseline — clearly visible in
// Android's Roboto (measured ~2.5px high in a 32px pill on a Pixel 9a). The fix
// is `text-box-trim`, which crops the line box to cap-height/baseline so the
// symmetric padding centres the glyphs on ANY font.
//
// The *visual* centring can only be confirmed on-device: it depends on the
// platform font (Roboto / SF), and on CI's Chromium font the residual offset is
// sub-pixel — below what screenshot sampling can reliably gate (an earlier
// per-pixel assertion flaked on exactly this). So, per the focus-zoom precedent,
// we assert the *mechanism* is live — `text-box-trim` is computed onto the pill
// text with balanced top/bottom padding — which fails before the fix (the
// property defaults to `normal`) and passes after. The checked-in page snapshot
// locks the CSS string itself as a second guard.
test.describe('rating pill vertical centring', () => {
  test('text-box-trim is applied to the pill text', async ({ page, browserName }) => {
    test.skip(browserName !== 'chromium', 'text-box-trim is not supported in this engine');

    await gotoAndWaitForCards(page, '/');
    await pinDateFilterAnytime(page);

    const value = page.locator('.rating-imdb-value').first();
    await expect(value).toBeAttached();

    const computed = await value.evaluate((el) => {
      const s = getComputedStyle(el) as CSSStyleDeclaration & { textBoxTrim?: string; textBoxEdge?: string };
      return {
        trim: s.textBoxTrim,
        edge: s.textBoxEdge,
        padTop: s.paddingTop,
        padBottom: s.paddingBottom,
      };
    });

    expect(computed.trim).toBe('trim-both');
    expect(computed.edge).toContain('cap');
    // Symmetric vertical padding is what centres the trimmed box — if they drift
    // apart the glyphs go off-centre again.
    expect(computed.padTop).toBe(computed.padBottom);
  });
});
