import { test, expect, Locator } from '@playwright/test';
import { gotoAndWaitForCards, pinDateFilterAnytime } from './helpers';
// The PNG decoder Playwright bundles for its own snapshot tooling.
// eslint-disable-next-line @typescript-eslint/no-var-requires
const { PNG } = require('playwright-core/lib/utilsBundle');

// Vertical offset of the rating text's ink from the centre of its coloured pill,
// in device pixels. Screenshot the value span, decode it, and find the first and
// last rows that carry opaque non-background pixels (the glyphs). A positive
// result means the ink sits below the box centre; negative means above. The
// `line-height: 1.3` regression left the text sitting high (~2px above centre).
async function inkCentreOffset(value: Locator): Promise<{ offset: number; height: number }> {
  const png = PNG.sync.read(await value.screenshot());
  const { width, height, data } = png;
  const px = (x: number, y: number) => {
    const i = (y * width + x) * 4;
    return [data[i], data[i + 1], data[i + 2], data[i + 3]];
  };
  // Background from interior top-centre (above the glyphs) — corners carry the
  // neighbouring label colour and the rounded-corner transparency.
  const bg = px(Math.floor(width / 2), 1);
  // Skip a few columns each side: the label/value seam bleeds a full-height
  // strip of the neighbour's colour there.
  const xLo = 5, xHi = width - 5;
  const isInk = (x: number, y: number) => {
    const [r, g, b, a] = px(x, y);
    if (a < 200) return false;
    return Math.abs(r - bg[0]) + Math.abs(g - bg[1]) + Math.abs(b - bg[2]) > 90;
  };
  let top = -1, bottom = -1;
  for (let y = 0; y < height; y++) {
    for (let x = xLo; x < xHi; x++) {
      if (isInk(x, y)) { if (top === -1) top = y; bottom = y; break; }
    }
  }
  expect(top, 'no glyph pixels found in the pill').toBeGreaterThan(-1);
  return { offset: (top + bottom) / 2 - height / 2, height };
}

// The bug was reported on a Pixel 9a — i.e. mobile Chrome. The fix is geometry
// (em padding + reduced line-height) so it holds everywhere, but the px
// tolerances below were calibrated against Chromium's text rendering; other
// engines rasterise the same face differently, so keep the assertion to
// Chromium to avoid font-driven flakiness.
test.describe('rating pill vertical centring', () => {
  test('IMDb value text is centred in its pill', async ({ page, browserName }) => {
    test.skip(browserName !== 'chromium', 'px tolerances calibrated for Chromium rendering');

    await gotoAndWaitForCards(page, '/');
    await pinDateFilterAnytime(page);

    // Force the column holding the first IMDb pill visible (applyFilters hides
    // out-of-window cards with display:none, which zeroes their screenshots).
    await page.evaluate(() => {
      const v = document.querySelector('.rating-imdb-value') as HTMLElement | null;
      if (!v) return;
      (v.closest('.col') as HTMLElement).style.display = 'block';
      v.closest('.ratings')!.scrollIntoView({ block: 'center' });
    });

    const value = page.locator('.rating-imdb-value').first();
    await expect(value).toBeVisible();

    const { offset, height } = await inkCentreOffset(value);
    // ≤ 1.5 device px off-centre. Pre-fix the text sat ~2px high (offset ≈ -2)
    // and this failed; with the centred padding it lands at ≈ 0.
    expect(Math.abs(offset), `ink off-centre by ${offset}px in a ${height}px pill`).toBeLessThanOrEqual(1.5);
  });
});
