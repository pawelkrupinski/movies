import { test, expect } from '@playwright/test';
import { gotoAndWaitForCards } from './helpers';

// Mobile portrait floats the search field as a fixed pill near the bottom of
// the viewport (the `(max-width: 575px) and (orientation: portrait)` block in
// `_sharedStyles`). Its gap from the bottom edge is `8px + safe-area-inset`.
// Under Playwright emulation the safe-area inset is 0, so the measurable gap is
// the bare 8px offset. Desktop/landscape keep search inline (not fixed), so
// those projects are skipped.

const isFloatingPillProject = (name: string) =>
  !name.includes('desktop') && !name.includes('landscape');

test.describe('floating search pill bottom offset on mobile portrait', () => {
  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(
      !isFloatingPillProject(testInfo.project.name),
      'floating search pill is mobile-portrait only',
    );
    await gotoAndWaitForCards(page, '/poznan/');
  });

  test('sits 8px above the viewport bottom', async ({ page }) => {
    const pill = page.locator('.navbar-search');
    // Confirm it is actually the fixed floating pill in this layout, not the
    // inline navbar control — `bottom` only resolves to a length when fixed.
    expect(await pill.evaluate((el) => getComputedStyle(el).position)).toBe('fixed');

    const box = (await pill.boundingBox())!;
    const gap = await page.evaluate(
      ([bottom]) => window.innerHeight - bottom,
      [box.y + box.height],
    );
    // Tight enough to catch the doubling (old value was 4px) yet tolerant of
    // sub-pixel rounding in boundingBox.
    expect(gap).toBeGreaterThan(6.5);
    expect(gap).toBeLessThan(9.5);
  });
});
