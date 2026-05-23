import { test, expect } from '@playwright/test';
import { pinDateFilterAnytime } from './helpers';

// Filtry dropdown filters (Wymiar / Wersja / IMAX / Od godziny) +
// the Filtry button's "fill" state. Each filter narrows the visible
// set of showtime badges via `applyFilters`; the button's textContent
// + class indicate which axes have a non-empty value.

const visibleBadges = (page: import('@playwright/test').Page) =>
  page.evaluate(
    () =>
      [...document.querySelectorAll<HTMLElement>('.badge-time')].filter(
        (b) => b.style.display !== 'none',
      ).length,
  );

test.describe('Filtry dropdown', () => {

  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await pinDateFilterAnytime(page);
  });

  test('Wymiar = 3D narrows badges to a strict subset', async ({ page }) => {
    const beforeCount = await visibleBadges(page);
    expect(beforeCount).toBeGreaterThan(0);

    // Click the 3D radio. `onFormatChange()` is the inline handler the
    // template wires onto every input — calling it directly skips the
    // need to open the panel (its visibility is just CSS chrome).
    await page.evaluate(() => {
      (document.querySelector(
        'input[name="format-dim"][value="3D"]',
      ) as HTMLInputElement).click();
    });

    const afterCount = await visibleBadges(page);
    expect(afterCount).toBeGreaterThan(0);
    expect(afterCount).toBeLessThan(beforeCount);

    // Every visible badge carries '3D' in its data-format.
    const allHave3D = await page.evaluate(() =>
      [...document.querySelectorAll<HTMLElement>('.badge-time')]
        .filter((b) => b.style.display !== 'none')
        .every((b) => (b.dataset.format ?? '').split(' ').includes('3D')),
    );
    expect(allHave3D).toBe(true);
  });

  test('Wersja = NAP narrows badges to subtitled-only', async ({ page }) => {
    const beforeCount = await visibleBadges(page);

    await page.evaluate(() => {
      (document.querySelector(
        'input[name="format-lang"][value="NAP"]',
      ) as HTMLInputElement).click();
    });

    const afterCount = await visibleBadges(page);
    expect(afterCount).toBeGreaterThan(0);
    expect(afterCount).toBeLessThan(beforeCount);

    const allHaveNAP = await page.evaluate(() =>
      [...document.querySelectorAll<HTMLElement>('.badge-time')]
        .filter((b) => b.style.display !== 'none')
        .every((b) => (b.dataset.format ?? '').split(' ').includes('NAP')),
    );
    expect(allHaveNAP).toBe(true);
  });

  test('IMAX checkbox narrows to IMAX-format showings only', async ({ page }) => {
    const beforeCount = await visibleBadges(page);

    await page.evaluate(() => {
      (document.getElementById('format-imax') as HTMLInputElement).click();
    });

    const afterCount = await visibleBadges(page);
    expect(afterCount).toBeGreaterThan(0);
    expect(afterCount).toBeLessThan(beforeCount);

    const allImax = await page.evaluate(() =>
      [...document.querySelectorAll<HTMLElement>('.badge-time')]
        .filter((b) => b.style.display !== 'none')
        .every((b) => (b.dataset.format ?? '').split(' ').includes('IMAX')),
    );
    expect(allImax).toBe(true);
  });

  test('Od godziny = 18:00 hides earlier showings', async ({ page }) => {
    const beforeCount = await visibleBadges(page);

    await page.evaluate(() => {
      const sel = document.getElementById('from-hour') as HTMLSelectElement;
      sel.value = '18';
      (globalThis as { onFormatChange?: () => void }).onFormatChange?.();
    });

    const afterCount = await visibleBadges(page);
    expect(afterCount).toBeGreaterThan(0);
    expect(afterCount).toBeLessThan(beforeCount);

    // Every visible badge starts at or after 18:00.
    const allAfter18 = await page.evaluate(() =>
      [...document.querySelectorAll<HTMLElement>('.badge-time')]
        .filter((b) => b.style.display !== 'none')
        .every((b) => {
          const t = b.dataset.time ?? '';
          const [h, m] = t.split(':').map(Number);
          return h * 60 + m >= 18 * 60;
        }),
    );
    expect(allAfter18).toBe(true);
  });

  test('combining filters AND-narrows the set', async ({ page }) => {
    await page.evaluate(() => {
      (document.querySelector(
        'input[name="format-dim"][value="2D"]',
      ) as HTMLInputElement).click();
      (document.querySelector(
        'input[name="format-lang"][value="NAP"]',
      ) as HTMLInputElement).click();
    });

    const visible = await page.evaluate(() =>
      [...document.querySelectorAll<HTMLElement>('.badge-time')]
        .filter((b) => b.style.display !== 'none')
        .map((b) => (b.dataset.format ?? '').split(' ')),
    );
    expect(visible.length).toBeGreaterThan(0);
    expect(visible.every((tokens) => tokens.includes('2D') && tokens.includes('NAP'))).toBe(
      true,
    );
  });

  test('Filtry button textContent reflects active filter axes', async ({ page }) => {
    const baseLabel = await page.locator('#format-filter-btn').textContent();
    expect(baseLabel?.trim()).toBe('Filtry');

    await page.evaluate(() => {
      (document.querySelector(
        'input[name="format-dim"][value="2D"]',
      ) as HTMLInputElement).click();
    });

    const filteredLabel = await page.locator('#format-filter-btn').textContent();
    expect(filteredLabel?.trim()).not.toBe('Filtry');
    // Some encoding of "2D" — the label is built from active tokens.
    expect(filteredLabel).toMatch(/2D/);
  });
});
