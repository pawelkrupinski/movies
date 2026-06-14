import { test, expect } from '@playwright/test';
import { pinDateFilterAnytime } from './helpers';

// "Wyczyść" inside the Filtry panel — `resetFormatFilter` flips
// every Wymiar / Wersja radio back to ""/checked, unchecks IMAX,
// clears from-hour/from-minute, re-enables every cinema, and closes
// the panel. The button is just `<button onclick="resetFormatFilter()">`
// so we drive the function the same way the click would.
//
// The Filtry trigger is now an icon-only funnel that gains the accent
// `.filters-active` class while any clearable filter is set, so the
// reset's effect is asserted on that class rather than on a text label.

test.describe('Filtry > Wyczyść', { tag: '@agnostic' }, () => {

  test('resets every format axis and the from-hour pickers', async ({ page }) => {
    await page.goto('/poznan/');
    await pinDateFilterAnytime(page);

    // Pile on every active axis the panel exposes so the reset has
    // a real diff to undo.
    await page.evaluate(() => {
      (document.querySelector('input[name="format-dim"][value="2D"]') as HTMLInputElement).click();
      (document.querySelector('input[name="format-lang"][value="NAP"]') as HTMLInputElement).click();
      (document.getElementById('format-imax') as HTMLInputElement).click();
      const fromHour = document.getElementById('from-hour') as HTMLSelectElement;
      fromHour.value = '18';
      const fromMinute = document.getElementById('from-minute') as HTMLSelectElement;
      fromMinute.value = '30';
      (globalThis as { onFormatChange?: () => void }).onFormatChange?.();
    });

    // Sanity: the funnel icon lights up (accent `.filters-active`) before reset.
    await expect(page.locator('#format-filter-btn')).toHaveClass(/filters-active/);

    await page.evaluate(() =>
      (globalThis as { resetFormatFilter?: () => void }).resetFormatFilter?.()
    );

    const state = await page.evaluate(() => ({
      dim:       (document.querySelector('input[name="format-dim"]:checked')  as HTMLInputElement | null)?.value ?? null,
      lang:      (document.querySelector('input[name="format-lang"]:checked') as HTMLInputElement | null)?.value ?? null,
      imax:      (document.getElementById('format-imax') as HTMLInputElement).checked,
      fromHour:  (document.getElementById('from-hour')   as HTMLSelectElement).value,
      fromMin:   (document.getElementById('from-minute') as HTMLSelectElement).value,
      panel:     (document.getElementById('format-panel') as HTMLElement).style.display,
    }));

    expect(state.dim).toBe('');
    expect(state.lang).toBe('');
    expect(state.imax).toBe(false);
    expect(state.fromHour).toBe('');
    expect(state.fromMin).toBe('0');
    // Reset closes the panel — `display: 'none'`.
    expect(state.panel).toBe('none');
    // …and the funnel icon goes back to neutral (no `.filters-active`).
    await expect(page.locator('#format-filter-btn')).not.toHaveClass(/filters-active/);
  });
});
