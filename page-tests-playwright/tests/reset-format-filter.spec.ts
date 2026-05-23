import { test, expect } from '@playwright/test';
import { pinDateFilterAnytime } from './helpers';

// "Wyczyść" inside the Filtry panel — `resetFormatFilter` flips
// every Wymiar / Wersja radio back to ""/checked, unchecks IMAX,
// clears from-hour/from-minute, and closes the panel. The button is
// just `<button onclick="resetFormatFilter()">` so we drive the
// function the same way the click would.

test.describe('Filtry > Wyczyść', () => {

  test('resets every format axis and the from-hour pickers', async ({ page }) => {
    await page.goto('/');
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

    // Sanity: the button shows it's narrowed before we reset.
    const narrowed = await page.locator('#format-filter-btn').textContent();
    expect(narrowed?.trim()).not.toBe('Filtry');

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
      btnLabel:  (document.getElementById('format-filter-btn') as HTMLElement).textContent?.trim(),
    }));

    expect(state.dim).toBe('');
    expect(state.lang).toBe('');
    expect(state.imax).toBe(false);
    expect(state.fromHour).toBe('');
    expect(state.fromMin).toBe('0');
    // Reset closes the panel — `display: 'none'`.
    expect(state.panel).toBe('none');
    expect(state.btnLabel).toBe('Filtry');
  });
});
