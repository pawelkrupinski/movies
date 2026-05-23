import { test, expect } from '@playwright/test';
import { pinDateFilterAnytime } from './helpers';

// `#no-films` is the empty-state placeholder. Hidden by default;
// shown when `applyFilters` produces zero visible cards (e.g. via
// a search query no fixture row matches).

test.describe('empty state', () => {

  test('shows "Brak repertuaru." when the search yields zero matches', async ({ page }) => {
    await page.goto('/');
    await pinDateFilterAnytime(page);

    // Hidden by default.
    const initialDisplay = await page.evaluate(
      () => (document.getElementById('no-films') as HTMLElement | null)?.style.display ?? null
    );
    expect(initialDisplay).toBe('none');

    // A clearly-non-existent substring. The fixture corpus is a
    // Polish-language film schedule, so a random ASCII string never
    // matches.
    await page.evaluate(() => {
      const input = document.getElementById('search-input') as HTMLInputElement;
      input.value = 'zzzzz_no_match';
      (globalThis as { applyFilters?: () => void }).applyFilters?.();
    });

    const visibleCards = await page.evaluate(
      () =>
        [...document.querySelectorAll<HTMLElement>('.col[data-title]')]
          .filter((c) => c.style.display !== 'none').length
    );
    expect(visibleCards).toBe(0);

    // The empty-state element is shown (style.display !== 'none').
    const noFilmsDisplay = await page.evaluate(
      () => (document.getElementById('no-films') as HTMLElement).style.display
    );
    expect(noFilmsDisplay).not.toBe('none');

    // Text is the expected Polish phrase.
    await expect(page.locator('#no-films')).toContainText('Brak repertuaru.');
  });

  test('hides the empty state again when the search clears', async ({ page }) => {
    await page.goto('/');
    await pinDateFilterAnytime(page);

    await page.evaluate(() => {
      const input = document.getElementById('search-input') as HTMLInputElement;
      input.value = 'zzzzz_no_match';
      (globalThis as { applyFilters?: () => void }).applyFilters?.();
    });
    await page.evaluate(() => {
      const input = document.getElementById('search-input') as HTMLInputElement;
      input.value = '';
      (globalThis as { applyFilters?: () => void }).applyFilters?.();
    });

    const visible = await page.evaluate(
      () =>
        [...document.querySelectorAll<HTMLElement>('.col[data-title]')]
          .filter((c) => c.style.display !== 'none').length
    );
    expect(visible).toBeGreaterThan(0);

    const noFilmsDisplay = await page.evaluate(
      () => (document.getElementById('no-films') as HTMLElement).style.display
    );
    expect(noFilmsDisplay).toBe('none');
  });
});
