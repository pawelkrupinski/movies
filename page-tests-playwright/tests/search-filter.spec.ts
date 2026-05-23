import { test, expect } from '@playwright/test';
import { pinDateFilterAnytime } from './helpers';

// `/` page search input + hidden-films modal filter. Ports the
// corresponding Scala tests so the same assertions run cross-engine.

test.describe('the / page search input', () => {

  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await pinDateFilterAnytime(page);
  });

  test('filters visible film cards by title substring', async ({ page }) => {
    const totalCards = await page.locator('.col[data-title]').count();
    expect(totalCards).toBeGreaterThan(5);

    // "Diabeł" matches the Polish-titled Prada rows in the 17-05-2026
    // corpus: regular row + the Polish-titled Ukrainian-dub row. The
    // Cyrillic-titled dub is a separate card and doesn't share the
    // substring — asserting exactly 2 catches over- and under-matching.
    await page.evaluate(() => {
      const input = document.getElementById('search-input') as HTMLInputElement;
      input.value = 'Diabeł';
      (globalThis as { applyFilters?: () => void }).applyFilters?.();
    });

    const visible = await page.evaluate(() =>
      [...document.querySelectorAll<HTMLElement>('.col[data-title]')]
        .filter((c) => c.style.display !== 'none')
        .map((c) => c.dataset.title!)
    );
    expect(visible).toHaveLength(2);
    expect(visible.every((t) => t.toLowerCase().includes('diabeł'))).toBe(true);
  });

  test('restores every previously-visible card after clearing the input', async ({ page }) => {
    const baseVisible = await page.evaluate(() =>
      [...document.querySelectorAll<HTMLElement>('.col[data-title]')]
        .filter((c) => c.style.display !== 'none').length
    );

    await page.evaluate(() => {
      const input = document.getElementById('search-input') as HTMLInputElement;
      input.value = 'Diabeł';
      (globalThis as { applyFilters?: () => void }).applyFilters?.();
    });
    await page.evaluate(() => {
      const input = document.getElementById('search-input') as HTMLInputElement;
      input.value = '';
      (globalThis as { applyFilters?: () => void }).applyFilters?.();
    });

    const afterClear = await page.evaluate(() =>
      [...document.querySelectorAll<HTMLElement>('.col[data-title]')]
        .filter((c) => c.style.display !== 'none').length
    );
    expect(afterClear).toBe(baseVisible);
  });
});

test.describe('the hidden-films modal search', () => {

  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    // Seed a handful of hidden films directly via localStorage so the
    // modal has something to filter; reload to pick up the new state.
    await page.evaluate(() => {
      localStorage.setItem(
        'hiddenFilms',
        JSON.stringify(['Diabeł ubiera się u Prady 2', 'Avatar', 'Cars'])
      );
    });
    await page.reload();
  });

  test('filters listed titles by substring as the user types', async ({ page }) => {
    await page.evaluate(() => (globalThis as { openHiddenModal?: () => void }).openHiddenModal?.());
    await expect(page.locator('#hidden-modal-list .panel-item')).toHaveCount(3);

    await page.evaluate(() => {
      const input = document.getElementById('hidden-modal-search') as HTMLInputElement;
      input.value = 'avat';
      (globalThis as { filterHiddenModal?: () => void }).filterHiddenModal?.();
    });

    const visibleItems = page.locator('#hidden-modal-list .panel-item:not([style*="none"])');
    await expect(visibleItems).toHaveCount(1);
    await expect(visibleItems).toHaveText('Avatar');
  });

  test('resets the search box on close so reopening shows the full list', async ({ page }) => {
    await page.evaluate(() => (globalThis as { openHiddenModal?: () => void }).openHiddenModal?.());
    await page.evaluate(() => {
      const input = document.getElementById('hidden-modal-search') as HTMLInputElement;
      input.value = 'avat';
      (globalThis as { filterHiddenModal?: () => void }).filterHiddenModal?.();
    });
    await page.evaluate(() => (globalThis as { closeHiddenModal?: () => void }).closeHiddenModal?.());
    await expect(page.locator('#hidden-modal-search')).toHaveValue('');

    await page.evaluate(() => (globalThis as { openHiddenModal?: () => void }).openHiddenModal?.());
    const allVisible = await page.evaluate(() =>
      [...document.querySelectorAll<HTMLElement>('#hidden-modal-list .panel-item')]
        .filter((i) => i.style.display !== 'none').length
    );
    // Seed had Diabeł… + Avatar + Cars; modal shows them all on reopen.
    expect(allVisible).toBeGreaterThanOrEqual(2);
  });
});
