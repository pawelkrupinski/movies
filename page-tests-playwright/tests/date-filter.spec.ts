import { test, expect } from '@playwright/test';

// Date-filter narrowing: 'today' is a subset of 'week' is a subset
// of 'anytime'. The fixture corpus is pinned to 2026-05-17; the
// page's `dateBounds()` reads the live `new Date()`, so the
// `today`/`week` sets depend on the runner's wall-clock relative
// to the fixture. We don't pin specific counts — just relative
// containment, which is true regardless of when the suite runs.

const visibleCount = (page: import('@playwright/test').Page) =>
  page.evaluate(
    () =>
      [...document.querySelectorAll<HTMLElement>('.col[data-title]')].filter(
        (c) => c.style.display !== 'none',
      ).length,
  );

const setFilter = (page: import('@playwright/test').Page, value: string) =>
  page.evaluate((v: string) => {
    const sel = document.getElementById('date-filter') as HTMLSelectElement;
    sel.value = v;
    (globalThis as { applyFilters?: () => void }).applyFilters?.();
  }, value);

test.describe('date filter', () => {

  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForSelector('.col[data-title]', { state: 'attached' });
  });

  test('week is a subset of anytime', async ({ page }) => {
    await setFilter(page, 'anytime');
    const anytime = await visibleCount(page);
    await setFilter(page, 'week');
    const week = await visibleCount(page);
    expect(anytime).toBeGreaterThan(0);
    expect(week).toBeLessThanOrEqual(anytime);
  });

  test('today is a subset of week', async ({ page }) => {
    await setFilter(page, 'week');
    const week = await visibleCount(page);
    await setFilter(page, 'today');
    const today = await visibleCount(page);
    expect(today).toBeLessThanOrEqual(week);
  });

  test('switching filters preserves the relative ordering today ≤ week ≤ anytime', async ({ page }) => {
    await setFilter(page, 'today');
    const today = await visibleCount(page);
    await setFilter(page, 'week');
    const week = await visibleCount(page);
    await setFilter(page, 'anytime');
    const anytime = await visibleCount(page);
    expect(today).toBeLessThanOrEqual(week);
    expect(week).toBeLessThanOrEqual(anytime);
  });
});
