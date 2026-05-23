import { test, expect } from '@playwright/test';
import { getVisibleTitles, setDateFilter, waitForCards } from './helpers';

// Date-filter narrowing: 'today' is a subset of 'week' is a subset
// of 'anytime'. The fixture corpus is pinned to 2026-05-17; the
// page's `dateBounds()` reads the live `new Date()`, so the
// `today`/`week` sets depend on the runner's wall-clock relative
// to the fixture. We don't pin specific counts — just relative
// containment, which is true regardless of when the suite runs.

const visibleCount = async (page: import('@playwright/test').Page) =>
  (await getVisibleTitles(page)).length;

test.describe('date filter', () => {

  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await waitForCards(page);
  });

  test('week is a subset of anytime', async ({ page }) => {
    await setDateFilter(page, 'anytime');
    const anytime = await visibleCount(page);
    await setDateFilter(page, 'week');
    const week = await visibleCount(page);
    expect(anytime).toBeGreaterThan(0);
    expect(week).toBeLessThanOrEqual(anytime);
  });

  test('today is a subset of week', async ({ page }) => {
    await setDateFilter(page,'week');
    const week = await visibleCount(page);
    await setDateFilter(page,'today');
    const today = await visibleCount(page);
    expect(today).toBeLessThanOrEqual(week);
  });

  test('switching filters preserves the relative ordering today ≤ week ≤ anytime', async ({ page }) => {
    await setDateFilter(page,'today');
    const today = await visibleCount(page);
    await setDateFilter(page,'week');
    const week = await visibleCount(page);
    await setDateFilter(page,'anytime');
    const anytime = await visibleCount(page);
    expect(today).toBeLessThanOrEqual(week);
    expect(week).toBeLessThanOrEqual(anytime);
  });
});
