import { test, expect } from './fixtures';
import { firstVisibleCard, getVisibleTitles, gotoAndWaitForCards, setDateFilter, waitForCards } from './helpers';

// Broad-strokes liveness check against kinowo.net. The home page
// 200s with at least one visible card, the date filter narrows the
// set rather than blowing it up, and a film detail page renders the
// expected title. Catches deploys that ship a green build but break
// downstream paths (Mongo connection lost, view template renames,
// applyFilters re-throwing, etc.) — the card-tap spec only covers the
// touch-handler contract.

const visibleCardCount = async (page: import('@playwright/test').Page) =>
  (await getVisibleTitles(page)).length;

test.describe('kinowo.net smoke', { tag: '@agnostic' }, () => {
  test('home page renders at least one visible card', async ({ page }) => {
    const resp = await page.goto('/poznan/', { waitUntil: 'domcontentloaded' });
    expect(resp?.status()).toBe(200);
    await waitForCards(page);
    await setDateFilter(page, 'anytime');
    expect(await visibleCardCount(page)).toBeGreaterThan(0);
  });

  test('date filter narrows the visible set', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');

    await setDateFilter(page, 'anytime');
    const anytimeCount = await visibleCardCount(page);

    await setDateFilter(page, 'today');
    const todayCount = await visibleCardCount(page);

    // "today" is a subset of "anytime" by construction. Equal is
    // tolerated for the rare day where every scheduled film also
    // screens today.
    expect(todayCount).toBeLessThanOrEqual(anytimeCount);
    expect(anytimeCount).toBeGreaterThan(0);
  });

  test('film detail page renders title from home selection', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    await setDateFilter(page, 'anytime');
    // One read, so the title asserted below and the slug navigated to are the
    // SAME card — see `firstVisibleCard`.
    const card = await firstVisibleCard(page);
    expect(card).toBeTruthy();
    const { title, slug } = card!;

    // `domcontentloaded`: the `/movie` page's `load` event waits on
    // poster-proxy images + the trailer iframe, which can stall the full
    // timeout on a contended runner. The status + server-rendered title we
    // assert on are present at DCL.
    const resp = await page.goto(`/poznan/movie/${slug}`, { waitUntil: 'domcontentloaded' });
    expect(resp?.status()).toBe(200);
    // Don't pin to a specific element — view templates evolve. The
    // contract is just "the film's title shows up on its detail page".
    await expect(page.locator('body')).toContainText(title);
  });
});
