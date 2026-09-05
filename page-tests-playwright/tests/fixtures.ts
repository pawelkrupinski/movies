import { test as base, expect } from '@playwright/test';
import { waitForGridSettled } from './helpers';

/**
 * `test` with a `page` that never hands a spec a listing still in motion.
 *
 * The listing renders TODAY and swaps the whole repertoire in after first paint
 * (`#film-grid[data-grid]` today → all), which is what takes the document from
 * 46k DOM tags to 9k. It also means the grid GAINS CARDS a moment after the
 * first ones attach, so a spec that measures, filters, and measures again can
 * straddle the swap and record it instead of the filter.
 *
 * ⚠️ WRAPPED AT `goto` RATHER THAN ADDED TO EACH SPEC, because the specs that
 * broke were the ones NOT using `gotoAndWaitForCards` — `filtry-cinemas` calls
 * `page.goto` directly, and fifteen other listing navigations do too. Fixing
 * them one at a time leaves the trap armed for the next test somebody writes;
 * this way a spec cannot opt out by accident. On a page with no grid (a film
 * page, the landing) the wait falls straight through.
 */
export const test = base.extend({
  page: async ({ page }, use) => {
    const goto = page.goto.bind(page);
    page.goto = async (url: string, opts?: Parameters<typeof goto>[1]) => {
      const response = await goto(url, opts);
      await waitForGridSettled(page);
      return response;
    };
    await use(page);
  },
});

export { expect };
