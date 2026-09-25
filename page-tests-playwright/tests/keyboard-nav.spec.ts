import { test, expect } from '@playwright/test';
import { gotoAndWaitForCards, setDateFilter, waitForCards } from './helpers';

// Document-level keydown handler in shared.js maps ArrowLeft / Right
// to `stepDate(-1 / 1)` — the same function the navbar's date-arrow
// buttons call. Keyboard-only users can cycle the date filter
// without reaching for the on-screen controls.

test.describe('keyboard arrow date navigation', { tag: '@agnostic' }, () => {

  test.beforeEach(async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    // Land on a known starting value so the cycle is deterministic.
    await setDateFilter(page, 'today');
  });

  // The arrow keys now route through the carousel's `animateToDay`, which
  // commits the `#date-filter` value only after the slide settles — so the
  // value change is asserted with `expect.poll`, not read synchronously.
  test('ArrowRight advances the date filter', async ({ page }) => {
    expect(await page.locator('#date-filter').inputValue()).toBe('today');

    await page.keyboard.press('ArrowRight');

    await expect.poll(() => page.locator('#date-filter').inputValue()).not.toBe('today');
  });

  test('ArrowLeft returns to the previous filter', async ({ page }) => {
    await page.keyboard.press('ArrowRight');
    await expect.poll(() => page.locator('#date-filter').inputValue()).not.toBe('today');
    const advanced = await page.locator('#date-filter').inputValue();
    await page.keyboard.press('ArrowLeft');
    await expect.poll(() => page.locator('#date-filter').inputValue()).toBe('today');
    expect(advanced).not.toBe('today');
  });

  // A throttled / background tab or a stalled renderer can deliver the frame
  // that starts the slide long after the slide's own duration. The commit must
  // wait for that frame: committing first and letting the late frame translate
  // the bare `#view-root` leaves the grid blank, parked two widths off-screen.
  // Holding every requestAnimationFrame callback past the slide forces it.
  test('a slide whose first frame arrives late still leaves the grid on-screen', async ({ page }) => {
    await page.emulateMedia({ reducedMotion: 'no-preference' });
    await page.evaluate(() => {
      const w = window as unknown as { __held: FrameRequestCallback[]; __realRaf: typeof requestAnimationFrame };
      w.__held = [];
      w.__realRaf = window.requestAnimationFrame;
      window.requestAnimationFrame = (cb) => { w.__held.push(cb); return w.__held.length; };
    });

    await page.keyboard.press('ArrowRight');
    await page.waitForTimeout(1200);   // well past the ≤550ms slide + its fallback margin
    await page.evaluate(() => {
      const w = window as unknown as { __held: FrameRequestCallback[]; __realRaf: typeof requestAnimationFrame };
      window.requestAnimationFrame = w.__realRaf;
      w.__held.splice(0).forEach(cb => cb(performance.now()));
    });

    await expect.poll(() => page.locator('#date-filter').inputValue()).not.toBe('today');
    await expect.poll(() => page.evaluate(() => document.querySelectorAll('#day-track > .day-col').length)).toBe(0);
    await page.waitForTimeout(800);    // let any straggling frame or transition land
    expect(await page.evaluate(() => document.getElementById('day-track')!.style.transform)).toBe('');
    expect(await page.evaluate(() =>
      Math.round(document.getElementById('view-root')!.getBoundingClientRect().left))).toBe(0);
  });

  test('arrow keys do nothing when focus is inside the search input', async ({ page }) => {
    const search = page.locator('#search-input');
    const hidden = !(await search.isVisible());
    test.skip(hidden, 'search input is display:none on ultra-narrow viewports');

    await search.focus();
    const before = await page.locator('#date-filter').inputValue();
    await page.keyboard.press('ArrowRight');
    // Give any (erroneous) slide a beat to commit, then assert no change.
    await page.waitForTimeout(400);
    const after = await page.locator('#date-filter').inputValue();
    expect(after).toBe(before);
  });
});
