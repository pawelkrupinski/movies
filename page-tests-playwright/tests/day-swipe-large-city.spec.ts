import { test, expect } from '@playwright/test';
import { waitForCards, cdpSwipe } from './helpers';

// `/many-showtimes` re-seats the fixture corpus with 10,500 synthetic
// showtimes on one film — enough to cross
// `MovieControllerService.LargeCityShowtimeThreshold`. Above that threshold,
// cloning the whole `#film-grid` per neighbour day (`buildDayColumn`) is
// expensive enough to stutter a real swipe (measured on Salt Lake City: ~17.5k
// DOM nodes per clone, two clones mounted at gesture start), so a coarse
// pointer skips the clone-and-slide carousel entirely and re-filters the live
// grid in place — see shared.js `usesInstantDayChange`. This is the same
// gesture path `day-swipe.spec.ts` exercises on a normal-size city, asserting
// the OPPOSITE: no `.day-col` is ever mounted, `#day-track` never arms.
test.describe('day-swipe on a large city', () => {
  test.beforeEach(async ({ page, browserName }) => {
    test.skip(browserName !== 'chromium', 'CDP touch injection is chromium-only');
    await page.emulateMedia({ reducedMotion: 'no-preference' });
    await page.goto('/poznan/many-showtimes', { waitUntil: 'domcontentloaded' });
    const coarse = await page.evaluate(() => matchMedia('(pointer: coarse)').matches);
    test.skip(!coarse, 'day-swipe is gated to coarse pointers (phones)');
    await waitForCards(page);
  });

  const dayValue = (page: import('@playwright/test').Page) =>
    page.evaluate(() => (document.getElementById('date-filter') as HTMLSelectElement).value);
  const cloneCount = (page: import('@playwright/test').Page) =>
    page.evaluate(() => document.querySelectorAll('#day-track > .day-col').length);
  const isArmed = (page: import('@playwright/test').Page) =>
    page.evaluate(() => document.getElementById('day-track')!.classList.contains('day-track--armed'));

  test('is flagged large-city and never mounts a neighbour clone on a committed swipe', async ({ page }) => {
    expect(await page.evaluate(() => document.getElementById('view-root')!.dataset.largeCity)).toBe('true');
    expect(await dayValue(page)).toBe('today');

    await cdpSwipe(page, 'left');   // next day — same commit-threshold drag day-swipe.spec drives
    await expect.poll(() => dayValue(page)).toBe('tomorrow');
    // Unlike the small-city spec, no clone was ever mounted and the track
    // never armed — the whole gesture re-filtered the live grid in place.
    expect(await cloneCount(page)).toBe(0);
    expect(await isArmed(page)).toBe(false);
  });

  test('never arms the track on a sub-threshold drag either', async ({ page }) => {
    const box = (await page.locator('#film-grid').boundingBox())!;
    const y = box.y + Math.min(box.height / 2, 120);
    const x0 = box.x + box.width * 0.5;
    const client = await page.context().newCDPSession(page);
    await client.send('Input.dispatchTouchEvent', { type: 'touchStart', touchPoints: [{ x: x0, y }] });
    for (let i = 1; i <= 6; i++) {
      await client.send('Input.dispatchTouchEvent',
        { type: 'touchMove', touchPoints: [{ x: x0 - (18 * i) / 6, y }] });
    }
    await client.send('Input.dispatchTouchEvent', { type: 'touchEnd', touchPoints: [] });
    await client.detach();
    expect(await cloneCount(page)).toBe(0);
    expect(await isArmed(page)).toBe(false);
    expect(await dayValue(page)).toBe('today');   // unchanged
  });

  test('a day-pill tap updates instantly with no armed track', async ({ page }) => {
    await page.locator('.day-pill[data-day="anytime"]').click();
    await expect.poll(() => dayValue(page)).toBe('anytime');
    expect(await cloneCount(page)).toBe(0);
    expect(await isArmed(page)).toBe(false);
  });
});
