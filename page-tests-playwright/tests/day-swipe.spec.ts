import { test, expect } from '@playwright/test';
import type { Page } from '@playwright/test';
import { waitForCards, cdpSwipe, setDateFilter } from './helpers';

// The day grid is the centre column of a prev|current|next carousel inside
// `#day-track`. A horizontal swipe translates the track to reveal the
// neighbouring day's column (a clone of `#film-grid` filtered to that day),
// WRAPPING the `#date-filter` option list: swipe LEFT = next day, RIGHT =
// previous. The arrow buttons, the Left/Right keys and tapping a day pill route
// through the SAME slide. The gesture is driven with REAL touch events via CDP
// (not synthetic PointerEvents), so it exercises the same pointer/touch path
// the production handlers use. CDP touch injection is chromium-only and the
// swipe is gated to coarse pointers (phones), so this runs on the
// mobile-chromium projects.
test.describe('day-swipe', () => {
  test.beforeEach(async ({ page, browserName }) => {
    test.skip(browserName !== 'chromium', 'CDP touch injection is chromium-only');
    // Force the animated slide path (not the reduced-motion instant commit) so
    // the in-flight `.day-track--armed` assertions are deterministic.
    await page.emulateMedia({ reducedMotion: 'no-preference' });
    await page.goto('/poznan/?date=anytime');   // anytime → cards present regardless of wall-clock
    const coarse = await page.evaluate(() => matchMedia('(pointer: coarse)').matches);
    test.skip(!coarse, 'day-swipe is gated to coarse pointers (phones)');
    await waitForCards(page);
  });

  const dayIndex = (page: Page) =>
    page.evaluate(() => (document.getElementById('date-filter') as HTMLSelectElement).selectedIndex);
  const dayCount = (page: Page) =>
    page.evaluate(() => (document.getElementById('date-filter') as HTMLSelectElement).options.length);
  // The slide settles once `#day-track`'s clones are gone and its inline
  // transform is cleared (back to the single-column resting layout).
  const settled = (page: Page) =>
    page.evaluate(() => {
      const track = document.getElementById('day-track')!;
      const clones = track.querySelectorAll(':scope > .day-col').length;
      const t = getComputedStyle(track).transform;
      return clones === 0 && (t === 'none' || t === 'matrix(1, 0, 0, 1, 0, 0)');
    });

  async function startOn(page: Page, value: string): Promise<void> {
    // `#date-filter` is visually-hidden, so drive it through the same evaluate
    // path the pills use rather than a (visibility-gated) selectOption.
    await setDateFilter(page, value);
    await expect.poll(() => settled(page)).toBe(true);
  }

  test('swipe left advances to the next day; swipe right goes back', async ({ page }) => {
    await startOn(page, 'today');               // selectedIndex 0
    await cdpSwipe(page, 'left');                   // next day
    await expect.poll(() => dayIndex(page)).toBe(1);
    await expect.poll(() => settled(page)).toBe(true);
    await cdpSwipe(page, 'right');                  // previous day
    await expect.poll(() => dayIndex(page)).toBe(0);
  });

  test('swipe right on the first day wraps to the last', async ({ page }) => {
    await startOn(page, 'today');               // selectedIndex 0
    const last = (await dayCount(page)) - 1;
    await cdpSwipe(page, 'right');                  // previous, wraps past the start
    await expect.poll(() => dayIndex(page)).toBe(last);
  });

  // NOTE: the "neighbour revealed mid-drag at the same scroll offset" assertion
  // lived here, driven by an indefinitely-HELD CDP touch (touchStart + moves, no
  // touchEnd) so it could inspect the in-flight carousel. CI's headless Chromium
  // cancels such an unterminated synthetic touch (pointercancel), snapping the
  // carousel back and unmounting the neighbour before the assertion runs — so it
  // failed ONLY on CI and passed every local run, making it impossible to
  // validate here. The same contract (neighbour mounted at the centre's scroll
  // offset mid-drag, scroll-to-top on commit, scroll preserved on snap-back) is
  // asserted deterministically in PageJsBehaviourSpec's "the day carousel" tests
  // (sbt real-Chrome, green on CI), so the behaviour stays covered.

  test('a committed swipe scrolls to top; a sub-threshold drag leaves scroll', async ({ page }) => {
    await startOn(page, 'anytime');

    // Sub-threshold: a short held-then-released drag snaps back, scroll kept.
    await page.evaluate(() => window.scrollTo(0, 300));
    const box = (await page.locator('#film-grid').boundingBox())!;
    const y = box.y + Math.min(box.height / 2, 120);
    const x0 = box.x + box.width * 0.5;
    // The drag must be sub-threshold by BOTH commit paths on EVERY viewport:
    // distance (< COMMIT_FRACTION·width) AND flick (< FLICK_MIN_PX = 24px, since
    // synthetic CDP moves fire fast → high velocity). A width-relative distance
    // flick-commits on a wide landscape grid (15% of width > 24px), so use a
    // fixed ~18px: past the ~10px deadzone (it arms) but under the flick floor.
    const subDx = 18;
    const client = await page.context().newCDPSession(page);
    await client.send('Input.dispatchTouchEvent', { type: 'touchStart', touchPoints: [{ x: x0, y }] });
    for (let i = 1; i <= 6; i++) {
      await client.send('Input.dispatchTouchEvent',
        { type: 'touchMove', touchPoints: [{ x: x0 - (subDx * i) / 6, y }] });
    }
    await client.send('Input.dispatchTouchEvent', { type: 'touchEnd', touchPoints: [] });
    await client.detach();
    await expect.poll(() => settled(page)).toBe(true);
    expect(await page.evaluate(() => Math.round(window.scrollY))).toBe(300);

    // A committed swipe (past threshold) scrolls to top. Swipe at a FIXED
    // on-screen y (the grid is scrolled, so a grid-relative y would land
    // off-viewport); a horizontal drag across most of the width commits.
    await page.evaluate(() => window.scrollTo(0, 300));
    const vw = page.viewportSize()!.width;
    // Viewport-centre y so the touch lands on the grid in landscape too — a
    // fixed 380 sits below a short landscape viewport and would miss.
    const yMid = Math.round(page.viewportSize()!.height / 2);
    const c2 = await page.context().newCDPSession(page);
    await c2.send('Input.dispatchTouchEvent', { type: 'touchStart', touchPoints: [{ x: vw * 0.85, y: yMid }] });
    for (let i = 1; i <= 12; i++) {
      await c2.send('Input.dispatchTouchEvent',
        { type: 'touchMove', touchPoints: [{ x: vw * 0.85 - (vw * 0.7 * i) / 12, y: yMid }] });
    }
    await c2.send('Input.dispatchTouchEvent', { type: 'touchEnd', touchPoints: [] });
    await c2.detach();
    // anytime (last index) + swipe-left wraps to today (index 0): the day
    // actually committed, which is what triggers the scroll-to-top.
    await expect.poll(() => dayIndex(page)).toBe(0);
    await expect.poll(() => settled(page)).toBe(true);
    expect(await page.evaluate(() => Math.round(window.scrollY))).toBe(0);
  });

  test('Left/Right keys and a day pill change the day through the carousel', async ({ page }) => {
    // The ‹ › arrows are hidden on portrait phones (where this swipe spec runs),
    // so this covers the OTHER entry points that share the slide: the keyboard
    // and a pill tap. The arrow path is exercised by date-stepper.spec on
    // desktop, and the in-flight `.day-track--armed` arming is asserted
    // deterministically in PageJsBehaviourSpec — here we assert the committed
    // OUTCOME (the day changed + settled), which isn't racy against the ~220ms
    // slide the way reading the transient armed class right after a click is.

    // Right / Left keyboard step one day each.
    await startOn(page, 'today');
    await page.keyboard.press('ArrowRight');
    await expect.poll(() => dayIndex(page)).toBe(1);
    await expect.poll(() => settled(page)).toBe(true);
    await page.keyboard.press('ArrowLeft');
    await expect.poll(() => dayIndex(page)).toBe(0);
    await expect.poll(() => settled(page)).toBe(true);

    // Tapping a pill jumps straight to that day and reflects it in the URL.
    await startOn(page, 'today');
    await page.locator('.day-pill[data-day="anytime"]').click();
    await expect.poll(() => settled(page)).toBe(true);
    await expect
      .poll(() => page.evaluate(() => new URL(location.href).searchParams.get('date')))
      .toBe('anytime');
  });
});
