import { test, expect } from '@playwright/test';
import { gotoAndWaitForCards } from './helpers';

// The day pills carry two highlights: `.active` (light blue #aad4ff = the
// selected day) and `:hover` (dark blue #3a3a6e). On a phone, tapping a pill
// used to leave a *sticky* `:hover` on it: after the user swiped to another
// day the `.active` light-blue moved on, but the dark-blue hover stayed parked
// on the pill they'd tapped — two highlights at once. The fix gates `:hover`
// behind `@media (hover: hover) and (pointer: fine)` so touch pointers never
// apply it. This asserts that gate per the project's real pointer capability.
const HOVER_BLUE = 'rgb(58, 58, 110)';   // #3a3a6e — the dark hover background

test('day-pill hover styling applies only to hover-capable (non-touch) pointers', async ({ page }) => {
  await gotoAndWaitForCards(page, '/poznan/?date=anytime');

  const hoverCapable = await page.evaluate(() =>
    matchMedia('(hover: hover) and (pointer: fine)').matches);

  // A pill that is NOT the selected day, so its only candidate background is
  // the hover rule (the `.active` rule, which wins by source order, can't mask
  // the bug here).
  const pill = page.locator('#day-pills .day-pill:not(.active)').first();
  await pill.hover();
  // `.day-pill` has a .15s background-color transition, so let it settle before
  // sampling — otherwise a hover-capable pointer reads the pre-transition
  // transparent start and looks like the touch case.
  const bg = () => pill.evaluate(el => getComputedStyle(el).backgroundColor);
  if (hoverCapable) {
    await expect.poll(bg).toBe(HOVER_BLUE);    // desktop / mouse: hover still works
  } else {
    await page.waitForTimeout(250);
    expect(await bg()).not.toBe(HOVER_BLUE);   // phone: no sticky dark-blue hover
  }
});
