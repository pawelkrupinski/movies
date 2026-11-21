import { test, expect } from '@playwright/test';
import { gotoAndWaitForCards, pinDateFilterAnytime } from './helpers';

// A showtime pill carries its screening room in `data-room`; hovering it shows a
// small `#room-tooltip` naming the room just above the pill, and moving away
// hides it. Desktop hover only: a touch pointer gets the same tooltip on a long
// press (PageJsBehaviourSpec, "the room tooltip").
test('hovering a showtime names its room just above it, and leaving hides it', async ({ page }) => {
  test.slow();   // the whole repertoire in one document, as `day-pill-hover` notes
  await gotoAndWaitForCards(page, '/poznan/');
  const hoverCapable = await page.evaluate(() => matchMedia('(hover: hover) and (pointer: fine)').matches);
  test.skip(!hoverCapable, 'a touch pointer has no hover: the long press is the page spec\'s');
  await pinDateFilterAnytime(page);

  const pill = page.locator('#film-grid .badge-time[data-room]:visible').first();
  const room = await pill.getAttribute('data-room');
  const tooltip = page.locator('#room-tooltip');

  await pill.hover();
  await expect(tooltip).toBeVisible();
  await expect(tooltip).toHaveText(room!);
  const pillBox = (await pill.boundingBox())!;
  const tipBox = (await tooltip.boundingBox())!;
  // Placed from the tooltip's `offsetHeight`, a whole number, over a pill at a fractional
  // offset: its bottom can sit a fraction of a pixel past the 4px gap (WebKit, 0.31px).
  expect(tipBox.y + tipBox.height).toBeLessThanOrEqual(pillBox.y + 1);

  await page.mouse.move(1, 1);
  await expect(tooltip).toBeHidden();
});
