import { test, expect } from '@playwright/test';
import { pinDateFilterAnytime } from './helpers';

// A US listing prints its pills on a 12-hour clock ("7:30 PM", `ClockStyle.TwelveHour`) and
// labels the from-hour picker the same way; the listing script reads each pill's time back
// off its text for the from-hour filter, the sort and the expiry. `/poznan/us-clock` is the
// fixture corpus rendered as a US deployment would (FixtureServerMain).

const TWELVE_HOUR = /^(\d{1,2}):(\d{2}) ([AP])M$/;

function minutesOf(text: string): number {
  const m = TWELVE_HOUR.exec(text);
  if (!m) throw new Error(`not a 12-hour time: "${text}"`);
  return ((parseInt(m[1], 10) % 12) + (m[3] === 'P' ? 12 : 0)) * 60 + parseInt(m[2], 10);
}

async function visiblePillTimes(page: import('@playwright/test').Page): Promise<string[]> {
  return page.evaluate(() =>
    [...document.querySelectorAll<HTMLElement>('.badge-time')]
      .filter(b => b.offsetParent !== null)
      .map(b => (b.firstChild?.nodeValue ?? '').trim()));
}

test.describe('12-hour clock (US)', { tag: '@agnostic' }, () => {

  test('prints every pill and from-hour label as a 12-hour time', async ({ page }) => {
    await page.goto('/poznan/us-clock', { waitUntil: 'domcontentloaded' });
    await pinDateFilterAnytime(page);
    const times = await visiblePillTimes(page);
    expect(times.length).toBeGreaterThan(0);
    for (const t of times) expect(t).toMatch(TWELVE_HOUR);
    await expect(page.locator('#from-hour option[value="0"]')).toHaveText('12 AM');
    await expect(page.locator('#from-hour option[value="18"]')).toHaveText('6 PM');
  });

  test('the from-hour filter keeps only pills from 6 PM on', async ({ page }) => {
    await page.goto('/poznan/us-clock', { waitUntil: 'domcontentloaded' });
    await pinDateFilterAnytime(page);
    const before = await visiblePillTimes(page);
    await page.evaluate(() => {
      (document.getElementById('from-hour') as HTMLSelectElement).value = '18';
      (globalThis as { onFormatChange?: () => void }).onFormatChange?.();
    });
    const after = await visiblePillTimes(page);
    expect(after.length).toBeGreaterThan(0);
    expect(after.length).toBeLessThan(before.length);
    for (const t of after) expect(minutesOf(t)).toBeGreaterThanOrEqual(18 * 60);
  });

  test('a pill fits on one line', async ({ page }) => {
    await page.goto('/poznan/us-clock', { waitUntil: 'domcontentloaded' });
    await pinDateFilterAnytime(page);
    // "11:45 PM" plus its format tokens is wider than "23:45": a pill that broke inside
    // would stand taller than the rest.
    const heights = await page.evaluate(() =>
      [...document.querySelectorAll<HTMLElement>('.badge-time')]
        .filter(b => b.offsetParent !== null)
        .map(b => b.getBoundingClientRect().height));
    expect(heights.length).toBeGreaterThan(0);
    expect(Math.max(...heights)).toBeLessThanOrEqual(Math.min(...heights) * 1.2);
  });
});
