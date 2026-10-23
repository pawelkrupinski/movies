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

});

// Viewport-SENSITIVE, so outside the `@agnostic` set: CI runs it on every phone width, the 250 px
// iPhone SE at 150 % zoom (portrait and landscape) included. There the card column (~85-94 px) is
// narrower than the widest pill ("11:30 AM IMAX 3D DUB", ~119 px on one line; "11:30 IMAX 3D DUB"
// at 24 h is ~106 px), so no font legible at that size fits every pill on one line. What must hold
// at every width: a pill breaks only BETWEEN its time and its format tag, never inside "11:45 PM"
// or inside "IMAX 3D DUB", and a pill whose time and tag fit the column side by side stays one line.
test('12-hour clock (US): a pill breaks only between its time and its format tag', async ({ page }) => {
  await page.goto('/poznan/us-clock', { waitUntil: 'domcontentloaded' });
  await pinDateFilterAnytime(page);
  const pills = await page.evaluate(() => {
    const lineCount = (node: Node) => {
      const range = document.createRange();
      range.selectNodeContents(node);
      return new Set([...range.getClientRects()].map(r => Math.round(r.top))).size;
    };
    const widthOf = (node: Node) => {
      const range = document.createRange();
      range.selectNodeContents(node);
      return range.getBoundingClientRect().width;
    };
    return [...document.querySelectorAll<HTMLElement>('.badge-time')]
      .filter(pill => pill.offsetParent !== null)
      .map(pill => {
        const time = pill.firstChild!;
        const format = pill.querySelector<HTMLElement>('.badge-fmt');
        const style = getComputedStyle(pill);
        const oneLineWidth = widthOf(time) + (format ? format.getBoundingClientRect().width + parseFloat(getComputedStyle(format).marginLeft) : 0)
          + parseFloat(style.paddingLeft) + parseFloat(style.paddingRight);
        return {
          text: pill.textContent,
          height: pill.getBoundingClientRect().height,
          timeLines: lineCount(time),
          formatLines: format ? lineCount(format) : 1,
          fitsOnOneLine: oneLineWidth <= pill.parentElement!.getBoundingClientRect().width - parseFloat(style.marginRight),
        };
      });
  });
  expect(pills.length).toBeGreaterThan(0);
  const oneLine = Math.min(...pills.map(p => p.height));
  for (const pill of pills) {
    expect(pill.timeLines, `time split across lines in "${pill.text}"`).toBe(1);
    expect(pill.formatLines, `format tag split across lines in "${pill.text}"`).toBe(1);
    if (pill.fitsOnOneLine) expect(pill.height, `"${pill.text}" fits its column but wrapped`).toBeLessThanOrEqual(oneLine * 1.2);
  }
});
