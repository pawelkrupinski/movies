import { test, expect } from '@playwright/test';
import { waitForCards } from './helpers';

// The four day presets render as a `.day-pill` row backed by a visually-hidden
// `#date-filter` <select> (the state every filter/carousel helper reads).
// Tapping a pill moves the select + URL and the active highlight follows.
// Runs on every engine — no touch/CDP needed.
test.describe('day pills', { tag: '@agnostic' }, () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/poznan/?date=anytime');
    await waitForCards(page);
  });

  test('exactly one pill is active and it matches the hidden select', async ({ page }) => {
    await expect(page.locator('.day-pill')).toHaveCount(4);
    await expect(page.locator('.day-pill.active')).toHaveCount(1);
    const active = await page.locator('.day-pill.active').getAttribute('data-day');
    const sel = await page.locator('#date-filter').inputValue();
    expect(active).toBe(sel);
    expect(active).toBe('anytime'); // from ?date=anytime
  });

  test('tapping a pill moves the active highlight, the select and the URL', async ({ page }) => {
    await page.locator('.day-pill[data-day="tomorrow"]').click();
    await expect.poll(() => page.locator('#date-filter').inputValue()).toBe('tomorrow');
    await expect(page.locator('.day-pill[data-day="tomorrow"]')).toHaveClass(/\bactive\b/);
    await expect(page.locator('.day-pill.active')).toHaveCount(1);
    await expect
      .poll(() => page.evaluate(() => new URL(location.href).searchParams.get('date')))
      .toBe('tomorrow');
  });

  // On mobile portrait the active pill is a filled highlight clipped to the
  // content box (it hugs its text). Without symmetric vertical padding the
  // highlight fills the FULL pill height while hugging its sides, so on a short
  // label like "Dziś" its flat stadium top/bottom read as "cut off" — reported
  // on a real iPhone 17 Pro Max (440px) where the WebKit proxy's larger navbar
  // gap hid it. The vertical padding must inset the painted highlight from the
  // pill's top/bottom so it reads as a balanced rounded pill at every width.
  test('the active highlight is vertically inset from the pill box (mobile portrait)', async ({ page }) => {
    const vp = page.viewportSize();
    test.skip(!vp || vp.width > 575 || vp.width >= vp.height, 'mobile portrait only');
    await page.goto('/poznan/?date=today');
    await waitForCards(page);
    const m = await page.evaluate(() => {
      const pill = document.querySelector('.day-pill.active') as HTMLElement;
      const cs = getComputedStyle(pill);
      const padT = parseFloat(cs.paddingTop), padB = parseFloat(cs.paddingBottom);
      return { pillH: pill.clientHeight, highlightH: pill.clientHeight - padT - padB, padT, padB, bgClip: cs.backgroundClip };
    });
    // The highlight (content box) must sit clear of the pill's top and bottom.
    expect(m.bgClip).toBe('content-box');
    expect(m.padT).toBeGreaterThanOrEqual(3);
    expect(m.padB).toBeGreaterThanOrEqual(3);
    expect(m.pillH - m.highlightH).toBeGreaterThanOrEqual(6);
  });
});
