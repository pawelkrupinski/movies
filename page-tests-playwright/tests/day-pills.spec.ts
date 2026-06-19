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

  // The active highlight must fill its whole rounded border box so it reads as a
  // full-size pill rounded on all four sides — not clipped to the content box,
  // which on mobile portrait flattened the top/bottom corners (a short label like
  // "Dziś" looked "cut off" on a real iPhone) and, when an attempt inset it with
  // vertical padding, painted a visibly smaller highlight. `background-clip:
  // border-box` (the default) keeps it full-height + fully rounded; asserting it
  // rejects both the content-box "cut off" and the inset "smaller" variants. On
  // ≤575px portrait this fails against the old content-box rule.
  test('active pill highlight fills its rounded border box (not cut off, not smaller)', async ({ page }) => {
    const clip = await page.locator('.day-pill.active').evaluate(
      el => getComputedStyle(el).backgroundClip || (getComputedStyle(el) as any).webkitBackgroundClip
    );
    expect(clip).toBe('border-box');
  });
});
