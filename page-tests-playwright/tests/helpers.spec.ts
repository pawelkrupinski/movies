import { test, expect } from '@playwright/test';
import { gotoAndWaitForCards, setDateFilter } from './helpers';

// The shared helpers' own contract: one that silently does nothing would let every spec built on
// it assert over whatever state the page happened to be in.
test.describe('setDateFilter', { tag: '@agnostic' }, () => {
  test('applies the day it is given', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    await setDateFilter(page, 'anytime');
    expect(new URL(page.url()).searchParams.get('date')).toBe('anytime');
  });

  test('fails on a page without the date select, rather than doing nothing', async ({ page }) => {
    await page.goto('/', { waitUntil: 'domcontentloaded' });
    await expect(page.locator('#date-filter')).toHaveCount(0);
    await expect(setDateFilter(page, 'anytime')).rejects.toThrow('no #date-filter');
  });

  test('fails on a day the select does not offer', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    await expect(setDateFilter(page, 'someday')).rejects.toThrow('no "someday" option');
  });
});
