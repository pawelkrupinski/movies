import { test, expect } from '@playwright/test';
import { waitForCards } from './helpers';

test.describe('mobile landscape navbar layout', () => {
  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(!testInfo.project.name.includes('landscape'),
      'landscape layout only applies to landscape projects');
    await page.goto('/');
    await waitForCards(page);
  });

  test('logo and tab links are hidden', async ({ page }) => {
    const logoTabs = page.locator('.navbar > .d-flex').first();
    await expect(logoTabs).toBeHidden();
  });

  test('row 1 has search then login, row 2 has date then filtry', async ({ page }) => {
    const boxes = await page.evaluate(() => {
      const q = (sel: string) => {
        const el = document.querySelector(sel);
        if (!el) return null;
        const r = el.getBoundingClientRect();
        return { top: r.top, bottom: r.bottom, left: r.left, right: r.right };
      };
      return {
        search: q('.navbar-search'),
        auth:   q('.navbar-auth'),
        date:   q('.navbar-date'),
        filtry: q('.navbar-filtry'),
      };
    });

    expect(boxes.search).toBeTruthy();
    expect(boxes.auth).toBeTruthy();
    expect(boxes.date).toBeTruthy();
    expect(boxes.filtry).toBeTruthy();

    const { search, auth, date, filtry } = boxes as Record<string, { top: number; bottom: number; left: number; right: number }>;

    // Row 1: search and auth share the same vertical band
    expect(Math.abs(search.top - auth.top)).toBeLessThan(10);
    // Search is left of auth
    expect(search.left).toBeLessThan(auth.left);

    // Row 2: date and filtry share the same vertical band
    expect(Math.abs(date.top - filtry.top)).toBeLessThan(10);
    // Date is left of filtry
    expect(date.left).toBeLessThan(filtry.left);

    // Row 2 is below row 1
    expect(date.top).toBeGreaterThan(search.top);
  });

  test('search input spans most of row width', async ({ page }) => {
    const ratio = await page.evaluate(() => {
      const navbar = document.querySelector('.navbar') as HTMLElement;
      const search = document.querySelector('.navbar-search') as HTMLElement;
      if (!navbar || !search) return 0;
      return search.getBoundingClientRect().width / navbar.getBoundingClientRect().width;
    });
    expect(ratio).toBeGreaterThan(0.4);
  });
});
