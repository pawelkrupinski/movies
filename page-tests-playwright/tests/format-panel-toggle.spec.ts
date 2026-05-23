import { test, expect } from '@playwright/test';

// The Filtry button (`#format-filter-btn`) toggles the dropdown
// panel (`#format-panel`) open/closed. The panel is `display: none`
// by default; clicking the button flips it; clicking the document
// outside closes it (delegated handler in shared.js).

test.describe('Filtry dropdown panel', () => {

  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForSelector('#format-filter-btn', { state: 'visible' });
  });

  test('starts hidden, opens on first click, closes on second', async ({ page }) => {
    // The inline style sets display:none at boot.
    const initialDisplay = await page.evaluate(
      () => (document.getElementById('format-panel') as HTMLElement).style.display
    );
    expect(initialDisplay).toBe('none');

    await page.locator('#format-filter-btn').click();
    const afterFirst = await page.evaluate(
      () => (document.getElementById('format-panel') as HTMLElement).style.display
    );
    expect(afterFirst).toBe('block');

    await page.locator('#format-filter-btn').click();
    const afterSecond = await page.evaluate(
      () => (document.getElementById('format-panel') as HTMLElement).style.display
    );
    expect(afterSecond).toBe('none');
  });

  test('an outside click closes the open panel', async ({ page }) => {
    await page.locator('#format-filter-btn').click();
    await expect(page.locator('#format-panel')).toBeVisible();

    // Click the document body well away from the panel. The shared.js
    // global click handler calls `closeOtherPanels(null)` on every
    // outside click.
    await page.evaluate(() => document.body.click());

    const afterOutside = await page.evaluate(
      () => (document.getElementById('format-panel') as HTMLElement).style.display
    );
    expect(afterOutside).toBe('none');
  });

  test('clicking inside the panel does NOT close it', async ({ page }) => {
    await page.locator('#format-filter-btn').click();
    await expect(page.locator('#format-panel')).toBeVisible();

    // Click the panel itself (its onclick `stopPropagation` should
    // keep the document-level outside-click handler from firing).
    await page.locator('#format-panel').click({ position: { x: 5, y: 5 } });

    const stillOpen = await page.evaluate(
      () => (document.getElementById('format-panel') as HTMLElement).style.display
    );
    expect(stillOpen).toBe('block');
  });
});
