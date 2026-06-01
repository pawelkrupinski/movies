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
    // capture-phase guard calls `closeOtherPanels(null)` whenever a click
    // lands outside an open dropdown.
    await page.evaluate(() => document.body.click());

    const afterOutside = await page.evaluate(
      () => (document.getElementById('format-panel') as HTMLElement).style.display
    );
    expect(afterOutside).toBe('none');
  });

  test('an outside click only dismisses — it does not navigate', async ({ page }) => {
    await page.locator('#format-filter-btn').click();
    await expect(page.locator('#format-panel')).toBeVisible();

    const startUrl = page.url();

    // Click a real card poster link (outside the panel). Before the fix this
    // both closed the panel AND followed the link to /film. The capture-phase
    // guard in shared.js must now swallow the click: panel closes, URL unchanged.
    //
    // Dispatched through the DOM (like the sibling test's `document.body.click()`)
    // rather than a real viewport click, because: (a) `.col[data-title].first()`
    // is a date-filtered `display:none` card — not clickable — so we must pick
    // the first VISIBLE one; and (b) on phones the open Filtry panel covers most
    // of the grid, so a real click would hit the panel, not a card. A synthetic
    // click still fires the same bubbling event the capture-phase guard sees.
    const clicked = await page.evaluate(() => {
      const col = [...document.querySelectorAll('.col[data-title]')]
        .find((c) => (c as HTMLElement).style.display !== 'none');
      const a = col?.querySelector('.card .poster-wrap > a') as HTMLElement | undefined;
      if (!a) return false;
      a.click();   // follows href to /film unless the guard preventDefaults it
      return true;
    });
    expect(clicked, 'no visible card poster to click').toBe(true);

    // Give any (unwanted) navigation a beat to start.
    await page.waitForTimeout(300);

    expect(page.url()).toBe(startUrl);
    const afterClick = await page.evaluate(
      () => (document.getElementById('format-panel') as HTMLElement).style.display
    );
    expect(afterClick).toBe('none');
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
