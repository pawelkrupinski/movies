import { test, expect } from '@playwright/test';

// /kina cinema-pill behaviour. Ports the corresponding tests from
// `PageJsBehaviourSpec` so we get cross-engine coverage (Chromium,
// WebKit, Firefox via Playwright projects) for the same assertions
// the Scala spec already pins headlessly under Chrome.

test.describe('the /kina pill row', () => {

  test('filters to only the pinned cinema section on click', async ({ page }) => {
    await page.goto('/kina');

    const totalSections = await page.locator('.cinema-section').count();
    expect(totalSections).toBeGreaterThan(1);

    // First cinema-section in DOM order — robust to future reorderings
    // of `Cinema.all`. Pinning a known-empty cinema would silently
    // mask a regression with a legitimately empty result.
    const pinTarget = await page.locator('.cinema-section').first().getAttribute('data-cinema');
    expect(pinTarget).toBeTruthy();

    await page.locator(`#cinema-pills .cinema-pill[data-cinema="${pinTarget}"]`).click();

    // After the pill click, exactly one section is visible and it's
    // the pinned one; pill row shows exactly one active pill.
    const visibleSections = await page.evaluate(() =>
      [...document.querySelectorAll('.cinema-section')].filter(
        (s) => (s as HTMLElement).style.display !== 'none'
      ).length
    );
    expect(visibleSections).toBe(1);

    await expect(page.locator('.cinema-section:not([style*="none"])')).toHaveAttribute(
      'data-cinema',
      pinTarget!,
    );
    await expect(page.locator('#cinema-pills .cinema-pill.active')).toHaveCount(1);
  });

  test('restores every cinema section when the active pill is clicked again', async ({ page }) => {
    await page.goto('/kina');

    const baseVisible = await page.evaluate(() =>
      [...document.querySelectorAll('.cinema-section')].filter(
        (s) => (s as HTMLElement).style.display !== 'none'
      ).length
    );
    const pinTarget = await page.locator('.cinema-section').first().getAttribute('data-cinema');

    await page.locator(`#cinema-pills .cinema-pill[data-cinema="${pinTarget}"]`).click();
    await expect(page.locator('#cinema-pills .cinema-pill.active')).toHaveCount(1);

    await page.locator(`#cinema-pills .cinema-pill[data-cinema="${pinTarget}"]`).click();
    await expect(page.locator('#cinema-pills .cinema-pill.active')).toHaveCount(0);
    const afterToggleVisible = await page.evaluate(() =>
      [...document.querySelectorAll('.cinema-section')].filter(
        (s) => (s as HTMLElement).style.display !== 'none'
      ).length
    );
    expect(afterToggleVisible).toBe(baseVisible);
  });

  test('rewrites the URL path to /kina/<cinema> when a pill is pinned', async ({ page }) => {
    await page.goto('/kina');
    const pinTarget = await page.locator('.cinema-section').first().getAttribute('data-cinema');

    await page.locator(`#cinema-pills .cinema-pill[data-cinema="${pinTarget}"]`).click();
    // `history.replaceState` rewrites the address bar. Compare the
    // decoded path to avoid the `encodeURIComponent` (browser, %20)
    // vs `URLEncoder.encode` (Java, `+`) mismatch.
    const decoded = decodeURIComponent(new URL(page.url()).pathname);
    expect(decoded).toBe(`/kina/${pinTarget}`);

    await page.locator(`#cinema-pills .cinema-pill[data-cinema="${pinTarget}"]`).click();
    expect(new URL(page.url()).pathname).toBe('/kina');
  });

  test('does not write the pin into the shared disabledCinemas localStorage', async ({ page }) => {
    await page.goto('/kina');
    // Pre-seed localStorage as if Filtry on `/` had set it. /kina
    // must ignore this on load AND must not overwrite it on a pill
    // click — the persistent filter on / / /ulubione should stay intact.
    const preset = JSON.stringify(['Multikino Stary Browar', 'Helios Posnania']);
    await page.evaluate((v) => localStorage.setItem('disabledCinemas', v), preset);
    await page.reload();

    await expect(page.locator('#cinema-pills .cinema-pill.active')).toHaveCount(0);
    const pinTarget = await page.locator('.cinema-section').first().getAttribute('data-cinema');
    await page.locator(`#cinema-pills .cinema-pill[data-cinema="${pinTarget}"]`).click();
    const after = await page.evaluate(() => localStorage.getItem('disabledCinemas'));
    expect(after).toBe(preset);
  });
});

test.describe('/kina/<cinema>', () => {

  test('seeds _kinaPinned from the URL path on load', async ({ page }) => {
    // Cinema City Kinepolis is reliably present in the fixture corpus
    // (84 showings of Prada alone), so the section + pill exist.
    const target = 'Cinema City Kinepolis';
    await page.goto(`/kina/${encodeURIComponent(target)}`);

    const pinned = await page.evaluate<string | null>(
      `typeof _kinaPinned !== 'undefined' ? _kinaPinned : null`
    );
    expect(pinned).toBe(target);
    await expect(page.locator('#cinema-pills .cinema-pill.active')).toHaveCount(1);
    await expect(page.locator('#cinema-pills .cinema-pill.active')).toHaveAttribute(
      'data-cinema',
      target,
    );
    const visibleSections = await page.evaluate(() =>
      [...document.querySelectorAll('.cinema-section')].filter(
        (s) => (s as HTMLElement).style.display !== 'none'
      ).length
    );
    expect(visibleSections).toBe(1);
  });

  test('forgets the pin across a plain /kina reload', async ({ page }) => {
    await page.goto('/kina');
    const pinTarget = await page.locator('.cinema-section').first().getAttribute('data-cinema');
    await page.locator(`#cinema-pills .cinema-pill[data-cinema="${pinTarget}"]`).click();
    expect(
      await page.evaluate<string | null>(
      `typeof _kinaPinned !== 'undefined' ? _kinaPinned : null`
    )
    ).toBe(pinTarget);

    // Force a navigation back to bare /kina — the server-side
    // controller, like the test fixture server, serves the no-pin
    // variant. The URL pin is the source of truth.
    await page.goto('/kina');
    expect(
      await page.evaluate<string | null>(
        `typeof _kinaPinned !== 'undefined' ? _kinaPinned : null`
      )
    ).toBeNull();
  });
});
