import { test, expect } from '@playwright/test';
import { waitForCards } from './helpers';

// Filmy (/) and Kina (/kina) switch IN PLACE — clicking a tab (or, on phones,
// swiping) slides between the two views without a full page navigation. The
// shell (navbar/modals/shared.js) persists; only `#view-root` is fetched and
// re-rendered. See shared.js `navigateTo`. These tests require the new
// server-rendered structure (`#view-pager`/`#view-root`), so they run against
// the local fixture server / a freshly-built app, not the deployed site.

const isKina  = (page) => page.evaluate(() => document.getElementById('view-root')?.dataset.view);

test.describe('Filmy ↔ Kina slide-swap (click)', () => {
  // Engine-independent behaviour — one DPR-1 desktop project is enough and
  // keeps assertions deterministic (mobile-emulation projects are exercised by
  // the swipe block below).
  test.beforeEach(({}, testInfo) => {
    test.skip(testInfo.project.name !== 'chrome-desktop',
      'in-place swap is engine-independent — chrome-desktop is enough');
  });

  test('clicking Kina swaps in place — no full reload, DOM + tab + URL update', async ({ page }) => {
    await page.goto('/');
    await waitForCards(page);
    // Sentinel: a full navigation wipes window globals; an in-place swap keeps it.
    await page.evaluate(() => { window.__noReload = 'kept'; });

    await page.locator('.navbar .nav-tab', { hasText: 'Kina' }).click();
    await page.waitForURL(/\/kina$/);
    await page.waitForSelector('#cinema-pills .cinema-pill', { state: 'attached' });

    expect(await page.evaluate(() => window.__noReload)).toBe('kept');           // no reload
    expect(await isKina(page)).toBe('kina');                                     // view flipped
    await expect(page.locator('.navbar .nav-tab.active')).toContainText('Kina'); // tab highlighted
    await expect(page.locator('#film-grid .cinema-section')).not.toHaveCount(0); // cinema-grouped grid
    // Filtry's cinema section is stashed out of the DOM on Kina.
    await expect(page.locator('#filtry-cinema-section')).toHaveCount(0);
  });

  test('clicking Filmy swaps back to the film grid', async ({ page }) => {
    await page.goto('/kina');
    await waitForCards(page);
    await page.locator('.navbar .nav-tab', { hasText: 'Filmy' }).click();
    await page.waitForURL((u) => new URL(u).pathname === '/');
    await waitForCards(page);

    expect(await isKina(page)).toBe('films');
    await expect(page.locator('.navbar .nav-tab.active')).toContainText('Filmy');
    await expect(page.locator('#film-grid > .col[data-title]')).not.toHaveCount(0);
    await expect(page.locator('#film-grid .cinema-section')).toHaveCount(0);
    await expect(page.locator('#cinema-pills')).toHaveCount(0);     // pill strip gone with Kina
    await expect(page.locator('#filtry-cinema-section')).toHaveCount(1); // Filtry section restored
  });

  test('no id collisions survive a swap', async ({ page }) => {
    await page.goto('/');
    await waitForCards(page);
    await page.locator('.navbar .nav-tab', { hasText: 'Kina' }).click();
    await page.waitForURL(/\/kina$/);
    await page.waitForSelector('#cinema-pills .cinema-pill', { state: 'attached' });
    // After the outgoing view-root is removed, exactly one of each shared id.
    await expect(page.locator('#view-root')).toHaveCount(1);
    await expect(page.locator('#film-grid')).toHaveCount(1);
    await expect(page.locator('#film-counter')).toHaveCount(1);
    await expect(page.locator('#no-films')).toHaveCount(1);
  });

  test('back button slides back to Filmy and re-highlights it', async ({ page }) => {
    await page.goto('/');
    await waitForCards(page);
    await page.locator('.navbar .nav-tab', { hasText: 'Kina' }).click();
    await page.waitForURL(/\/kina$/);
    await page.waitForSelector('#cinema-pills .cinema-pill', { state: 'attached' });
    await expect(page.locator('#view-pager')).not.toHaveClass(/view-sliding/); // let swap 1 settle

    await page.goBack();
    await page.waitForURL((u) => new URL(u).pathname === '/');
    // popstate kicks off an async fetch-and-swap — wait for it to settle
    // before asserting (the URL flips before the DOM does).
    await expect(page.locator('#view-root')).toHaveAttribute('data-view', 'films');
    await expect(page.locator('.navbar .nav-tab.active')).toContainText('Filmy');
  });

  test('reduced-motion still completes the swap (no transition to wait on)', async ({ page }) => {
    await page.emulateMedia({ reducedMotion: 'reduce' });
    await page.goto('/');
    await waitForCards(page);
    await page.locator('.navbar .nav-tab', { hasText: 'Kina' }).click();
    await page.waitForURL(/\/kina$/);
    await page.waitForSelector('#cinema-pills .cinema-pill', { state: 'attached' });
    expect(await isKina(page)).toBe('kina');
  });
});

test.describe('Filmy ↔ Kina slide-swap (swipe)', () => {
  // Swipe-to-switch is gated to coarse pointers (phones). Auto-skip on any
  // project whose emulated pointer isn't coarse (desktop engines).
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    const coarse = await page.evaluate(() => matchMedia('(pointer: coarse)').matches);
    test.skip(!coarse, 'swipe-to-switch only binds on coarse-pointer devices');
  });

  // Dispatch a horizontal touch-pointer swipe of `dx` px starting on `selector`.
  async function swipe(page, selector, dx) {
    await page.evaluate(({ selector, dx }) => {
      const el = document.querySelector(selector);
      const r = el.getBoundingClientRect();
      const y = r.top + Math.min(40, r.height / 2);
      const x0 = r.left + r.width / 2;
      const ev = (type, x) => el.dispatchEvent(new PointerEvent(type, {
        clientX: x, clientY: y, pointerType: 'touch', pointerId: 1,
        bubbles: true, cancelable: true,
      }));
      ev('pointerdown', x0);
      for (let i = 1; i <= 6; i++) ev('pointermove', x0 + (dx * i) / 6);
      ev('pointerup', x0 + dx);
    }, { selector, dx });
  }

  test('swipe left switches Filmy → Kina and highlights Kina', async ({ page }) => {
    await waitForCards(page);
    await swipe(page, '#film-grid', -200);
    await page.waitForURL(/\/kina$/);
    expect(await isKina(page)).toBe('kina');
    await expect(page.locator('.navbar .nav-tab.active')).toContainText('Kina');
  });

  test('swipe right switches Kina → Filmy and highlights Filmy', async ({ page }) => {
    await page.goto('/kina');
    await waitForCards(page);
    await swipe(page, '#film-grid', 200);
    await page.waitForURL((u) => new URL(u).pathname === '/');
    expect(await isKina(page)).toBe('films');
    await expect(page.locator('.navbar .nav-tab.active')).toContainText('Filmy');
  });

  test('swipe starting on the cinema-pill strip does NOT switch view', async ({ page }) => {
    await page.goto('/kina');
    await waitForCards(page);
    await page.waitForSelector('#cinema-pills .cinema-pill', { state: 'attached' });
    await swipe(page, '#cinema-pills .cinema-pill', -200);
    // Still on Kina — the strip owns the horizontal gesture.
    expect(await isKina(page)).toBe('kina');
    await expect(page).toHaveURL(/\/kina$/);
  });
});
