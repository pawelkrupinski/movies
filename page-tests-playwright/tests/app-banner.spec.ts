import { test, expect } from '@playwright/test';
import type { Page } from '@playwright/test';
import { reload } from './helpers';

// The app-promo top banner: nudges EVERY visitor (not just phones, unlike the
// swipe hint) toward the native app, picking the store badge that matches the
// detected OS — both when it can't tell (desktop, or an unrecognised UA). The
// interval is device-aware: once per calendar day on a touch/coarse-pointer
// device, once every 10 days on desktop. The ✕ snoozes it for 24h on top of
// the interval cap. It's also suppressed outright when Chrome can confirm
// (Android only — no iOS/Safari equivalent) the app is already installed via
// `navigator.getInstalledRelatedApps()`.
test.describe('app banner', () => {
  const banner      = (page: Page) => page.locator('#app-banner');
  const iosBadge     = (page: Page) => page.locator('#app-banner-ios');
  const androidBadge = (page: Page) => page.locator('#app-banner-android');

  test.beforeEach(async ({ page }) => {
    await page.goto('/poznan/?date=anytime', { waitUntil: 'domcontentloaded' });
  });

  test('shows on a fresh visit, with the Polish headline', async ({ page }) => {
    await page.evaluate(() => localStorage.clear());
    await reload(page);
    await expect(banner(page)).toBeVisible();
    await expect(banner(page)).toContainText('Kinowo — aplikacja mobilna');
  });

  // Regression: `visibility` used to TRANSITION (.25s) on show, like the
  // transform and opacity do. WebKit leaves a new transition at its start value
  // — `visibility: hidden` — until the next rendered frame resolves its start
  // time, so the banner stayed hidden to `toBeVisible()`, screen readers and
  // tab order until a frame came. On a GPU-less CI runner that frame can take
  // longer than the whole 5s expect: the test above failed on
  // `webkit-iphone-se-zoomed` (run 36044091569) with the banner hidden in the
  // ARIA snapshot but fully drawn in the failure screenshot, which forces a
  // frame. Reading the style the instant `.visible` lands — in a
  // MutationObserver callback, a microtask no frame can precede — makes that
  // dependency deterministic instead of load-dependent.
  test('is visible the moment it is shown, without waiting for a rendered frame', async ({ page }) => {
    await page.addInitScript(() => {
      // A transition only starts from a style the element already HAD. On a
      // slow runner the ~600-card document paints a frame mid-parse, so the
      // banner has its hidden style before DOMContentLoaded shows it; on a
      // fast one it doesn't, and there is no transition to wait on. Resolving
      // its style at `interactive` (just before DOMContentLoaded) pins the
      // slow runner's case.
      document.addEventListener('readystatechange', () => {
        const el = document.getElementById('app-banner');
        if (document.readyState === 'interactive' && el) getComputedStyle(el).visibility;
      });
      new MutationObserver((records, observer) => {
        for (const r of records) {
          const el = r.target as HTMLElement;
          if (el.id === 'app-banner' && el.classList.contains('visible')) {
            (window as unknown as { bannerVisibilityOnShow: string }).bannerVisibilityOnShow =
              getComputedStyle(el).visibility;
            observer.disconnect();
          }
        }
      }).observe(document, { subtree: true, attributes: true, attributeFilter: ['class'] });
    });
    await page.evaluate(() => localStorage.clear());
    await reload(page);
    await expect.poll(() => page.evaluate(() =>
      (window as unknown as { bannerVisibilityOnShow?: string }).bannerVisibilityOnShow)).toBe('visible');
  });

  test('does not show a second time the same day', async ({ page }) => {
    await page.evaluate(() => localStorage.clear());
    await reload(page);
    await expect(banner(page)).toBeVisible();   // first visit today
    await reload(page);
    await expect(banner(page)).toBeHidden();    // same-day reload → suppressed
  });

  // Back-dates `kinowoAppBannerDay` by `days` and reloads — simulates time
  // passing without waiting it out or mocking the clock. Parsed as UTC
  // midnight, matching `_daysBetween` in shared.js.
  async function backdateShownDayAndReload(page: Page, days: number): Promise<void> {
    await page.evaluate((d) => {
      const shown = localStorage.getItem('kinowoAppBannerDay');
      if (!shown) return;
      const date = new Date(shown + 'T00:00:00Z');
      date.setUTCDate(date.getUTCDate() - d);
      localStorage.setItem('kinowoAppBannerDay', date.toISOString().slice(0, 10));
    }, days);
    await reload(page);
  }

  test('a touch device sees it again after 1 day; a mouse/trackpad device needs 10', async ({ page }) => {
    await page.evaluate(() => localStorage.clear());
    await reload(page);
    await expect(banner(page)).toBeVisible();   // first visit today
    const isMobile = await page.evaluate(() => matchMedia('(pointer: coarse)').matches);

    await backdateShownDayAndReload(page, 1);
    if (isMobile) {
      await expect(banner(page)).toBeVisible();   // due again after exactly 1 day
    } else {
      await expect(banner(page)).toBeHidden();    // desktop: 1 day is not due yet
      await backdateShownDayAndReload(page, 9);    // 10 days total elapsed
      await expect(banner(page)).toBeVisible();
    }
  });

  test('suppressed when the app is already installed (Android/Chrome only signal)', async ({ page }) => {
    await page.addInitScript(() => {
      (navigator as unknown as { getInstalledRelatedApps: () => Promise<Array<{ platform: string; id: string }>> })
        .getInstalledRelatedApps = async () => [{ platform: 'play', id: 'net.pawel.kinowo' }];
    });
    await page.evaluate(() => localStorage.clear());
    await reload(page);
    await expect(banner(page)).toBeHidden();
  });

  test('shows normally when the installed-apps check reports no match', async ({ page }) => {
    await page.addInitScript(() => {
      (navigator as unknown as { getInstalledRelatedApps: () => Promise<Array<{ platform: string; id: string }>> })
        .getInstalledRelatedApps = async () => [];
    });
    await page.evaluate(() => localStorage.clear());
    await reload(page);
    await expect(banner(page)).toBeVisible();
  });

  test('?forceAppBanner=1 bypasses the installed-app check too', async ({ page }) => {
    await page.addInitScript(() => {
      (navigator as unknown as { getInstalledRelatedApps: () => Promise<Array<{ platform: string; id: string }>> })
        .getInstalledRelatedApps = async () => [{ platform: 'play', id: 'net.pawel.kinowo' }];
    });
    await page.evaluate(() => localStorage.clear());
    await page.goto('/poznan/?date=anytime&forceAppBanner=1', { waitUntil: 'domcontentloaded' });
    await expect(banner(page)).toBeVisible();
  });

  test('picks the store badge matching the detected OS, both when it cannot tell', async ({ page }) => {
    await page.evaluate(() => localStorage.clear());
    await reload(page);
    await expect(banner(page)).toBeVisible();
    const ua = await page.evaluate(() => navigator.userAgent);
    const isIOS     = /iPhone|iPad|iPod/.test(ua);
    const isAndroid = /Android/.test(ua);
    if (isIOS) {
      await expect(iosBadge(page)).toBeVisible();
      await expect(androidBadge(page)).toBeHidden();
    } else if (isAndroid) {
      await expect(androidBadge(page)).toBeVisible();
      await expect(iosBadge(page)).toBeHidden();
    } else {
      await expect(iosBadge(page)).toBeVisible();
      await expect(androidBadge(page)).toBeVisible();
    }
  });

  test('the ✕ hides it and survives a per-day reset (24h snooze)', async ({ page }) => {
    await page.evaluate(() => localStorage.clear());
    await reload(page);
    await expect(banner(page)).toBeVisible();
    await page.locator('.app-banner-close').click();
    // The default 5s expect timeout has flaked here on a contended CI runner
    // (`webkit-iphone-13-zoomed`, 2026-09-14, reran green): the click's synchronous
    // `dismissAppBanner()` handler races main-thread starvation from the other
    // WebKit phone variants sharing the runner — same contention class documented
    // in helpers.ts's `firstVisibleCard`. Widened rather than just re-running.
    await expect(banner(page)).toBeHidden({ timeout: 15_000 });
    // Even after clearing the once-a-day marker, the snooze keeps it away.
    await page.evaluate(() => localStorage.removeItem('kinowoAppBannerDay'));
    await reload(page);
    await expect(banner(page)).toBeHidden();
  });

  test('store badges link to the real App Store / Play Store listings', async ({ page }) => {
    await page.evaluate(() => localStorage.clear());
    await reload(page);
    await expect(iosBadge(page)).toHaveAttribute('href', 'https://apps.apple.com/app/id6792566321');
    await expect(androidBadge(page))
      .toHaveAttribute('href', 'https://play.google.com/store/apps/details?id=net.pawel.kinowo');
  });

  test('logs its gate state to the console on every visit', async ({ page }) => {
    await page.evaluate(() => localStorage.clear());
    // `console.log('[app-banner]', {…})` — the second arg is a JSHandle, not
    // text, so read it back via jsonValue() rather than ConsoleMessage.text().
    const states: Array<Record<string, unknown>> = [];
    page.on('console', (msg) => {
      if (msg.text().startsWith('[app-banner]') && msg.args().length > 1) {
        void msg.args()[1].jsonValue().then((v) => states.push(v as Record<string, unknown>));
      }
    });
    await reload(page);
    await expect(banner(page)).toBeVisible();
    await expect.poll(() => states.length).toBe(1);
    expect(states[0]).toMatchObject({ willShow: true, dueByInterval: true, snoozed: false });
  });

  test('?forceAppBanner=1 bypasses both the daily cap and the dismiss snooze', async ({ page }) => {
    await page.evaluate(() => localStorage.clear());
    await reload(page);
    await expect(banner(page)).toBeVisible();          // first visit today
    await page.locator('.app-banner-close').click();   // dismiss → snoozed 24h
    // See the widened timeout's comment above — same click-then-hidden shape.
    await expect(banner(page)).toBeHidden({ timeout: 15_000 });

    await page.goto('/poznan/?date=anytime&forceAppBanner=1', { waitUntil: 'domcontentloaded' });
    await expect(banner(page)).toBeVisible();           // forced past BOTH gates
  });

  // Regression: `#app-banner` used to sit at z-index 1000, ABOVE the
  // full-viewport `.hidden-modal-backdrop` (z-index 200) — so while the
  // banner was visible, a click meant for the backdrop's top-left corner hit
  // the banner instead and never reached the modal. Broke
  // hidden-modal-ui.spec.ts's "clicking the backdrop dismisses the modal" in
  // CI, since a fresh browser context always shows the banner on first load.
  test('does not intercept clicks meant for a modal backdrop underneath it', async ({ page }) => {
    await page.evaluate(() => localStorage.clear());
    await reload(page);
    await expect(banner(page)).toBeVisible();   // the overlap only exists while it's up

    await page.evaluate(() =>
      (globalThis as { openHiddenModal?: (e?: Event) => void }).openHiddenModal?.(),
    );
    const backdrop = page.locator('#hidden-modal-backdrop');
    await expect(backdrop).toHaveClass(/open/);

    // Same coordinate hidden-modal-ui.spec.ts uses — inside the banner's own
    // box while it's visible, so this only proves the fix if the banner is
    // still up (asserted above) and the click still lands on the backdrop.
    await backdrop.click({ position: { x: 5, y: 5 } });
    await expect(backdrop).not.toHaveClass(/open/);
  });
});
