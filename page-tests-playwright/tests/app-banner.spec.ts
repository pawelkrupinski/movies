import { test, expect } from '@playwright/test';
import type { Page } from '@playwright/test';
import { reload } from './helpers';

// The app-promo top banner: nudges EVERY visitor (not just phones, unlike the
// swipe hint) toward the native app once per calendar day, picking the store
// badge that matches the detected OS — both when it can't tell (desktop, or
// an unrecognised UA). The ✕ snoozes it for 24h on top of the daily cap.
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

  test('does not show a second time the same day', async ({ page }) => {
    await page.evaluate(() => localStorage.clear());
    await reload(page);
    await expect(banner(page)).toBeVisible();   // first visit today
    await reload(page);
    await expect(banner(page)).toBeHidden();    // same-day reload → suppressed
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
    await expect(banner(page)).toBeHidden();
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
    expect(states[0]).toMatchObject({ willShow: true, shownToday: false, snoozed: false });
  });

  test('?forceAppBanner=1 bypasses both the daily cap and the dismiss snooze', async ({ page }) => {
    await page.evaluate(() => localStorage.clear());
    await reload(page);
    await expect(banner(page)).toBeVisible();          // first visit today
    await page.locator('.app-banner-close').click();   // dismiss → snoozed 24h
    await expect(banner(page)).toBeHidden();

    await page.goto('/poznan/?date=anytime&forceAppBanner=1', { waitUntil: 'domcontentloaded' });
    await expect(banner(page)).toBeVisible();           // forced past BOTH gates
  });
});
