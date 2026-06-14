import { test, expect } from '@playwright/test';
import { waitForCards } from './helpers';

// The signed-out "Zaloguj" pill lives in the navbar on DESKTOP only. On
// mobile — both portrait (≤575px) and phone-landscape (height ≤500px) —
// it's hidden; the avatar pill still surfaces in the navbar once a user
// logs in, and the signed-out login entry point moves into Filtry
// (mirroring the iOS/Android apps). The fixture server renders the
// anonymous + `google` provider state, so the button is always in the
// DOM here; only its CSS visibility flips with the viewport.
//
// This re-derives the same predicate the CSS media queries key on, so the
// assertion is correct for every project — portrait, landscape, zoomed,
// and the desktop trio — without hard-coding project names.
function isMobileViewport(vw: number, vh: number): boolean {
  const portraitMobile = vw <= 575;
  const landscapeMobile = vh <= 500 && vw > vh; // CSS orientation:landscape ⇔ vw > vh
  return portraitMobile || landscapeMobile;
}

test.describe('navbar "Zaloguj" visibility', () => {
  test('hidden on mobile, shown on desktop', async ({ page }) => {
    await page.goto('/poznan/');
    await waitForCards(page);

    const login = page.locator('.nav-tab-login');
    // Anonymous + provider-configured render always emits the button.
    await expect(login).toHaveCount(1);

    const viewport = page.viewportSize()!;
    if (isMobileViewport(viewport.width, viewport.height)) {
      await expect(
        login,
        `Zaloguj should be hidden at mobile viewport ${viewport.width}×${viewport.height}`,
      ).toBeHidden();
    } else {
      await expect(
        login,
        `Zaloguj should be visible at desktop viewport ${viewport.width}×${viewport.height}`,
      ).toBeVisible();
    }
  });
});
