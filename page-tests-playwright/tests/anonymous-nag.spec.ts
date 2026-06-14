import { test, expect } from '@playwright/test';

// Anonymous nag toast — `showAnonymousNag` adds `.open` to the
// `#anon-nag-toast` element + queues a fade-out timer;
// `closeAnonymousNag` removes the class on demand. The toast itself
// has an onclick that closes itself + opens the login modal, plus a
// nested ✕ button that just closes.

test.describe('anonymous nag toast', { tag: '@agnostic' }, () => {

  test('showAnonymousNag adds .open; closeAnonymousNag removes it', async ({ page }) => {
    await page.goto('/poznan/');

    // Pre-check the toast exists in the DOM (rendered by
    // `_loginModal.scala.html`).
    await expect(page.locator('#anon-nag-toast')).toBeAttached();

    // At rest the toast is hidden (no `.open`).
    const isOpenBefore = await page.evaluate(
      () => document.getElementById('anon-nag-toast')?.classList.contains('open') ?? null
    );
    expect(isOpenBefore).toBe(false);

    await page.evaluate(() =>
      (globalThis as { showAnonymousNag?: () => void }).showAnonymousNag?.()
    );

    const isOpenAfter = await page.evaluate(
      () => document.getElementById('anon-nag-toast')?.classList.contains('open') ?? null
    );
    expect(isOpenAfter).toBe(true);

    await page.evaluate(() =>
      (globalThis as { closeAnonymousNag?: () => void }).closeAnonymousNag?.()
    );

    const isOpenClosed = await page.evaluate(
      () => document.getElementById('anon-nag-toast')?.classList.contains('open') ?? null
    );
    expect(isOpenClosed).toBe(false);
  });
});
