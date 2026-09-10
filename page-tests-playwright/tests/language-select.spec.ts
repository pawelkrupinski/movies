import { test, expect } from '@playwright/test';
import { gotoAndWaitForCards } from './helpers';

// The Filtry → Język picker. This fixture server (`FixtureServerMain` /
// `TestHttpServer`) is a deliberately minimal `PartialFunction[String, String]`
// path→body router with no status-code, redirect or cookie support — so the
// full round trip (`/lang/:code` sets PLAY_LANG and redirects; the next
// request's rendering follows it) genuinely cannot be driven through this
// harness without extending shared infrastructure `PageJsBehaviourSpec` also
// depends on, which is out of scope here. That server-side behaviour — the
// cookie gets set, `Accept-Language` is honoured, the cache-eligibility branch
// respects a non-default pick — is covered at the JVM level instead, against
// the REAL `MovieController`/`LanguageController`/`WebLangResolver` (see
// `web/src/test/scala/controllers/LanguagePickSpec.scala`,
// `LanguageControllerSpec.scala`, `WebLangResolverSpec.scala`).
//
// What Playwright DOES uniquely cover, and what these tests pin: the picker
// renders the right options for the right rendered language, and picking one
// drives the browser to the right URL — `onLanguageChange`'s client-side
// contract (`shared.js`).
test.describe('Filtry → Język picker', { tag: '@agnostic' }, () => {
  test('offers all four languages, the deployment default selected', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    await expect(page.locator('html')).toHaveAttribute('lang', 'pl');
    await page.locator('#format-filter-btn').click();
    const options = page.locator('#language-select option');
    await expect(options).toHaveText(['Polski', 'English', 'Deutsch', 'Español']);
    await expect(page.locator('#language-select')).toHaveValue('pl');
  });

  test('picking a language navigates to /lang/:code with the current page as `back`', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    await page.locator('#format-filter-btn').click();
    // The fixture server's bare `sendResponseHeaders(404, -1)` (no body, no
    // Content-Length) isn't a well-formed enough response for Chrome to
    // COMMIT the navigation on — `page.waitForURL` throws
    // `ERR_HTTP_RESPONSE_CODE_FAILURE` no matter which `waitUntil` stage it's
    // told to settle for. The REQUEST still goes out before any of that,
    // though, which is the one thing `onLanguageChange` actually controls —
    // so assert on it directly instead of the (here, unreachable) navigation
    // outcome.
    const [request] = await Promise.all([
      page.waitForRequest((r) => new URL(r.url()).pathname === '/lang/de'),
      page.selectOption('#language-select', 'de'),
    ]);
    const url = new URL(request.url());
    expect(url.searchParams.get('back')).toBe('/poznan/');
  });
});
