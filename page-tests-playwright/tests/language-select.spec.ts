import { test, expect } from '@playwright/test';
import { gotoAndWaitForCards } from './helpers';

// The Filtry → Język picker. Language switching is entirely client-side now
// (`i18n.js`'s `applyLanguage`/`onLanguageChange`, fed by the `#i18n-packs`
// pack every page embeds) — the server always renders the deployment's fixed
// default language, so there is no `/lang/:code` round trip left to drive
// through this or any other harness. That's also the whole point: the old
// design's per-visitor `Cache-Control`/ETag branching — the actual
// production bug this replaced (a first request after picking a language
// routinely served/revalidated against the WRONG cached entry) — is gone
// along with the round trip. Server-side regression coverage for that is
// `web/src/test/scala/controllers/LanguagePickSpec.scala`.
test.describe('Filtry → Język picker', { tag: '@agnostic' }, () => {
  // `i18n.js`'s boot sniffs `navigator.languages` as a fallback when no pick
  // is stored — matching what a REAL Polish visitor's browser reports, which
  // is the case these tests pin. Without this the suite's own (English)
  // locale would auto-switch the page before the "deployment default"
  // assertion ever ran.
  test.use({ locale: 'pl-PL' });

  test('offers all four languages, the deployment default selected', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    await expect(page.locator('html')).toHaveAttribute('lang', 'pl');
    await page.locator('#format-filter-btn').click();
    const options = page.locator('#language-select option');
    await expect(options).toHaveText(['Polski', 'English', 'Deutsch', 'Español']);
    await expect(page.locator('#language-select')).toHaveValue('pl');
  });

  test('picking a language swaps the visible copy in place, with no navigation and no request', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    await page.locator('#format-filter-btn').click();

    let sawLangRequest = false;
    page.on('request', (r) => {
      if (new URL(r.url()).pathname.startsWith('/lang/')) sawLangRequest = true;
    });

    await page.selectOption('#language-select', 'de');

    await expect(page.locator('html')).toHaveAttribute('lang', 'de');
    // `nav.clear` — a plain `data-i18n` text node in the same open panel.
    // Scoped to `#format-panel`: the hidden-films modal's "show all" button
    // shares the same key (same Polish/German word), so the bare selector
    // matches two elements.
    await expect(page.locator('#format-panel [data-i18n="nav.clear"]')).toHaveText('Zurücksetzen');
    expect(new URL(page.url()).pathname).toBe('/poznan/');
    expect(sawLangRequest).toBe(false);
  });

  test('a stored pick survives a reload with no second interaction', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    await page.locator('#format-filter-btn').click();
    await page.selectOption('#language-select', 'de');
    await expect(page.locator('html')).toHaveAttribute('lang', 'de');

    await page.reload({ waitUntil: 'domcontentloaded' });
    await expect(page.locator('html')).toHaveAttribute('lang', 'de');
    await page.locator('#format-filter-btn').click();
    await expect(page.locator('#language-select')).toHaveValue('de');
  });
});
