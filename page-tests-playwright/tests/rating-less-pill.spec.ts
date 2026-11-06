import { test, expect } from '@playwright/test';
import { gotoAndWaitForCards, pinDateFilterAnytime } from './helpers';

// A source we hold a link for but no rating yet (a new release) still gets its pill: the same
// anchor and classes as a rated one, the source's short name for the score, the source's own page as its link, and
// an accessible "no rating yet" label. The fixture corpus carries both kinds (Poznań has Filmweb
// pages without a rating, and IMDb ids without one).
test.describe('rating-less rating pills', () => {
  test.beforeEach(async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    await pinDateFilterAnytime(page);
  });

  // The label follows the page language: the Polish default here, then each other language the picker offers.
  const labels = {
    pl: { Filmweb: 'Filmweb – brak oceny', IMDb: 'IMDb – brak oceny' },
    de: { Filmweb: 'Filmweb – noch keine Bewertung', IMDb: 'IMDb – noch keine Bewertung' },
    en: { Filmweb: 'Filmweb, no rating yet', IMDb: 'IMDb, no rating yet' },
    es: { Filmweb: 'Filmweb – sin valoración todavía', IMDb: 'IMDb – sin valoración todavía' },
  } as const;

  for (const [site, cls, hrefStart] of [
    ['Filmweb', 'rating-fw', 'https://www.filmweb.pl/film/'],
    ['IMDb', 'rating-imdb', 'https://www.imdb.com/title/tt'],
  ] as const) {
    test(`${site}: a rating-less pill is a labelled link to the source page`, async ({ page }) => {
      const pill = page.locator(`a.${cls}[aria-label]`).first();
      await expect(pill).toBeAttached();
      for (const [lang, names] of Object.entries(labels)) {
        await page.evaluate((code) => (window as any).applyLanguage(code), lang);
        expect(await pill.getAttribute('aria-label')).toBe(names[site]);
        expect(await pill.getAttribute('title')).toBe(names[site]);
      }
      await page.evaluate(() => (window as any).applyLanguage('pl'));
      expect(await pill.getAttribute('href')).toContain(hrefStart);
      expect(await pill.getAttribute('target')).toBe('_blank');
      expect(await pill.getAttribute('rel')).toBe('nofollow noopener');
      await expect(pill.locator(`.${cls}-value`)).toHaveText(site === 'IMDb' ? 'IMDb' : 'FW');
    });
  }

  test('a rated pill is unchanged: a score, no label', async ({ page }) => {
    const rated = page.locator('a.rating-imdb:not([aria-label])').first();
    await expect(rated).toBeAttached();
    await expect(rated.locator('.rating-imdb-value')).toHaveText(/^\d\.\d$/);
  });

  test('a rating-less pill is as tall as a rated one: its short name does not wrap, even on a narrow phone', async ({ page }) => {
    const rated = await page.locator('a.rating-imdb:not([aria-label])').first().boundingBox();
    for (const sel of ['a.rating-imdb[aria-label]', 'a.rating-fw[aria-label]']) {
      const pill = await page.locator(sel).first().boundingBox();
      expect(pill!.height).toBeCloseTo(rated!.height, 0);
    }
  });
});
