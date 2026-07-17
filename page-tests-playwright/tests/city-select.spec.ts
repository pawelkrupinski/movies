import { test, expect } from '@playwright/test';
import { waitForCards } from './helpers';

// Multi-city: the bare `/` is a city-selection landing (every page lives
// under `/{city}/`). It tries browser geolocation first and falls back to a
// manual list; the Filtry → Miasto picker switches cities from any page.

test.describe('city selection landing (/)', { tag: '@agnostic' }, () => {
  test('lists every supported city and a pick navigates into that city', async ({ page }) => {
    await page.goto('/');
    // 41 Polish + 79 UK + 3 German cities — the fixture `/` renders `City.all`,
    // the LIVE union across every country (see FixtureServerMain), not one
    // country. Every Flicks region is now live (`activeUkCities = allUkCities`),
    // so the full 79-region UK roster appears.
    const links = page.locator('.city-list a');
    await expect(links).toHaveCount(123);
    await expect(page.locator('.city-list')).toContainText('Poznań');
    await expect(page.locator('.city-list')).toContainText('Wrocław');
    await expect(page.locator('.city-list')).toContainText('Warszawa');
    await expect(page.locator('.city-list')).toContainText('Kraków');
    await expect(page.locator('.city-list')).toContainText('Łódź');
    await expect(page.locator('.city-list')).toContainText('Katowice');
    await expect(page.locator('.city-list')).toContainText('Szczecin');
    await expect(page.locator('.city-list')).toContainText('Trójmiasto');
    await expect(page.locator('.city-list')).toContainText('Białystok');
    await expect(page.locator('.city-list')).toContainText('Bydgoszcz');
    await expect(page.locator('.city-list')).toContainText('Lublin');
    await expect(page.locator('.city-list')).toContainText('Częstochowa');
    await expect(page.locator('.city-list')).toContainText('Radom');
    await expect(page.locator('.city-list')).toContainText('Sosnowiec');
    await expect(page.locator('.city-list')).toContainText('Toruń');
    await expect(page.locator('.city-list')).toContainText('Kielce');
    await expect(page.locator('.city-list')).toContainText('Rzeszów');
    await expect(page.locator('.city-list')).toContainText('Gliwice');
    await expect(page.locator('.city-list')).toContainText('Zabrze');
    // The full UK roster (English labels) is live — the big metros plus every
    // other Flicks region (Cornwall, Kent, Yorkshire, …) now all appear.
    await expect(page.locator('.city-list')).toContainText('London');
    await expect(page.locator('.city-list')).toContainText('Manchester');
    await expect(page.locator('.city-list')).toContainText('Birmingham');
    await expect(page.locator('.city-list')).toContainText('Glasgow');
    await expect(page.locator('.city-list')).toContainText('Liverpool');
    await expect(page.locator('.city-list')).toContainText('West Yorkshire');
    await expect(page.locator('.city-list')).toContainText('Edinburgh');
    await expect(page.locator('.city-list')).toContainText('Cornwall');
    await expect(page.locator('.city-list')).toContainText('Kent');
    await expect(page.locator('.city-list')).toContainText('Yorkshire');

    await page.locator('.city-list a', { hasText: 'Poznań' }).click();
    await page.waitForURL((u) => new URL(u).pathname === '/poznan/');
  });
});

test.describe('geolocation auto-redirect', { tag: '@agnostic' }, () => {
  // A fix inside 100 km of a supported city redirects straight there.
  test.use({ permissions: ['geolocation'], geolocation: { latitude: 52.4064, longitude: 16.9252 } });

  test('a fix near Poznań redirects to /poznan/', async ({ page }) => {
    await page.goto('/');
    await page.waitForURL((u) => new URL(u).pathname === '/poznan/');
  });
});

test.describe('Filtry → Miasto switch', { tag: '@agnostic' }, () => {
  test('selecting another city navigates to its repertoire root', async ({ page }) => {
    await page.goto('/poznan/');
    await waitForCards(page);
    // Open the Filtry panel, then switch the city select to Wrocław.
    await page.locator('#format-filter-btn').click();
    await page.selectOption('#city-select', 'wroclaw');
    await page.waitForURL((u) => new URL(u).pathname === '/wroclaw/');
  });
});
