import { test, expect } from '@playwright/test';

// Multi-city: the bare `/` is a city-selection landing (every page lives
// under `/{city}/`). It tries browser geolocation first and falls back to a
// manual list; the Filtry → Miasto picker switches cities from any page.

test.describe('city selection landing (/)', () => {
  test('lists every supported city and a pick navigates into that city', async ({ page }) => {
    await page.goto('/');
    const links = page.locator('.city-list a');
    await expect(links).toHaveCount(9);
    await expect(page.locator('.city-list')).toContainText('Poznań');
    await expect(page.locator('.city-list')).toContainText('Wrocław');
    await expect(page.locator('.city-list')).toContainText('Warszawa');
    await expect(page.locator('.city-list')).toContainText('Kraków');
    await expect(page.locator('.city-list')).toContainText('Łódź');
    await expect(page.locator('.city-list')).toContainText('Trójmiasto');
    await expect(page.locator('.city-list')).toContainText('Białystok');
    await expect(page.locator('.city-list')).toContainText('Bydgoszcz');
    await expect(page.locator('.city-list')).toContainText('Lublin');

    await page.locator('.city-list a', { hasText: 'Poznań' }).click();
    await page.waitForURL((u) => new URL(u).pathname === '/poznan/');
  });
});

test.describe('geolocation auto-redirect', () => {
  // A fix inside 100 km of a supported city redirects straight there.
  test.use({ permissions: ['geolocation'], geolocation: { latitude: 52.4064, longitude: 16.9252 } });

  test('a fix near Poznań redirects to /poznan/', async ({ page }) => {
    await page.goto('/');
    await page.waitForURL((u) => new URL(u).pathname === '/poznan/');
  });
});

test.describe('Filtry → Miasto switch', () => {
  test('selecting another city navigates to its repertoire root', async ({ page }) => {
    await page.goto('/poznan/');
    await page.waitForSelector('.col[data-title]', { state: 'attached' });
    // Open the Filtry panel, then switch the city select to Wrocław.
    await page.locator('#format-filter-btn').click();
    await page.selectOption('#city-select', 'wroclaw');
    await page.waitForURL((u) => new URL(u).pathname === '/wroclaw/');
  });
});
