import { test, expect } from '@playwright/test';
import { waitForCards } from './helpers';

// Multi-city: the bare `/` is a city-selection landing (every page lives
// under `/{city}/`). It tries browser geolocation first and falls back to a
// manual list; the Filtry → Miasto picker switches cities from any page.

test.describe('city selection landing (/)', { tag: '@agnostic' }, () => {
  test('lists every supported city and a pick navigates into that city', async ({ page }) => {
    await page.goto('/');
    // 41 Polish + 79 UK + 158 German + 55 US cities — the fixture `/` renders
    // `City.all`, the LIVE union across every country (see FixtureServerMain),
    // not one country. Every Flicks region is now live
    // (`activeUkCities = allUkCities`), so the full 79-region UK roster appears;
    // Germany is the full 158-region Filmstarts roster (data/germany); and the US
    // is one region per state/territory (data/us) rather than one per Flicks
    // metro, which is why it adds 55 and not 577.
    const links = page.locator('.city-list a');
    await expect(links).toHaveCount(333);
    await expect(page.locator('.city-list')).toContainText('California');
    await expect(page.locator('.city-list')).toContainText('New York');
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
    // The full German roster (native labels) is live too.
    await expect(page.locator('.city-list')).toContainText('Berlin');
    await expect(page.locator('.city-list')).toContainText('München');
    await expect(page.locator('.city-list')).toContainText('Köln');
    await expect(page.locator('.city-list')).toContainText('Hamburg');

    await page.locator('.city-list a', { hasText: 'Poznań' }).click();
    await page.waitForURL((u) => new URL(u).pathname === '/poznan/');
  });
});

// A US state (`City.hasAreaChooser`) puts a metro PICK SCREEN at `/{city}/` and
// moves its films down to `/{city}/{area}/` — its "city" is a whole state, so
// `/california/` is not a place anyone has chosen yet. Flat states and every
// city outside the US (London included, split though it is) are untouched and
// still land straight on their repertoire.
test.describe('metro chooser (/{city}/ for a split US state)', { tag: '@agnostic' }, () => {
  test('picking California lands on the area chooser, not a listing', async ({ page }) => {
    await page.goto('/');
    await page.locator('.city-list a', { hasText: 'California' }).click();
    await page.waitForURL((u) => new URL(u).pathname === '/california/');

    // The chooser, not the repertoire: area rows, no film grid.
    // California's 486 venues cluster into 21 metros (`UsRoster.metroAreas`).
    const areas = page.locator('.area-list a');
    await expect(areas).toHaveCount(21);
    await expect(page.locator('.area-list')).toContainText('Los Angeles');
    await expect(page.locator('.area-list')).toContainText('San Francisco');
    await expect(page.locator('#film-grid')).toHaveCount(0);
    // Biggest metro first — City.areas' own order.
    await expect(areas.first()).toContainText('Los Angeles');
    await expect(areas.nth(1)).toContainText('San Francisco');
    // Each row carries its venue count. Matched without the noun: the fixture
    // server is a Polish deployment, so it renders "133 kin" where the real US
    // host renders "133 cinemas" (asserted per-language in WebI18nSpec).
    await expect(areas.first()).toContainText(/Los Angeles\s*133\b/);
    // And a way back to the city list.
    await expect(page.locator('a.back')).toHaveAttribute('href', '/');
  });

  test('picking a metro lands on that metro’s repertoire', async ({ page }) => {
    await page.goto('/california/');
    await page.locator('.area-list a', { hasText: 'Los Angeles' }).click();
    await page.waitForURL((u) => new URL(u).pathname === '/california/los-angeles/');
    // The ordinary repertoire view — same shell, scoped cinema universe. (The
    // fixture corpus is Poznań's, so the grid itself is empty here; the Scala
    // AreaRoutingSpec is where the scoping of actual films is asserted.)
    await expect(page.locator('#view-root')).toHaveCount(1);
    await expect(page.locator('.area-list')).toHaveCount(0);
  });

  // The pick is remembered (`area_{city}` cookie), so `/california/` stops
  // asking — which makes this link the ONLY way back to the chooser. It carries
  // `?areas` for exactly that reason: a bare `/california/` would bounce a
  // returning visitor straight back to the metro they are trying to leave.
  test('the metro page links back to the chooser, and the link reaches it', async ({ page }) => {
    await page.goto('/california/los-angeles/');
    const change = page.locator('#change-area');
    await expect(change).toBeVisible();
    await expect(change).toContainText('Los Angeles');
    await expect(change).toHaveAttribute('href', '/california/?areas');

    await change.click();
    await page.waitForURL((u) => new URL(u).pathname === '/california/');
    await expect(page.locator('.area-list a')).toHaveCount(21);
  });

  test('a listing that is not area-scoped shows no change-area link', async ({ page }) => {
    await page.goto('/poznan/');
    await expect(page.locator('#change-area')).toHaveCount(0);
  });

  test('London is split too, but keeps its single listing', async ({ page }) => {
    await page.goto('/london/');
    await expect(page.locator('.area-list')).toHaveCount(0);
    await expect(page.locator('#view-root')).toHaveCount(1);
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
