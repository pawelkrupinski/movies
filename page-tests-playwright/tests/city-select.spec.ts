import { test, expect } from '@playwright/test';
import { gotoAndWaitForCards, waitForCards } from './helpers';

// Multi-city: the bare `/` is a city-selection landing (every page lives
// under `/{city}/`). It tries browser geolocation first and falls back to a
// manual list; the Filtry → Miasto picker switches cities from any page.

test.describe('city selection landing (/)', { tag: '@agnostic' }, () => {
  test('lists every city of the country it serves and a pick navigates into it', async ({ page }) => {
    await page.goto('/', { waitUntil: 'domcontentloaded' });
    // The fixture `/` renders ONE country's list, exactly as a deployment does
    // (`views.html.landing(Country.default)` — see FixtureServerMain), so this is
    // Poland's 41 cities. The grouped shape lives at /landing-us below.
    const links = page.locator('.city-list a');
    await expect(links).toHaveCount(41);
    await expect(page.locator('.city-list')).toContainText('Poznań');
    await expect(page.locator('.city-list')).toContainText('Wrocław');
    await expect(page.locator('.city-list')).toContainText('Warszawa');
    await expect(page.locator('.city-list')).toContainText('Kraków');
    await expect(page.locator('.city-list')).toContainText('Łódź');
    await expect(page.locator('.city-list')).toContainText('Trójmiasto');
    await expect(page.locator('.city-list')).toContainText('Częstochowa');
    // One flat A-to-Z: only a grouped country carries state headings.
    await expect(page.locator('.city-group')).toHaveCount(0);

    await page.locator('.city-list a', { hasText: 'Poznań' }).click();
    await page.waitForURL((u) => new URL(u).pathname === '/poznan/');
  });
});

// The US lists 468 places — 461 distance-clustered METROS plus the seven states
// and territories both small enough and compact enough not to split — grouped
// under their state, because "Los Angeles" is found under "California" and a
// 468-row A-to-Z is not a list anybody reads. The state is a heading, never a
// link: `/california/` is gone.
//
// The count is a roster fact, so it moves on a re-harvest and on any change to
// the cut (`UsRoster.MinCinemasToSplit` / `MaxSpanToStayWholeKm`). `UsRosterSpec`
// and `CountrySpec` pin the same number model-side; this one pins that the page
// RENDERS all of them, which is the part they cannot see.
test.describe('grouped city landing (the US)', { tag: '@agnostic' }, () => {
  test('lists metros under their state, and a pick lands on that metro', async ({ page }) => {
    await page.goto('/landing-us', { waitUntil: 'domcontentloaded' });
    await expect(page.locator('.city-list a')).toHaveCount(468);
    const groups = page.locator('.city-group');
    await expect(groups).toHaveCount(55);
    await expect(groups.first()).toContainText('Alabama');

    const california = page.locator('.city-group', { hasText: 'California' }).first();
    await expect(california.locator('.city-group-label')).toHaveText('California');
    await expect(california.locator('a')).toHaveCount(22);
    // Biggest metro first — City.usStates' own order.
    await expect(california.locator('a').first()).toHaveText('Los Angeles');
    // No state is addressable.
    await expect(page.locator('.city-list a[href="/california/"]')).toHaveCount(0);

    await page.locator('.city-list a', { hasText: 'Los Angeles' }).first().click();
    await page.waitForURL((u) => new URL(u).pathname === '/los-angeles/');
    await expect(page.locator('#view-root')).toHaveCount(1);
  });

  test('the search box narrows to matching metros and drops the empty states', async ({ page }) => {
    await page.goto('/landing-us', { waitUntil: 'domcontentloaded' });
    await page.locator('#city-search').fill('los angeles');
    await expect(page.locator('.city-list a:visible')).toHaveCount(1);
    // The heading of a state with nothing left goes with its rows.
    await expect(page.locator('.city-group:visible')).toHaveCount(1);
    await expect(page.locator('.city-group:visible')).toContainText('California');

    await page.locator('#city-search').fill('');
    await expect(page.locator('.city-group:visible')).toHaveCount(55);
  });
});

// A metro is an ordinary city: its `/{slug}/` is its listing, scoped to its own
// venues, with no level below it. (The fixture corpus is Poznań's, so the grid
// itself is empty here; the Scala AreaRoutingSpec asserts the film scoping.)
test.describe('a metro is a city', { tag: '@agnostic' }, () => {
  test('a metro serves its listing straight away, with no chooser in between', async ({ page }) => {
    await page.goto('/los-angeles/', { waitUntil: 'domcontentloaded' });
    await expect(page.locator('#view-root')).toHaveCount(1);
    await expect(page.locator('.area-list')).toHaveCount(0);
    await expect(page.locator('#change-area')).toHaveCount(0);
  });

  test('London is split too, and keeps its single listing', async ({ page }) => {
    await page.goto('/london/', { waitUntil: 'domcontentloaded' });
    await expect(page.locator('.area-list')).toHaveCount(0);
    await expect(page.locator('#view-root')).toHaveCount(1);
  });
});

test.describe('geolocation auto-redirect', { tag: '@agnostic' }, () => {
  // A fix inside 100 km of a supported city redirects straight there.
  test.use({ permissions: ['geolocation'], geolocation: { latitude: 52.4064, longitude: 16.9252 } });

  test('a fix near Poznań redirects to /poznan/', async ({ page }) => {
    await page.goto('/', { waitUntil: 'domcontentloaded' });
    await page.waitForURL((u) => new URL(u).pathname === '/poznan/');
  });
});

test.describe('Filtry → Miasto switch', { tag: '@agnostic' }, () => {
  test('selecting another city navigates to its repertoire root', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    // Open the Filtry panel, then switch the city select to Wrocław.
    await page.locator('#format-filter-btn').click();
    await page.selectOption('#city-select', 'wroclaw');
    await page.waitForURL((u) => new URL(u).pathname === '/wroclaw/');
  });
});
