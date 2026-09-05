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
  test('keeps every state shut until it is opened, then lands on the metro picked', async ({ page }) => {
    await page.goto('/landing-us', { waitUntil: 'domcontentloaded' });
    // All 468 places are in the document — 461 metros behind a state heading,
    // plus the seven states small AND compact enough to be a place in their own
    // right, which link straight through rather than heading a list of one
    // (`CityGroup.soleCity`).
    await expect(page.locator('.city-list a')).toHaveCount(468);
    const groups = page.locator('details.city-group');
    await expect(groups).toHaveCount(48);
    await expect(page.locator('#city-list > li > a')).toHaveCount(7);
    await expect(page.locator('#city-list > li > a[href="/delaware/"]')).toHaveText('Delaware');
    // Every heading SHUT. The grouping only earns its keep closed: rendered open
    // this is the 468-row A-to-Z the states were introduced to break up, with 55
    // headings added to it.
    await expect(page.locator('details.city-group[open]')).toHaveCount(0);
    await expect(groups.first()).toContainText('Alabama');

    const california = page.locator('details.city-group', { hasText: 'California' }).first();
    await expect(california.locator('summary')).toHaveText('California');
    await expect(california.locator('a')).toHaveCount(22);
    // No state is addressable.
    await expect(page.locator('.city-list a[href="/california/"]')).toHaveCount(0);

    await california.locator('summary').click();
    await expect(california).toHaveAttribute('open', '');
    // Opening one leaves the rest alone.
    await expect(page.locator('details.city-group[open]')).toHaveCount(1);
    // Biggest metro first — City.usStates' own order.
    await expect(california.locator('a').first()).toHaveText('Los Angeles');

    await california.locator('a').first().click();
    await page.waitForURL((u) => new URL(u).pathname === '/los-angeles/');
    await expect(page.locator('#view-root')).toHaveCount(1);
  });

  test('a heading closes again on a second click', async ({ page }) => {
    await page.goto('/landing-us', { waitUntil: 'domcontentloaded' });
    const california = page.locator('details.city-group', { hasText: 'California' }).first();
    await california.locator('summary').click();
    await expect(california.locator('a').first()).toBeVisible();
    await california.locator('summary').click();
    await expect(california).not.toHaveAttribute('open', '');
    await expect(california.locator('a').first()).toBeHidden();
  });

  test('the search box opens the state holding a hit and drops the rest', async ({ page }) => {
    await page.goto('/landing-us', { waitUntil: 'domcontentloaded' });
    await page.locator('#city-search').fill('los angeles');
    // A match behind a shut heading is a match the searcher cannot see, so the
    // one state that still has a row OPENS.
    const open = page.locator('details.city-group[open]');
    await expect(open).toHaveCount(1);
    await expect(open).toContainText('California');
    await expect(page.locator('.city-list a:visible')).toHaveText(['Los Angeles']);

    // The heading is a term the box takes too — nobody types "Los Angeles" to
    // find out what California has.
    await page.locator('#city-search').fill('california');
    await expect(page.locator('details.city-group[open]')).toHaveCount(1);
    await expect(page.locator('.city-list a:visible')).toHaveCount(22);

    // Clearing puts the list back the way it was arrived at: every heading
    // present, every one of them shut.
    await page.locator('#city-search').fill('');
    await expect(page.locator('details.city-group[open]')).toHaveCount(0);
    await expect(page.locator('#city-list > li:visible')).toHaveCount(55);
  });
});

// The UK lists 79 places — Flicks regions, which are already the COUNTY
// ("Cheshire", "Kent") plus the handful of cities big enough to be a region of
// their own (London, Birmingham, Glasgow). There is nothing to cut them into,
// so the level the picker gained is the one ABOVE: the four nations, plus the
// Crown Dependencies, which are served from the same market but are not part of
// any of them.
test.describe('grouped city landing (the UK)', { tag: '@agnostic' }, () => {
  test('keeps every nation shut until it is opened, then lands on the county picked', async ({ page }) => {
    await page.goto('/landing-uk', { waitUntil: 'domcontentloaded' });
    await expect(page.locator('.city-list a')).toHaveCount(79);
    const groups = page.locator('details.city-group');
    await expect(groups).toHaveCount(5);
    await expect(page.locator('details.city-group[open]')).toHaveCount(0);
    // By size, not alphabetically — the order a visitor scans.
    await expect(groups.locator('summary')).toHaveText(
      ['England', 'Scotland', 'Wales', 'Northern Ireland', 'Crown Dependencies']);
    // No nation is a page: `/scotland/` is nothing, the way `/california/` is.
    await expect(page.locator('.city-list a[href="/scotland/"]')).toHaveCount(0);
    await expect(page.locator('.city-list a[href="/england/"]')).toHaveCount(0);
    // Every UK place is behind a heading — the UK has no county that is also its
    // own nation, so nothing links straight through here.
    await expect(page.locator('#city-list > li > a')).toHaveCount(0);

    const scotland = page.locator('details.city-group', { hasText: 'Scotland' }).first();
    await scotland.locator('summary').click();
    await expect(scotland.locator('a')).toHaveCount(13);
    await expect(scotland.locator('a', { hasText: 'Glasgow' })).toBeVisible();

    await scotland.locator('a', { hasText: 'Glasgow' }).click();
    await page.waitForURL((u) => new URL(u).pathname === '/glasgow/');
    await expect(page.locator('#view-root')).toHaveCount(1);
  });

  test('the search box reaches a county through its nation, and by its nation', async ({ page }) => {
    await page.goto('/landing-uk', { waitUntil: 'domcontentloaded' });
    await page.locator('#city-search').fill('cheshire');
    await expect(page.locator('details.city-group[open]')).toContainText('England');
    await expect(page.locator('.city-list a:visible')).toHaveText(['Cheshire']);

    // The nation is the term the list taught them, so the box takes it.
    await page.locator('#city-search').fill('wales');
    await expect(page.locator('.city-list a:visible')).toHaveCount(7);

    await page.locator('#city-search').fill('');
    await expect(page.locator('details.city-group[open]')).toHaveCount(0);
    await expect(page.locator('#city-list > li:visible')).toHaveCount(5);
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
