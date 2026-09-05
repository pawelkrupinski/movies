import { test, expect, type Page } from '@playwright/test';
import { gotoAndWaitForCards, waitForCards } from './helpers';

// The group whose OWN heading is `name`. `hasText` would not do: it matches any
// group containing the text, and a nested picker's outermost group contains
// every heading under it — asking for "West Midlands" that way returns England.
const groupNamed = (page: Page, name: string) =>
  page.locator(`#city-list details.city-group:has(> summary:text-is("${name}"))`);

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
    await expect(page.locator('#city-list > li.city-direct > a')).toHaveCount(7);
    await expect(page.locator('#city-list > li.city-direct > a[href="/delaware/"]')).toHaveText('Delaware');
    // Every heading SHUT. The grouping only earns its keep closed: rendered open
    // this is the 468-row A-to-Z the states were introduced to break up, with 55
    // headings added to it.
    await expect(page.locator('details.city-group[open]')).toHaveCount(0);
    await expect(groups.first()).toContainText('Alabama');

    const california = groupNamed(page, 'California');
    await expect(california.locator('a')).toHaveCount(22);
    // No state is addressable.
    await expect(page.locator('.city-list a[href="/california/"]')).toHaveCount(0);

    await california.locator('> summary').click();
    await expect(california).toHaveAttribute('open', '');
    // Opening one leaves the rest alone.
    await expect(page.locator('details.city-group[open]')).toHaveCount(1);
    // ALPHABETICAL — a heading you open is a list you scan for a name you already
    // know. Los Angeles is the state's biggest metro and led the roster-ordered
    // list this replaced; it is now in the middle, which is the point.
    await expect(california.locator('a').first()).toHaveText('Bakersfield');

    await page.locator('a[href="/los-angeles/"]').click();
    await page.waitForURL((u) => new URL(u).pathname === '/los-angeles/');
    await expect(page.locator('#view-root')).toHaveCount(1);
  });

  test('a heading closes again on a second click', async ({ page }) => {
    await page.goto('/landing-us', { waitUntil: 'domcontentloaded' });
    const california = groupNamed(page, 'California');
    await california.locator('> summary').click();
    await expect(california.locator('a').first()).toBeVisible();
    await california.locator('> summary').click();
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

// The UK lists 79 places — Flicks regions, which are usually already the COUNTY
// ("Cheshire", "Kent") plus the handful of cities big enough to be a region of
// their own (Birmingham, Glasgow, Liverpool). So its picker gained BOTH levels
// above them: the county, and the nation over that. Most counties are the region
// and collapse straight back into it, which is what keeps two levels readable —
// only the ones that really group something cost a second tap.
test.describe('two-level city landing (the UK)', { tag: '@agnostic' }, () => {
  test('shows nations first, then counties, then places — one level per tap', async ({ page }) => {
    await page.goto('/landing-uk', { waitUntil: 'domcontentloaded' });
    await expect(page.locator('.city-list a')).toHaveCount(79);
    // Nothing but the five nations: not a county, not a place.
    await expect(page.locator('#city-list > li > details.city-group > summary')).toHaveText(
      ['England', 'Scotland', 'Wales', 'Northern Ireland', 'Crown Dependencies']);
    await expect(page.locator('details.city-group[open]')).toHaveCount(0);
    // Neither level is a page.
    await expect(page.locator('.city-list a[href="/scotland/"]')).toHaveCount(0);
    await expect(page.locator('.city-list a[href="/west-midlands/"]')).toHaveCount(0);

    const england = groupNamed(page, 'England');
    await england.locator('> summary').click();
    // A county that IS its one place is a link, not a heading to open.
    await expect(page.locator('#city-list a[href="/cheshire/"]')).toBeVisible();
    // A county that groups something keeps its heading — and stays shut.
    const westMidlands = groupNamed(page, 'West Midlands');
    await expect(westMidlands.locator('> summary')).toBeVisible();
    await expect(page.locator('a[href="/birmingham/"]')).toBeHidden();

    await westMidlands.locator('> summary').click();
    await expect(westMidlands.locator('a')).toHaveText(['Birmingham', 'Dudley', 'Sandwell']);
    await expect(page.locator('details.city-group[open]')).toHaveCount(2);

    await page.locator('a[href="/birmingham/"]').click();
    await page.waitForURL((u) => new URL(u).pathname === '/birmingham/');
  });

  test('a search hit opens every heading above it, not just the nearest', async ({ page }) => {
    await page.goto('/landing-uk', { waitUntil: 'domcontentloaded' });
    // Birmingham is two headings deep; opening only West Midlands would leave it
    // inside a shut England, which is a hit the searcher still cannot see.
    await page.locator('#city-search').fill('birmingham');
    await expect(page.locator('.city-list a:visible')).toHaveText(['Birmingham']);
    await expect(page.locator('#city-list summary:visible')).toHaveText(['England', 'West Midlands']);

    // Every heading above a row is a term the box takes.
    await page.locator('#city-search').fill('west midlands');
    await expect(page.locator('.city-list a:visible')).toHaveText(['Birmingham', 'Dudley', 'Sandwell']);

    await page.locator('#city-search').fill('');
    await expect(page.locator('details.city-group[open]')).toHaveCount(0);
    await expect(page.locator('#city-list > li:visible')).toHaveCount(5);
  });
});

// Germany lists 158 regions — each already a travel-shed of towns around a hub
// ("Köln" also covers Düsseldorf and Bonn) — under the 16 Bundesländer, which is
// what a visitor knows them by. One level, like the US.
test.describe('grouped city landing (Germany)', { tag: '@agnostic' }, () => {
  test('lists regions under their Bundesland, collated as German', async ({ page }) => {
    await page.goto('/landing-de', { waitUntil: 'domcontentloaded' });
    await expect(page.locator('.city-list a')).toHaveCount(158);
    await expect(page.locator('details.city-group[open]')).toHaveCount(0);
    // Two of the 16 are the Land AND its one region, so they link straight
    // through: Berlin and Hamburg. The other 14 are headings.
    await expect(page.locator('#city-list > li.city-direct > a')).toHaveText(['Berlin', 'Hamburg']);
    await expect(page.locator('#city-list > li > details.city-group > summary')).toHaveCount(14);

    const nrw = groupNamed(page, 'Nordrhein-Westfalen');
    await nrw.locator('> summary').click();
    // Collated, not code-point-ordered: Köln belongs under K-o, and a bare sort
    // files it after Krefeld because 'ö' outranks every letter.
    const names = await nrw.locator('a').allTextContents();
    expect(names.indexOf('Köln')).toBeLessThan(names.indexOf('Krefeld'));

    await page.locator('#city-search').fill('bayern');
    await expect(page.locator('.city-list a:visible')).toContainText(['München']);
    await expect(page.locator('#city-list summary:visible')).toHaveText(['Bayern']);
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
