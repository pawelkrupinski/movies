import { test, expect, type Page } from '@playwright/test';
import { gotoAndWaitForCards } from './helpers';

// The group whose OWN heading is `name`, inside the STATIC fallback tree
// (`#picker-static` — real crawlable links, hidden once the dynamic picker
// below takes over; see `landing.scala.html`). `hasText` would not do: it
// matches any group containing the text, and a nested picker's outermost
// group contains every heading under it — asking for "West Midlands" that
// way returns England.
const staticGroupNamed = (page: Page, name: string) =>
  page.locator(`#city-list details.city-group:has(> summary:text-is("${name}"))`);

// The dynamic picker's CURRENT LEVEL row whose label is `label` — a region /
// subregion heading (drills in on click) or a city (navigates on click).
const pickerRow = (page: Page, label: string) =>
  page.locator('#picker-list .picker-item', { hasText: label });

const pickerCountryPill = (page: Page, label: string) =>
  page.locator('#picker-countries .picker-country-pill', { hasText: label });

// Multi-city: the bare `/` is the unified country + city picker (every page
// lives under `/{city}/`). It tries browser geolocation first and falls back
// to the picker; the Filtry → Miasto row navigates here from any page.

test.describe('city selection landing (/)', { tag: '@agnostic' }, () => {
  test('serves a crawlable static list of every city of the country it serves', async ({ page }) => {
    await page.goto('/', { waitUntil: 'domcontentloaded' });
    // The fixture `/` renders ONE country's list, exactly as a deployment does
    // (`views.html.landing(Country.default, isApex = false)` — see
    // FixtureServerMain), so this is Poland's 41 cities. Real `<a href>`
    // markup even though the dynamic picker below hides it once JS runs —
    // `toHaveCount`/`allTextContents` read the DOM regardless of visibility.
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
  });

  test('hides the static fallback and picks a city through the dynamic picker', async ({ page }) => {
    await page.goto('/', { waitUntil: 'domcontentloaded' });
    await expect(page.locator('#picker-static')).toBeHidden();
    await expect(page.locator('#picker-dynamic')).toBeVisible();

    await pickerRow(page, 'Poznań').click();
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
  test('the static fallback lists every place, all headings shut', async ({ page }) => {
    await page.goto('/landing-us', { waitUntil: 'domcontentloaded' });
    // All 468 places are in the document — 461 metros behind a state heading,
    // plus the seven states small AND compact enough to be a place in their
    // own right, which link straight through (`CityGroup.soleCity`).
    await expect(page.locator('.city-list a')).toHaveCount(468);
    const groups = page.locator('details.city-group');
    await expect(groups).toHaveCount(48);
    await expect(page.locator('#city-list > li.city-direct > a')).toHaveCount(7);
    await expect(page.locator('#city-list > li.city-direct > a[href="/delaware/"]')).toHaveText('Delaware');
    await expect(page.locator('details.city-group[open]')).toHaveCount(0);
    await expect(groups.first()).toContainText('Alabama');
    const california = staticGroupNamed(page, 'California');
    await expect(california.locator('a')).toHaveCount(22);
    await expect(page.locator('.city-list a[href="/california/"]')).toHaveCount(0);
  });

  test('the dynamic picker opens on the states and drills into one', async ({ page }) => {
    await page.goto('/landing-us', { waitUntil: 'domcontentloaded' });
    await expect(page.locator('#picker-list .picker-item')).toHaveCount(55);
    await expect(page.locator('#picker-back-row')).toBeHidden();

    await pickerRow(page, 'California').click();
    await expect(page.locator('#picker-back-row')).toBeVisible();
    await expect(page.locator('#picker-subtitle')).toHaveText('California');
    // ALPHABETICAL — a heading you open is a list you scan for a name you
    // already know, not the roster order.
    await expect(page.locator('#picker-list .picker-item').first()).toHaveText(/Bakersfield/);
    await expect(pickerRow(page, 'Los Angeles')).toBeVisible();
    // A pick's final navigation is not exercised here: `/landing-us` is the
    // fixture harness's SIMULATED render of the US's own landing, carrying
    // the US's real `pathPrefix` ("/us") baked into `rememberAndGo` — but
    // this flat, single-process test server doesn't actually mount anything
    // under `/us`, so a click would 404 in THIS harness even though the same
    // code is correct on the real US deployment (whose own process really is
    // mounted there). The same-country "navigates straight there" contract
    // is exercised where the harness CAN support it — the flat, unprefixed
    // default (`/` → `/poznan/`, below) and the popup's own retired tests
    // for this exact case, which this replaces.
  });

  test('a back tap returns from the state level to the root', async ({ page }) => {
    await page.goto('/landing-us', { waitUntil: 'domcontentloaded' });
    await pickerRow(page, 'California').click();
    await expect(pickerRow(page, 'Los Angeles')).toBeVisible();
    await page.locator('#picker-back-row').click();
    await expect(page.locator('#picker-back-row')).toBeHidden();
    await expect(page.locator('#picker-list .picker-item')).toHaveCount(55);
  });

  test('the search box narrows the level it is showing, not the whole tree', async ({ page }) => {
    await page.goto('/landing-us', { waitUntil: 'domcontentloaded' });
    // A metro is one level below the root the box is currently searching.
    await page.locator('#picker-search').fill('los angeles');
    await expect(page.locator('#picker-list .picker-item')).toHaveCount(0);
    // The state's own name IS a root-level row. `.picker-item-label`, not
    // the row's own text — a region/subregion row also carries a trailing
    // chevron span.
    await page.locator('#picker-search').fill('california');
    await expect(page.locator('#picker-list .picker-item-label')).toHaveText(['California']);

    await page.locator('#picker-search').fill('');
    await pickerRow(page, 'California').click();
    await expect(page.locator('#picker-list .picker-item')).toHaveCount(22);
    await page.locator('#picker-search').fill('los angeles');
    await expect(page.locator('#picker-list .picker-item')).toHaveText(['Los Angeles']);
  });
});

// The UK lists 79 places — Flicks regions, which are usually already the COUNTY
// ("Cheshire", "Kent") plus the handful of cities big enough to be a region of
// their own (Birmingham, Glasgow, Liverpool). So its picker gained BOTH levels
// above them: the county, and the nation over that. Most counties are the region
// and collapse straight back into it, which is what keeps two levels readable —
// only the ones that really group something cost a second tap.
test.describe('two-level city landing (the UK)', { tag: '@agnostic' }, () => {
  test('the static fallback lists every place under its county under its nation', async ({ page }) => {
    await page.goto('/landing-uk', { waitUntil: 'domcontentloaded' });
    await expect(page.locator('.city-list a')).toHaveCount(79);
    await expect(page.locator('#city-list > li > details.city-group > summary')).toHaveText(
      ['England', 'Scotland', 'Northern Ireland', 'Wales', 'Crown Dependencies']);
    await expect(page.locator('details.city-group[open]')).toHaveCount(0);
    await expect(page.locator('.city-list a[href="/scotland/"]')).toHaveCount(0);
    await expect(page.locator('.city-list a[href="/west-midlands/"]')).toHaveCount(0);
  });

  test('the dynamic picker needs two taps to reach a place — nation, then county', async ({ page }) => {
    await page.goto('/landing-uk', { waitUntil: 'domcontentloaded' });
    // The fixed reading order every client agrees on — `Catalog.json` emits
    // the UK's cities in `Country.cityGroups`' own nation order, and the
    // dynamic picker preserves that encounter order rather than re-sorting
    // it (which used to make this alphabetical and disagree with the static
    // tree and the apps).
    await expect(page.locator('#picker-list .picker-item-label')).toHaveText(
      ['England', 'Scotland', 'Northern Ireland', 'Wales', 'Crown Dependencies']);

    await pickerRow(page, 'England').click();
    // A county holding ONE place is that place's link, pulled up a level —
    // whether the two names agree (Cheshire) or not (Merseyside → Liverpool).
    await expect(pickerRow(page, 'Cheshire')).toBeVisible();
    await expect(pickerRow(page, 'Liverpool')).toBeVisible();
    await expect(pickerRow(page, 'Merseyside')).toHaveCount(0);
    await expect(pickerRow(page, 'Birmingham')).toHaveCount(0);
    // "West Midlands" sits interleaved in its own alphabetical position,
    // beside "West Sussex" — not stranded ahead of every direct city in
    // England, the reported "West Midlands out of order" bug.
    const englandLabels = await page.locator('#picker-list .picker-item-label').allTextContents();
    expect(englandLabels.indexOf('Warwickshire')).toBeLessThan(englandLabels.indexOf('West Midlands'));
    expect(englandLabels.indexOf('West Midlands')).toBeLessThan(englandLabels.indexOf('West Sussex'));

    await pickerRow(page, 'West Midlands').click();
    await expect(page.locator('#picker-subtitle')).toHaveText('England — West Midlands');
    // Scoped to the SUBREGION alone (3 places), not the whole region's 49 —
    // a real bug this exact assertion caught upstream in `landing.scala.html`'s
    // `buildPickerRows`, inherited unchanged from the retired popup.
    await expect(page.locator('#picker-list .picker-item')).toHaveText(['Birmingham', 'Dudley', 'Sandwell']);
    // A pick's final navigation isn't exercised here — see the US test above
    // for why the fixture harness can't support it for a non-default country.
  });

  test('back pops one level at a time — county, then nation', async ({ page }) => {
    await page.goto('/landing-uk', { waitUntil: 'domcontentloaded' });
    await pickerRow(page, 'England').click();
    await pickerRow(page, 'West Midlands').click();
    await expect(pickerRow(page, 'Birmingham')).toBeVisible();

    await page.locator('#picker-back-row').click();
    await expect(pickerRow(page, 'West Midlands')).toBeVisible();
    await expect(page.locator('#picker-subtitle')).toHaveText('England');

    await page.locator('#picker-back-row').click();
    await expect(page.locator('#picker-back-row')).toBeHidden();
    await expect(page.locator('#picker-list .picker-item-label')).toHaveText(
      ['England', 'Scotland', 'Northern Ireland', 'Wales', 'Crown Dependencies']);
  });

  test('a search hit at the county level does NOT find a place by its collapsed county name', async ({ page }) => {
    await page.goto('/landing-uk', { waitUntil: 'domcontentloaded' });
    await pickerRow(page, 'England').click();
    // The static fallback's `data-alias="Merseyside"` (see `_cityPickerGroup`)
    // is what makes Liverpool findable by "merseyside" there — but
    // `KINOWO_CATALOG` (the same JSON the mobile apps read) never carries a
    // collapsed group's name at all (see `Catalog.scala`'s `subregionOf`,
    // built only from sub-groups holding MORE than one place). The dynamic
    // picker searches THAT data, so it shares the apps' own gap here rather
    // than closing it — a pre-existing limitation of the popup this
    // replaces, not a regression.
    await page.locator('#picker-search').fill('merseyside');
    await expect(page.locator('#picker-list .picker-item')).toHaveCount(0);
    // The place's OWN name still finds it, same as any other row.
    await page.locator('#picker-search').fill('liverpool');
    await expect(page.locator('#picker-list .picker-item')).toHaveText(['Liverpool']);
  });
});

// Germany lists 158 regions — each already a travel-shed of towns around a hub
// ("Köln" also covers Düsseldorf and Bonn) — under the 16 Bundesländer, which is
// what a visitor knows them by. One level, like the US.
test.describe('grouped city landing (Germany)', { tag: '@agnostic' }, () => {
  test('the static fallback lists every region under its Bundesland, collated as German', async ({ page }) => {
    await page.goto('/landing-de', { waitUntil: 'domcontentloaded' });
    await expect(page.locator('.city-list a')).toHaveCount(158);
    await expect(page.locator('details.city-group[open]')).toHaveCount(0);
    // Three of the 16 hold one region each and are pulled up: Berlin and
    // Hamburg share their region's name, Saarland collapses onto Saarbrücken.
    await expect(page.locator('#city-list > li.city-direct > a')).toHaveText(
      ['Berlin', 'Hamburg', 'Saarbrücken']);
    await expect(page.locator('#city-list > li > details.city-group > summary')).toHaveCount(13);
    const nrw = staticGroupNamed(page, 'Nordrhein-Westfalen');
    const names = await nrw.locator('a').allTextContents();
    // Collated, not code-point-ordered: Köln belongs under K-o, and a bare
    // sort files it after Krefeld because 'ö' outranks every letter.
    expect(names.indexOf('Köln')).toBeLessThan(names.indexOf('Krefeld'));
  });

  test('the dynamic picker folds umlauts both ways, one level down', async ({ page }) => {
    await page.goto('/landing-de', { waitUntil: 'domcontentloaded' });
    // Unlike the static fallback's `data-alias`, `KINOWO_CATALOG` carries no
    // collapsed-group name at all (see the UK's equivalent test above) — so,
    // unlike the static tree, "saarland" finds nothing here; "saarbruecken"
    // (the city's own name) still does.
    await page.locator('#picker-search').fill('saarland');
    await expect(page.locator('#picker-list .picker-item')).toHaveCount(0);
    await page.locator('#picker-search').fill('saarbruecken');
    await expect(page.locator('#picker-list .picker-item')).toHaveText(['Saarbrücken']);
    await page.locator('#picker-search').fill('');

    await pickerRow(page, 'Nordrhein-Westfalen').click();
    await page.locator('#picker-search').fill('koeln');
    await expect(page.locator('#picker-list .picker-item')).toHaveText(['Köln']);
    await page.locator('#picker-search').fill('koln');
    await expect(page.locator('#picker-list .picker-item')).toHaveText(['Köln']);
    await page.locator('#picker-search').fill('köln');
    await expect(page.locator('#picker-list .picker-item')).toHaveText(['Köln']);
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

// The Filtry → Miasto row no longer opens an in-page modal — it navigates to
// the unified `/` picker (`landing.scala.html`), the same drill-down this file
// already exercises above on the bare `/`. This block covers what only shows
// up from THAT entry point: the picker opening on THIS deployment's own
// country, the manual "use my location" button, and the cross-country
// hand-off.
test.describe('Filtry → Miasto navigates to the unified picker', { tag: '@agnostic' }, () => {
  test('opens on this deployment\'s own country, flat, with no back row', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    await page.locator('#format-filter-btn').click();
    await page.locator('#city-picker-row').click();
    await page.waitForURL((u) => new URL(u).pathname === '/' && new URL(u).search === '?pick=city');
    await expect(page.locator('#picker-back-row')).toBeHidden();
    await expect(pickerRow(page, 'Wrocław')).toBeVisible();
  });

  test('picking a city in the current country navigates straight to its repertoire root', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    await page.locator('#format-filter-btn').click();
    await page.locator('#city-picker-row').click();
    await pickerRow(page, 'Wrocław').click();
    await page.waitForURL((u) => new URL(u).pathname === '/wroclaw/');
  });

  test('picking a city in another country hands off through this origin\'s SSO start', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    await page.locator('#format-filter-btn').click();
    await page.locator('#city-picker-row').click();
    // Germany is one level deep (Bundesland over its regions, no third
    // level), so one region tap reaches cities directly.
    await pickerCountryPill(page, 'Deutschland').click();
    await pickerRow(page, 'Bayern').click();
    // `crossCountryUrl` routes every genuine origin crossing through
    // `/auth/sso/start` FIRST (unconditionally — see `landing.scala.html`'s
    // own comment), so the outgoing REQUEST is what to assert on: the
    // fixture harness has no `AuthController` behind that route to actually
    // land the visitor on `showtimes.cc/de` (a real Play app does).
    const [request] = await Promise.all([
      page.waitForRequest((r) => r.url().includes('/auth/sso/start')),
      pickerRow(page, 'München').click(),
    ]);
    expect(request.url()).toContain('to=https%3A%2F%2Fshowtimes.cc%2Fde');
    expect(request.url()).toContain('pick=city');
  });

  // The picker's own manual locate button — a re-run of the same 100 km
  // check the `/` landing does automatically, but ACROSS EVERY deployed
  // country rather than just this one, mirroring the apps'
  // `resolveNearestCityAnyCountry`.
  test('a fix near Wrocław switches from Poznań to /wroclaw/', async ({ page, context }) => {
    await context.grantPermissions(['geolocation']);
    await context.setGeolocation({ latitude: 51.1079, longitude: 17.0385 });
    await gotoAndWaitForCards(page, '/poznan/');
    await page.locator('#format-filter-btn').click();
    await page.locator('#city-picker-row').click();
    await page.locator('#picker-locate-btn').click();
    await page.waitForURL((u) => new URL(u).pathname === '/wroclaw/');
  });

  test('a fix nowhere near any supported city says so and stays put', async ({ page, context }) => {
    await context.grantPermissions(['geolocation']);
    // The middle of the Pacific — nowhere near any city any deployment serves.
    await context.setGeolocation({ latitude: 0, longitude: -160 });
    await gotoAndWaitForCards(page, '/poznan/');
    await page.locator('#format-filter-btn').click();
    await page.locator('#city-picker-row').click();
    await page.locator('#picker-locate-btn').click();
    await expect(page.locator('#picker-locate-status')).toHaveText(/./);
    // The status text only lands after the async fix and the nearest-city
    // check both ran — by then a hit would already have started navigating.
    expect(new URL(page.url()).pathname).toBe('/');
  });
});
