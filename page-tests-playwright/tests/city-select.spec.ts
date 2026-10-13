import { test, expect, type Page } from '@playwright/test';
import { gotoAndWaitForCards } from './helpers';
import { readFileSync } from 'node:fs';
import { join } from 'node:path';

// The server-rendered level a region heading `name` drills into
// (`_pickerPane.scala.html` — real crawlable links in the page, one level on
// screen at a time once the script runs). Matched by its own `data-region`
// and no subregion, so "Wales" is Wales's level, not its Glamorgan below it.
const regionLevel = (page: Page, name: string) =>
  page.locator(`#picker-panes > .picker-pane:first-child .picker-level[data-region="${name}"]:not([data-subregion])`);

// Every place link the page carries for the country it opened on — the pane
// that came with the page, first in `#picker-panes`; the others follow as they
// arrive.
const placeLinks = (page: Page) => page.locator('#picker-panes > .picker-pane:first-child a[data-slug]');

// The root level's own rows: region headings, and the places a region collapsed onto.
const rootHeadings = (page: Page) => page.locator('#picker-panes > .picker-pane:first-child .picker-level[data-level="root"] .picker-group-item .picker-item-label');
const rootPlaces = (page: Page) => page.locator('#picker-panes > .picker-pane:first-child .picker-level[data-level="root"] a[data-slug] .picker-item-label');

// The dynamic picker's CURRENT LEVEL row whose label is `label` — a region /
// subregion heading (drills in on click) or a city (navigates on click). The
// WHOLE label (a heading's drill-in "›" aside): "München" must not also find
// "Grafing bei München und Umgebung".
const pickerRow = (page: Page, label: string) =>
  page.locator('#picker-list .picker-item', {
    hasText: new RegExp(`^\\s*${label.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}\\s*›?\\s*$`),
  });

const pickerCountryPill = (page: Page, label: string) =>
  page.locator('#picker-countries .picker-country-pill', { hasText: label });

// Multi-city: the bare `/` is the unified country + city picker (every page
// lives under `/{city}/`). It tries browser geolocation first and falls back
// to the picker; the Filtry → Miasto row navigates here from any page.

test.describe('city selection landing (/)', { tag: '@agnostic' }, () => {
  test('serves a crawlable list of every city of the country it serves', async ({ page }) => {
    await page.goto('/', { waitUntil: 'domcontentloaded' });
    // The fixture `/` renders ONE country's pane, exactly as a deployment does
    // (`views.html.landing(Country.default, isApex = false, …)` — see
    // FixtureServerMain), so this is Poland's 180 pages. Real `<a href>`
    // markup even on the levels the picker is not showing —
    // `toHaveCount`/`allTextContents` read the DOM regardless of visibility.
    const panes = page.locator('#picker-panes');
    await expect(placeLinks(page)).toHaveCount(180);
    for (const name of ['Poznań', 'Wrocław', 'Warszawa', 'Kraków', 'Łódź', 'Trójmiasto', 'Częstochowa']) {
      await expect(panes).toContainText(name);
    }
    // Grouped by voivodeship: a major city sits beside the towns around it.
    await expect(rootHeadings(page)).toHaveCount(16);
    await expect(regionLevel(page, 'Wielkopolskie')).toContainText('Turek i okolice');
  });

  test('shows one level at a time and picks a city through it', async ({ page }) => {
    await page.goto('/', { waitUntil: 'domcontentloaded' });
    await expect(page.locator('#picker-dynamic')).toBeVisible();
    await expect(regionLevel(page, 'Wielkopolskie')).toBeHidden();

    await pickerRow(page, 'Wielkopolskie').click();
    await pickerRow(page, 'Poznań').click();
    await page.waitForURL((u) => new URL(u).pathname === '/poznan/');
  });

  test('the search box names the voivodeship by its own term, then just asks for a city', async ({ page }) => {
    await page.goto('/', { waitUntil: 'domcontentloaded' });
    await expect(page.locator('#picker-search')).toHaveAttribute('placeholder', 'Szukaj: województwo lub miasto…');
    await pickerRow(page, 'Wielkopolskie').click();
    await expect(page.locator('#picker-search')).toHaveAttribute('placeholder', 'Szukaj miasta…');
  });
});

// Every country but the one the landing opens on arrives after the page, from
// `/api/picker/:country`, fetched in parallel as soon as the script runs. A
// visitor who switches country before its pane is in sees a spinner where the
// places will be, and the places once it lands — or a short error, retried on
// the next tap, if it never does.
test.describe('another country’s places arrive after the page', { tag: '@agnostic' }, () => {
  test('a country switched to before its pane lands shows a spinner, then its places', async ({ page }) => {
    let release: () => void = () => {};
    const held = new Promise<void>((resolve) => { release = resolve; });
    await page.route('**/api/picker/de*', async (route) => { await held; await route.continue(); });
    await page.goto('/', { waitUntil: 'domcontentloaded' });

    await pickerCountryPill(page, 'Niemcy').click();
    await expect(page.locator('#picker-loading')).toBeVisible();
    await expect(page.locator('#picker-loading .picker-spinner')).toBeVisible();
    await expect(page.locator('#picker-list')).toHaveCount(0);

    release();
    await expect(page.locator('#picker-loading')).toBeHidden();
    await expect(pickerRow(page, 'Bayern')).toBeVisible();
    // Poland came with the page: back to it is instant, no spinner.
    await pickerCountryPill(page, 'Polska').click();
    await expect(pickerRow(page, 'Wielkopolskie')).toBeVisible();
    await expect(page.locator('#picker-loading')).toBeHidden();
  });

  test('a pane that fails says so, and a second tap asks again', async ({ page }) => {
    let failing = true;
    await page.route('**/api/picker/de*', (route) =>
      failing ? route.fulfill({ status: 500, body: '' }) : route.continue());
    await page.goto('/', { waitUntil: 'domcontentloaded' });

    await pickerCountryPill(page, 'Niemcy').click();
    await expect(page.locator('#picker-error')).toBeVisible();
    await expect(page.locator('#picker-error')).toHaveText('Nie udało się wczytać miast — dotknij, aby spróbować ponownie.');
    await expect(page.locator('#picker-loading')).toBeHidden();

    failing = false;
    await page.locator('#picker-error').click();
    await expect(page.locator('#picker-error')).toBeHidden();
    await expect(pickerRow(page, 'Bayern')).toBeVisible();
  });
});

// Each grouped country's pages, read off the roster the page is built from, so a venue
// retired or a town re-cut moves the expected counts with it instead of turning CI red.
type RosterPage = { state?: string; bundesland?: string };
function rosterPages(country: string): RosterPage[] {
  return JSON.parse(readFileSync(join(__dirname, '..', '..', 'data', country, 'pages.json'), 'utf8')).pages;
}
const usPages = rosterPages('us');
const californiaPages = usPages.filter((p) => p.state === 'California').length;
const germanPages = rosterPages('germany').length;
const ukPages = rosterPages('uk').length;

// The US lists ~2,000 places (`usPages`) — major cities, towns and clusters of small towns
// (`data/us/scripts/build_pages.py`) — grouped under their anchor town's state,
// because "Los Angeles" is found under "California" and a 2,000-row A-to-Z is not
// a list anybody reads. The state is a heading, never a link.
//
// The count is a roster fact, so it moves on a re-harvest and on any re-cut.
// `UsRosterSpec` and `CountrySpec` pin the same number model-side; this one pins
// that the page RENDERS all of them, which is the part they cannot see.
test.describe('grouped city landing (the US)', { tag: '@agnostic' }, () => {
  test('the page carries every place, one state’s level on screen at a time', async ({ page }) => {
    await page.goto('/landing-us', { waitUntil: 'domcontentloaded' });
    // Every place is in the document — behind a state heading, except the
    // three states and territories with a single page, which link straight
    // through (`CityGroup.soleCity`): the District of Columbia is Washington.
    await expect(placeLinks(page)).toHaveCount(usPages.length);
    const states = page.locator('#picker-panes > .picker-pane:first-child .picker-level[data-region]');
    await expect(states).toHaveCount(52);
    await expect(rootPlaces(page)).toHaveCount(3);
    await expect(page.locator('#picker-panes > .picker-pane:first-child a[href="https://showtimes.cc/us/washington/"] .picker-item-label')).toHaveText('Washington');
    await expect(states.filter({ visible: true })).toHaveCount(0);
    await expect(states.first()).toHaveAttribute('data-region', 'Alabama');
    await expect(regionLevel(page, 'California').locator('a[data-slug]')).toHaveCount(californiaPages);
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
    await expect(page.locator('#picker-list .picker-item').first()).toHaveText(/Alhambra area/);
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

  test('the search box names the state level at the root, and drops it once inside one', async ({ page }) => {
    await page.goto('/landing-us', { waitUntil: 'domcontentloaded' });
    await expect(page.locator('#picker-search')).toHaveAttribute('placeholder', 'Szukaj: stan lub miasto…');
    await expect(page.locator('#picker-search')).toHaveAttribute('aria-label', 'Szukaj: stan lub miasto');
    await pickerRow(page, 'California').click();
    // One level, no subregion here — once inside a state, only its own
    // pages are left to search for.
    await expect(page.locator('#picker-search')).toHaveAttribute('placeholder', 'Szukaj miasta…');
  });

  test('the search box searches every level at once, not just the one it is showing', async ({ page }) => {
    await page.goto('/landing-us', { waitUntil: 'domcontentloaded' });
    // "los angeles" is a CITY, one level below the root the box is showing —
    // the query still finds it: it scans the whole country, not just the
    // level currently drilled into. `.picker-item-label`, not the row's own
    // text — a region/subregion row also carries a trailing chevron span.
    await page.locator('#picker-search').fill('los angeles');
    await expect(page.locator('#picker-list .picker-item-label')).toHaveText(['Los Angeles']);
    // The heading itself is a root-level term and matches too — alongside the
    // town of California, Maryland, whose page carries the name.
    await page.locator('#picker-search').fill('california');
    await expect(page.locator('#picker-list .picker-item-label')).toHaveText(['California', 'California area']);

    await page.locator('#picker-search').fill('');
    await pickerRow(page, 'California').click();
    await expect(page.locator('#picker-list .picker-item')).toHaveCount(californiaPages);
    // One level down, a DIFFERENT state's own name STILL matches — the whole
    // country is searched no matter how deep the visitor has drilled, same
    // as the root level above.
    await page.locator('#picker-search').fill('texas');
    await expect(page.locator('#picker-list .picker-item-label')).toHaveText(['Texas']);
    await page.locator('#picker-search').fill('los angeles');
    await expect(page.locator('#picker-list .picker-item')).toHaveText(['Los Angeles']);
  });

  test('jumps straight to the right state when a flattened result is picked from inside another one', async ({ page }) => {
    await page.goto('/landing-us', { waitUntil: 'domcontentloaded' });
    await pickerRow(page, 'California').click();
    await expect(pickerRow(page, 'Los Angeles')).toBeVisible();
    // "Texas" only matches at the ROOT level, yet it is offered from inside
    // California — picking it has to leave California behind and land on
    // Texas's own pages, not merely narrow California's list. Exact text,
    // not `pickerRow`'s substring match.
    await page.locator('#picker-search').fill('texas');
    await page.locator('#picker-list .picker-item')
      .filter({ has: page.locator('.picker-item-label', { hasText: /^Texas$/ }) })
      .click();
    await expect(page.locator('#picker-subtitle')).toHaveText('Texas');
    await expect(pickerRow(page, 'Los Angeles')).toHaveCount(0);
  });
});

// The UK lists ~260 town and cluster pages (`ukPages`), so its picker has BOTH levels above
// them: the county, and the nation over that. A county holding one page
// collapses straight back into it (Shetland is Lerwick's row), which is what
// keeps two levels readable — only the ones that really group something cost a
// second tap.
test.describe('two-level city landing (the UK)', { tag: '@agnostic' }, () => {
  test('the page carries every place under its county under its nation', async ({ page }) => {
    await page.goto('/landing-uk', { waitUntil: 'domcontentloaded' });
    await expect(placeLinks(page)).toHaveCount(ukPages);
    await expect(rootHeadings(page)).toHaveText(
      ['England', 'Scotland', 'Northern Ireland', 'Wales', 'Crown Dependencies']);
    await expect(page.locator('#picker-panes > .picker-pane:first-child .picker-level[data-subregion="Glamorgan"] a[data-slug]')).toHaveCount(4);
    await expect(page.locator('#picker-panes > .picker-pane:first-child a[href$="/scotland/"]')).toHaveCount(0);
    await expect(page.locator('#picker-panes > .picker-pane:first-child a[href$="/west-midlands/"]')).toHaveCount(0);
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
    // whether the two names agree (Bristol) or not (Greater London → London).
    await expect(pickerRow(page, 'Bristol')).toBeVisible();
    await expect(pickerRow(page, 'London')).toBeVisible();
    await expect(pickerRow(page, 'Greater London')).toHaveCount(0);
    // West Midlands holds Birmingham, Coventry, Dudley…: a heading.
    await expect(pickerRow(page, 'West Midlands')).toBeVisible();
    await expect(pickerRow(page, 'Birmingham')).toHaveCount(0);

    await page.locator('#picker-back-row').click();
    await pickerRow(page, 'Wales').click();
    await expect(pickerRow(page, 'Cardiff')).toHaveCount(0);
    // "Glamorgan" sits interleaved in its own alphabetical position, between
    // Dyfed and Gwent — not stranded ahead of every direct place in Wales, the
    // reported "West Midlands out of order" bug.
    const walesLabels = await page.locator('#picker-list .picker-item-label').allTextContents();
    expect(walesLabels.indexOf('Dyfed')).toBeLessThan(walesLabels.indexOf('Glamorgan'));
    expect(walesLabels.indexOf('Glamorgan')).toBeLessThan(walesLabels.indexOf('Gwent'));

    await pickerRow(page, 'Glamorgan').click();
    await expect(page.locator('#picker-subtitle')).toHaveText('Wales — Glamorgan');
    // Scoped to the SUBREGION alone (4 places), not the whole region's —
    // a real bug this exact assertion caught upstream in `landing.scala.html`'s
    // `buildPickerRows`, inherited unchanged from the retired popup.
    await expect(page.locator('#picker-list .picker-item')).toHaveText(['Bridgend area', 'Cardiff', 'Merthyr Tydfil area', 'Swansea']);
    // A pick's final navigation isn't exercised here — see the US test above
    // for why the fixture harness can't support it for a non-default country.
  });

  test('the search box narrows one level at a time — nation, then county, then city', async ({ page }) => {
    await page.goto('/landing-uk', { waitUntil: 'domcontentloaded' });
    // Two levels here — the root box names both, deepest last.
    await expect(page.locator('#picker-search')).toHaveAttribute('placeholder', 'Szukaj: kraj, hrabstwo lub miasto…');
    await pickerRow(page, 'Wales').click();
    // The nation is fixed now — only its counties and places are left.
    await expect(page.locator('#picker-search')).toHaveAttribute('placeholder', 'Szukaj: hrabstwo lub miasto…');
    await pickerRow(page, 'Glamorgan').click();
    await expect(page.locator('#picker-search')).toHaveAttribute('placeholder', 'Szukaj miasta…');
  });

  test('back pops one level at a time — county, then nation', async ({ page }) => {
    await page.goto('/landing-uk', { waitUntil: 'domcontentloaded' });
    await pickerRow(page, 'Wales').click();
    await pickerRow(page, 'Glamorgan').click();
    await expect(pickerRow(page, 'Cardiff')).toBeVisible();

    await page.locator('#picker-back-row').click();
    await expect(pickerRow(page, 'Dyfed')).toBeVisible();
    await expect(page.locator('#picker-subtitle')).toHaveText('Wales');

    await page.locator('#picker-back-row').click();
    await expect(page.locator('#picker-back-row')).toBeHidden();
    await expect(page.locator('#picker-list .picker-item-label')).toHaveText(
      ['England', 'Scotland', 'Northern Ireland', 'Wales', 'Crown Dependencies']);
  });

  test('finds a county from the nation root, and lands two levels deep in one tap', async ({ page }) => {
    await page.goto('/landing-uk', { waitUntil: 'domcontentloaded' });
    // "Glamorgan" is a COUNTY two levels below the root the box shows
    // (nation → county → place) — the search scans the whole country
    // regardless of level, so it surfaces from the very first screen.
    await page.locator('#picker-search').fill('glamorgan');
    await expect(page.locator('#picker-list .picker-item-label')).toHaveText(['Glamorgan']);

    // Picking it has to land on the right NATION too, not just the county —
    // a bare subregion name says nothing about which nation it belongs to.
    await pickerRow(page, 'Glamorgan').first().click();
    await expect(page.locator('#picker-subtitle')).toHaveText('Wales — Glamorgan');
    await expect(page.locator('#picker-list .picker-item')).toHaveText(['Bridgend area', 'Cardiff', 'Merthyr Tydfil area', 'Swansea']);
  });

  test('a search finds a place by the county it collapsed onto', async ({ page }) => {
    await page.goto('/landing-uk', { waitUntil: 'domcontentloaded' });
    await pickerRow(page, 'Scotland').click();
    // Shetland holds only Lerwick, so it is Lerwick's row rather than a
    // heading (`CityGroup.soleCity`) — and its name rides on that row as
    // `data-alias`, which the search reads. (While the picker searched the
    // apps' catalog JSON, which carries no such alias, this found nothing.)
    await page.locator('#picker-search').fill('shetland');
    await expect(page.locator('#picker-list .picker-item')).toHaveText(['Lerwick']);
    // The place's OWN name still finds it, same as any other row.
    await page.locator('#picker-search').fill('lerwick');
    await expect(page.locator('#picker-list .picker-item')).toHaveText(['Lerwick']);
  });
});

// Germany lists ~450 town pages (`germanPages`) — a major city's own venues, a town of three or
// more, or a cluster of small towns ("Bayreuth und Umgebung") — under the 16
// Bundesländer, which is what a visitor knows them by. One level, like Poland's
// voivodeships.
test.describe('grouped city landing (Germany)', { tag: '@agnostic' }, () => {
  test('the page carries every page under its Bundesland, collated as German', async ({ page }) => {
    await page.goto('/landing-de', { waitUntil: 'domcontentloaded' });
    await expect(placeLinks(page)).toHaveCount(germanPages);
    // Two of the 16 hold one page each and are pulled up: the city-states
    // Berlin and Hamburg, which share their page's name.
    await expect(rootPlaces(page)).toHaveText(['Berlin', 'Hamburg']);
    await expect(rootHeadings(page)).toHaveCount(14);
    const nrw = regionLevel(page, 'Nordrhein-Westfalen');
    const names = await nrw.locator('a[data-slug] .picker-item-label').allTextContents();
    // Collated, not code-point-ordered: Köln belongs under K-o, and a bare
    // sort files it after Krefeld because 'ö' outranks every letter.
    expect(names.indexOf('Köln')).toBeLessThan(names.indexOf('Krefeld'));
  });

  test('the search box names the Bundesland by its own term, not a bare "region"', async ({ page }) => {
    await page.goto('/landing-de', { waitUntil: 'domcontentloaded' });
    await expect(page.locator('#picker-search')).toHaveAttribute('placeholder', 'Szukaj: kraj związkowy lub miasto…');
    await pickerRow(page, 'Nordrhein-Westfalen').click();
    await expect(page.locator('#picker-search')).toHaveAttribute('placeholder', 'Szukaj miasta…');
  });

  test('a Bundesland holds its major cities beside the town clusters around them', async ({ page }) => {
    await page.goto('/landing-de', { waitUntil: 'domcontentloaded' });
    await pickerRow(page, 'Bayern').click();
    await expect(pickerRow(page, 'München')).toBeVisible();
    await expect(pickerRow(page, 'Bayreuth und Umgebung')).toBeVisible();
  });

  test('the dynamic picker folds umlauts both ways, one level down', async ({ page }) => {
    await page.goto('/landing-de', { waitUntil: 'domcontentloaded' });
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

// A US page is an ordinary city: its `/{slug}/` is its listing, scoped to its own
// venues, with no level below it. (The fixture corpus is Poznań's, so the grid
// itself is empty here; the Scala AreaRoutingSpec asserts the film scoping.)
test.describe('a US page is a city', { tag: '@agnostic' }, () => {
  test('a US page serves its listing straight away, with no chooser in between', async ({ page }) => {
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

// The bare `showtimes.cc` apex (`isApex = true`, `/landing-apex` in the
// fixture harness) skips the AUTOMATIC redirect above — guessing a country
// from coordinates would send a visitor somewhere they cannot opt out of —
// but the manual "use my location" button is opt-in and already searches
// every deployed country (`locatePickerAnywhere`), so it carries no such
// risk and is offered here too, not just on a country's own door.
test.describe('the apex front door offers the manual locate button too', { tag: '@agnostic' }, () => {
  test('the locate button is visible on the apex, not just a country\'s own door', async ({ page }) => {
    await page.goto('/landing-apex', { waitUntil: 'domcontentloaded' });
    await expect(page.locator('#picker-locate-btn')).toBeVisible();
  });

  test('a fix near a supported city hands off through this origin\'s SSO start', async ({ page, context }) => {
    await context.grantPermissions(['geolocation']);
    await context.setGeolocation({ latitude: 52.4064, longitude: 16.9252 }); // Poznań
    await page.goto('/landing-apex', { waitUntil: 'domcontentloaded' });
    // Same hand-off `crossCountryUrl` exercises for any cross-country pick —
    // the apex is never itself a city's own door, so even a hit on the
    // default country's own city routes through SSO start rather than
    // navigating straight there.
    const [request] = await Promise.all([
      page.waitForRequest((r) => r.url().includes('/auth/sso/start')),
      page.locator('#picker-locate-btn').click(),
    ]);
    expect(request.url()).toContain('to=https%3A%2F%2Fkinowo.net');
    expect(request.url()).toContain('city=poznan');
  });

  test('a fix nowhere near any supported city says so and stays on the apex', async ({ page, context }) => {
    await context.grantPermissions(['geolocation']);
    // The middle of the Pacific — nowhere near any city any deployment serves.
    await context.setGeolocation({ latitude: 0, longitude: -160 });
    await page.goto('/landing-apex', { waitUntil: 'domcontentloaded' });
    await page.locator('#picker-locate-btn').click();
    await expect(page.locator('#picker-locate-status')).toHaveText(/./);
    expect(new URL(page.url()).pathname).toBe('/landing-apex');
  });
});

// The Filtry → Miasto row no longer opens an in-page modal — it navigates to
// the unified `/` picker (`landing.scala.html`), the same drill-down this file
// already exercises above on the bare `/`. This block covers what only shows
// up from THAT entry point: the picker opening on THIS deployment's own
// country, the manual "use my location" button, and the cross-country
// hand-off.
test.describe('Filtry → Miasto navigates to the unified picker', { tag: '@agnostic' }, () => {
  test('opens on this deployment\'s own country, at its voivodeships, with no back row', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    await page.locator('#format-filter-btn').click();
    await page.locator('#city-picker-row').click();
    await page.waitForURL((u) => new URL(u).pathname === '/' && new URL(u).search === '?pick=city');
    await expect(page.locator('#picker-back-row')).toBeHidden();
    await expect(pickerRow(page, 'Dolnośląskie')).toBeVisible();
  });

  test('picking a city in the current country navigates straight to its repertoire root', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    await page.locator('#format-filter-btn').click();
    await page.locator('#city-picker-row').click();
    await pickerRow(page, 'Dolnośląskie').click();
    await pickerRow(page, 'Wrocław').click();
    await page.waitForURL((u) => new URL(u).pathname === '/wroclaw/');
  });

  test('switching the country pill switches the search box\'s level term with it', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    await page.locator('#format-filter-btn').click();
    await page.locator('#city-picker-row').click();
    // Poland: one level, the voivodeship.
    await expect(page.locator('#picker-search')).toHaveAttribute('placeholder', 'Szukaj: województwo lub miasto…');
    // Germany: one level, its OWN term — not Poland's, not a bare "region".
    // The pill reads "Niemcy" — localized to the visitor's language via
    // `COUNTRY_NAMES` (`landing.scala.html`), not "Deutschland".
    await pickerCountryPill(page, 'Niemcy').click();
    await expect(page.locator('#picker-search')).toHaveAttribute('placeholder', 'Szukaj: kraj związkowy lub miasto…');
    // Spain: one level since its pages became towns rather than provinces — the
    // province, which a Barcelona-province town is found under.
    await pickerCountryPill(page, 'Hiszpania').click();
    await expect(page.locator('#picker-search')).toHaveAttribute('placeholder', 'Szukaj: prowincja lub miasto…');
    await pickerRow(page, 'Barcelona').click();
    await expect(pickerRow(page, 'Sitges y alrededores')).toBeVisible();
  });

  test('picking a city in another country hands off through this origin\'s SSO start', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    await page.locator('#format-filter-btn').click();
    await page.locator('#city-picker-row').click();
    // Germany is one level deep (Bundesland over its pages, no third
    // level), so one Bundesland tap reaches cities directly. The pill reads
    // "Niemcy" — the fixture always renders in Polish (`TestMessages`'s
    // deployment `Messages`) — not "Deutschland", Germany's own native
    // name: the pill is now localized to the VISITOR's language via
    // `COUNTRY_NAMES` (`landing.scala.html`), not `Country.displayName`.
    await pickerCountryPill(page, 'Niemcy').click();
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
    expect(request.url()).toContain('city=muenchen');
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
