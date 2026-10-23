import { test, expect } from '@playwright/test';
import { gotoAndWaitForCards, getVisibleTitles } from './helpers';

// Reported from prod: "/uk/manchester/?date=tomorrow occasionally renders empty
// on iPhone Safari -- and only tomorrow."
//
// The cause is the browser, not the listing. On a history navigation the engine
// replays saved form state onto the re-parsed document, matching controls
// POSITIONALLY -- and the state was saved from a document that had since grown
// the cinema/genre/room checkboxes the Filtry panel builds at runtime. The
// fresh parse has none of them, so those saved "checked" values land on the
// STATIC controls instead. Measured in WebKit against prod: `#format-imax` came
// back checked with no `checked` attribute and no script touching it, and 636
// of tomorrow's 647 showtime badges went display:none, leaving 2 cards.
//
// It reads as "only tomorrow" because `applyFilters` visits badges ONLY inside
// date-groups that pass the day predicate: a phantom badge filter empties the
// day being looked at and leaves every other day's badges alone.
//
// Tapping a film and pressing Back is the commonest gesture this site sees on a
// phone, which is what made an engine quirk look like an intermittent outage.
//
// Every image request is held for the whole test, so the window `load` event
// never fires: the restored state must be on screen from DOMContentLoaded. It
// was not. Chromium writes a control's saved value back only as `load`
// completes, and `load` waits on every eager image (the app-banner icon among
// them), so on a slow network the visitor saw an empty search box over the
// unfiltered grid until the last of them landed. The spec used to wait for
// `goBack({ waitUntil: 'load' })`, which hid that and tied the test to how fast
// a loaded machine served those images instead (it went red once at load ~117).
test.describe('returning to the listing through history', { tag: '@agnostic' }, () => {
  test.beforeEach(async ({ page }) => {
    await page.route('**/*', (route) => {
      if (route.request().resourceType() !== 'image') return route.continue();
      // Never answered: the page closes with the request still in flight.
    });
  });

  test('a day survives tapping into a film and pressing Back', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/?date=tomorrow');
    const before = await getVisibleTitles(page);
    expect(before.length, 'fixture has no films tomorrow, so this proves nothing').toBeGreaterThan(0);

    // Away and back, the way a visitor does it.
    await gotoAndWaitForCards(page, '/poznan/');
    await page.goBack({ waitUntil: 'domcontentloaded' });

    await expect(page.locator('#date-filter')).toHaveValue('tomorrow');
    // No filter the visitor never set, and the same films as before.
    await expect(page.locator('#format-imax')).not.toBeChecked();
    await expect.poll(() => getVisibleTitles(page).then(t => t.length)).toBe(before.length);
    await expect(page.locator('#no-films')).toBeHidden();
  });

  // The other half of the same mechanism. The engine restores a text input
  // correctly even while it is corrupting a checkbox beside it, so clearing the
  // search on Back cost a real behaviour and bought nothing. Only the controls
  // that share a queue with the panel's runtime-built checkboxes are reset.
  test('the search you typed survives the same trip', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    const all = await getVisibleTitles(page);
    const term = all[0].slice(0, 4).toLowerCase();

    await page.fill('#search-input', term);
    await expect.poll(() => getVisibleTitles(page).then(t => t.length)).toBeLessThan(all.length);
    const narrowed = await getVisibleTitles(page);

    await gotoAndWaitForCards(page, '/poznan/?date=anytime');
    await page.goBack({ waitUntil: 'domcontentloaded' });

    await expect(page.locator('#search-input')).toHaveValue(term);
    await expect(page.locator('#format-imax')).not.toBeChecked();
    await expect.poll(() => getVisibleTitles(page).then(t => t.length)).toBe(narrowed.length);
  });

  // The page keeps the search in the history entry's state, and a reload keeps
  // that state too -- but a reload starts clear in every engine, so the page
  // puts the controls back only on a back/forward traversal.
  test('a reload starts with an empty search', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/');
    const all = await getVisibleTitles(page);
    await page.fill('#search-input', all[0].slice(0, 4).toLowerCase());
    await expect.poll(() => getVisibleTitles(page).then(t => t.length)).toBeLessThan(all.length);

    await page.reload({ waitUntil: 'domcontentloaded' });

    await expect(page.locator('#search-input')).toHaveValue('');
    await expect.poll(() => getVisibleTitles(page).then(t => t.length)).toBe(all.length);
  });
});
