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
test.describe('returning to the listing through history', { tag: '@agnostic' }, () => {

  test('a day survives tapping into a film and pressing Back', async ({ page }) => {
    await gotoAndWaitForCards(page, '/poznan/?date=tomorrow');
    const before = await getVisibleTitles(page);
    expect(before.length, 'fixture has no films tomorrow, so this proves nothing').toBeGreaterThan(0);

    // Away and back, the way a visitor does it.
    await gotoAndWaitForCards(page, '/poznan/');
    await page.goBack({ waitUntil: 'load' });
    await page.waitForFunction(() => document.getElementById('film-grid') !== null);

    await expect(page.locator('#date-filter')).toHaveValue('tomorrow');
    // No filter the visitor never set, and the same films as before.
    await expect(page.locator('#format-imax')).not.toBeChecked();
    await expect(page.locator('#search-input')).toHaveValue('');
    await expect.poll(() => getVisibleTitles(page).then(t => t.length)).toBe(before.length);
    await expect(page.locator('#no-films')).toBeHidden();
  });
});
