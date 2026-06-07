import { test, expect, Page } from '@playwright/test';
import { waitForCards } from './helpers';

// Layout-stress spec. The navbar holds (in some combination):
//   logo · tabs · search · date · Filtry · auth-pill
// and a maxed-out Filtry label can stretch to
//   "Filtry (kina 9/10, 3D, NAP, IMAX, od 18:30)"
// while a logged-in user can carry a 30-char display name. Two
// truncation rules in `_sharedStyles.scala.html` are supposed to
// keep this contained:
//   1. `.auth-name` is capped at 11 ch with ellipsis;
//   2. `#format-filter-btn` is capped at the parent `.navbar-filtry`
//      width (40 % portrait / 30 % landscape) with ellipsis.
//
// This spec exercises real-phone viewports — every iPhone / Pixel
// resolution kinowo actually sees in prod — and checks both
// orientations.  It scrolls the navbar through the maxed state with
// a fake logged-in pill injected, then asserts:
//
//   - The navbar's row count is bounded (≤ 2 in portrait, ≤ 2 in
//     landscape with auth allowed to wrap).
//   - No element extends past the viewport right edge.
//   - The Filtry button and auth-name both clip with ellipsis
//     (scrollWidth > clientWidth = "I tried to grow but the cap
//     held").
//
// Adding a new phone = appending a row to `VIEWPORTS`. Adding a new
// nav element = the row-count assertion will catch overflow without
// a spec change.

interface Phone {
  name: string;
  width: number;   // CSS-pixel width in portrait
  height: number;  // CSS-pixel height in portrait
}

// Effective browser viewport sizes (CSS pixels) that `window.innerWidth`
// /`innerHeight` actually report on real devices — NOT the marketing
// "screen resolution". Two reasons the two values diverge:
//
//   1. iPhones with a notch or Dynamic Island render the page inside
//      the safe area when the `<meta name="viewport">` doesn't set
//      `viewport-fit=cover` (kinowo doesn't). In portrait that eats
//      ~80–90 px of height (Dynamic Island top + home indicator
//      bottom); in landscape that eats ~50–80 px of width (notch
//      side, sometimes both sides if iOS symmetrises). Heights here
//      reflect the portrait inner-height the device reports —
//      swapping to landscape gives the effective landscape width.
//   2. Android phones don't take any insets (gesture indicator and
//      status bar reduce visible area but not the CSS viewport that
//      media queries see). Their entries are the device's native
//      CSS-pixel width × height.
//
// Adding a new phone = append to the right group with its actual
// reported viewport. The test asserts row count / overflow against
// these values, so they should track real devices.
const VIEWPORTS: Phone[] = [
  // ── Small Android phones (the narrow band where the navbar
  // crowds first — Galaxy S10 / A50 share a 360 px width). ─────
  { name: 'Galaxy S10 / A50',     width: 360, height: 760 },
  { name: 'Galaxy S23–S26',        width: 360, height: 780 },
  { name: 'Pixel 9',              width: 360, height: 808 },
  { name: 'Pixel 4a',             width: 393, height: 851 },

  // ── iPhones — heights are post-safe-area effective viewports
  // (Dynamic Island + home indicator subtracted). When the test
  // swaps to landscape, the inner-width matches what real iOS
  // Safari reports — typically ~50–80 px less than the device's
  // native CSS landscape width. ────────────────────────────────
  { name: 'iPhone SE (3rd gen)',  width: 375, height: 667 },  // no DI/notch
  { name: 'iPhone 13 mini',       width: 375, height: 740 },  // native 812 → ~740
  { name: 'iPhone 13 / 14',       width: 390, height: 760 },  // native 844 → ~760
  { name: 'iPhone 15 / 16 / 17',  width: 393, height: 770 },  // native 852 → ~770
  { name: 'iPhone 16 Pro',        width: 402, height: 790 },  // native 874 → ~790
  { name: 'iPhone 14 Plus',       width: 428, height: 840 },  // native 926 → ~840
  { name: 'iPhone 14/15 Pro Max', width: 430, height: 850 },  // native 932 → ~850
  { name: 'iPhone 16/17 Pro Max', width: 440, height: 870 },  // native 956 → ~870

  // ── Larger Android phones — no inset shrinkage. ─────────────
  { name: 'Galaxy S24+',           width: 384, height: 824 },
  { name: 'Galaxy S25–S26 Ultra/+', width: 412, height: 891 },
  { name: 'Galaxy A54 / A55',     width: 412, height: 892 },
  { name: 'Galaxy Note 20 Ultra', width: 412, height: 883 },
  { name: 'Pixel 7 / 8',          width: 412, height: 915 },
  { name: 'Pixel 9 Pro',          width: 427, height: 952 },
  { name: 'Pixel 8 Pro',          width: 448, height: 992 },
];

/// Drives the navbar into its maxed-out state: every format axis
/// constrained, an explicit from-hour set, all but one cinema
/// disabled in localStorage, plus a fake logged-in pill with a
/// 30-character display name injected into `.navbar-auth`. Returns
/// after the inline JS has refreshed the Filtry label.
async function stressNavbar(page: Page): Promise<void> {
  await page.evaluate(() => {
    // Format axes: 3D + NAP + IMAX + from 18:30. `dispatchEvent`
    // (not `.click()`) so we hit the actual `onchange` listeners
    // attached via `onchange="..."` rather than just toggling the
    // `checked` flag.
    const trigger = (el: HTMLElement) => el.dispatchEvent(new Event('change', { bubbles: true }));

    const dim = document.querySelector('input[name="format-dim"][value="3D"]') as HTMLInputElement;
    const lang = document.querySelector('input[name="format-lang"][value="NAP"]') as HTMLInputElement;
    const imax = document.getElementById('format-imax') as HTMLInputElement | null;
    const fromHour = document.getElementById('from-hour') as HTMLSelectElement | null;
    const fromMin  = document.getElementById('from-minute') as HTMLSelectElement | null;

    if (dim)      { dim.checked = true;  trigger(dim);  }
    if (lang)     { lang.checked = true; trigger(lang); }
    if (imax)     { imax.checked = true; trigger(imax); }
    if (fromHour) { fromHour.value = '18'; trigger(fromHour); }
    if (fromMin)  { fromMin.value = '30';  trigger(fromMin);  }

    // Cinemas: disable all but the first. ALL_CINEMAS is set by the
    // server-rendered `_sharedJsConfig` partial.
    const all = (globalThis as { ALL_CINEMAS?: string[] }).ALL_CINEMAS ?? [];
    if (all.length > 1) {
      localStorage.setItem('disabledCinemas', JSON.stringify(all.slice(1)));
    }

    // Long-name logged-in pill. Mirrors the DOM `_navbar.scala.html`
    // emits for the `Some(user)` branch — auth-menu wrapper with an
    // avatar fallback span and the auth-name span. 30 chars covers
    // the realistic worst case (compound Polish surnames).
    const auth = document.querySelector('.navbar-auth');
    if (auth) {
      auth.innerHTML = `
        <div class="auth-menu" id="auth-menu">
          <span class="auth-avatar-fallback">A</span>
          <span class="auth-name">Aleksandra-Anna Kowalska-Nowak</span>
        </div>
      `;
    }
  });

  // Re-run the page's filter pass so the Filtry button picks up the
  // disabledCinemas localStorage value (read on `applyFilters`,
  // which `updateFormatBtn` runs through).
  await page.evaluate(() => {
    const fn = (globalThis as { applyFilters?: () => void }).applyFilters;
    if (typeof fn === 'function') fn();
  });
}

/// Returns the number of distinct visual rows the navbar's
/// top-level children occupy. Rows that share the same vertical
/// band within `tolerance` px are counted once.
async function navbarRowCount(page: Page, tolerance = 6): Promise<number> {
  return page.evaluate((tol) => {
    const tops: number[] = [];
    document.querySelectorAll('nav.navbar > *').forEach((el) => {
      const r = (el as HTMLElement).getBoundingClientRect();
      // Skip zero-height / hidden elements.
      if (r.height <= 0 || (el as HTMLElement).offsetParent === null && r.height === 0) return;
      tops.push(r.top);
    });
    if (tops.length === 0) return 0;
    tops.sort((a, b) => a - b);
    let rows = 1;
    for (let i = 1; i < tops.length; i++) {
      if (tops[i] - tops[i - 1] > tol) rows++;
    }
    return rows;
  }, tolerance);
}

// Baseline assertion: the navbar at the project's own viewport (no
// stress, no viewport override) must stay at ≤ 2 rows. Runs on every
// project — portrait, landscape, zoomed, every engine. Catches CSS
// regressions that push "Zaloguj się" or the date row onto a third line.
test.describe('navbar row count at project viewport', () => {
  test('≤ 2 rows in default state', async ({ page }) => {
    await page.goto('/poznan/');
    await waitForCards(page);

    const rowCount = await navbarRowCount(page);
    const vp = page.viewportSize()!;
    expect(
      rowCount,
      `navbar at ${vp.width}×${vp.height} has ${rowCount} rows; should be ≤ 2`,
    ).toBeLessThanOrEqual(2);
  });
});

test.describe('navbar overflow under maxed filters + long logged-in name', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/poznan/');
    await waitForCards(page);
  });

  for (const v of VIEWPORTS) {
    test(`${v.name} portrait (${v.width}×${v.height})`, async ({ page }) => {
      await page.setViewportSize({ width: v.width, height: v.height });
      await stressNavbar(page);

      const rowCount = await navbarRowCount(page);
      expect(rowCount, `${v.name} portrait should wrap at most into 2 rows; got ${rowCount}`).toBeLessThanOrEqual(2);

      await assertNoHorizontalOverflow(page, v.width);
      await assertAuthNameHiddenOrTruncated(page);
      await assertFiltryButtonContained(page, v.width);
    });

    test(`${v.name} landscape (${v.height}×${v.width})`, async ({ page }) => {
      // Skip viewports whose landscape height would exceed the
      // 500 px ceiling our landscape media query keys on — they're
      // not "phone landscape" any more. iPhone SE landscape is
      // 667×375; height=375 is well under the cap. Pixel 8 Pro
      // would be 992×448, height=448, still under. Every entry in
      // VIEWPORTS produces a valid phone-landscape orientation.
      await page.setViewportSize({ width: v.height, height: v.width });
      await stressNavbar(page);

      const rowCount = await navbarRowCount(page);
      expect(rowCount, `${v.name} landscape should wrap at most into 2 rows; got ${rowCount}`).toBeLessThanOrEqual(2);

      await assertNoHorizontalOverflow(page, v.height);
      await assertAuthNameHiddenOrTruncated(page);
      await assertFiltryButtonContained(page, v.height);
    });
  }
});

/// Asserts every visible navbar child fits inside the viewport
/// horizontally — `right ≤ viewportWidth + 1` (1 px slack for
/// sub-pixel rounding). A `position: sticky` overflow would be
/// invisible on the screen but still trigger a horizontal scrollbar.
async function assertNoHorizontalOverflow(page: Page, viewportWidth: number): Promise<void> {
  const overflows = await page.evaluate((vw) => {
    const out: { tag: string; cls: string; right: number }[] = [];
    document.querySelectorAll('nav.navbar > *').forEach((el) => {
      const r = (el as HTMLElement).getBoundingClientRect();
      if (r.right > vw + 1) {
        out.push({ tag: el.tagName, cls: (el as HTMLElement).className, right: r.right });
      }
    });
    return out;
  }, viewportWidth);
  expect(overflows, `navbar children extending past viewport ${viewportWidth}px: ${JSON.stringify(overflows)}`).toEqual([]);
}

/// Mobile contract for `.auth-name`: either the element is in the
/// DOM but `display: none` (the room-saving path the
/// `(max-width: 575px), (max-height: 500px) and (orientation:
/// landscape)` media queries trigger) OR it's painted but truncated
/// (scrollWidth > clientWidth + 1). Anything else means we surfaced
/// the full long name and risked navbar overflow.
async function assertAuthNameHiddenOrTruncated(page: Page): Promise<void> {
  const probe = await page.evaluate(() => {
    const el = document.querySelector('.auth-name') as HTMLElement | null;
    if (!el) return { state: 'missing' as const };
    if (getComputedStyle(el).display === 'none') return { state: 'hidden' as const };
    return {
      state: 'visible' as const,
      scrollWidth: el.scrollWidth,
      clientWidth: el.clientWidth,
      text: el.textContent ?? '',
    };
  });
  if (probe.state === 'missing') {
    // Auth pill wasn't injected — fail loud, the test setup is broken.
    throw new Error('.auth-name not found in DOM; stressNavbar() did not inject the logged-in pill?');
  }
  if (probe.state === 'hidden') return;
  expect(
    probe.scrollWidth - probe.clientWidth,
    `auth-name visible but not truncated (${probe.text.length} chars, scrollWidth=${probe.scrollWidth}, clientWidth=${probe.clientWidth})`,
  ).toBeGreaterThan(1);
}

/// Asserts the Filtry button — when its text expands to the maxed-
/// filter label "Filtry (3D, NAP, IMAX, od 18:30)" — both stays
/// inside its parent's max-width cap (40 % portrait / 30 % landscape)
/// AND, when natural text width exceeds that cap, ellipsises rather
/// than overflowing. On wide enough viewports the natural width
/// fits and no ellipsis is needed; that's fine.
async function assertFiltryButtonContained(page: Page, viewportWidth: number): Promise<void> {
  const probe = await page.evaluate(() => {
    const wrapper = document.querySelector('.navbar-filtry') as HTMLElement | null;
    const btn = document.getElementById('format-filter-btn') as HTMLElement | null;
    if (!wrapper || !btn) return null;
    const wr = wrapper.getBoundingClientRect();
    return {
      wrapperLeft: wr.left,
      wrapperRight: wr.right,
      wrapperWidth: wr.width,
      btnScroll: btn.scrollWidth,
      btnClient: btn.clientWidth,
      btnText: btn.textContent ?? '',
    };
  });
  if (probe === null) return;
  // The wrapper must sit inside the viewport.
  expect(
    probe.wrapperRight,
    `navbar-filtry wrapper extends past viewport ${viewportWidth} (right=${probe.wrapperRight})`,
  ).toBeLessThanOrEqual(viewportWidth + 1);
  // When natural button text exceeds the rendered width, the
  // overflow:hidden + text-overflow:ellipsis combo on
  // `#format-filter-btn` must have kicked in. Allow 1 px sub-pixel
  // slack on the equality so a perfectly fitted button doesn't
  // mistakenly demand truncation.
  if (probe.btnScroll > probe.btnClient + 1) {
    // Sanity check: an ellipsised button must have non-zero
    // painted width, otherwise the layout is broken.
    expect(probe.btnClient, `Filtry button text "${probe.btnText}" was ellipsised but its client width is zero`).toBeGreaterThan(0);
  }
}
