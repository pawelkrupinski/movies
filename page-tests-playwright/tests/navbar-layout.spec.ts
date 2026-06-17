import { test, expect, Page } from '@playwright/test';
import { waitForCards, pinDateFilterAnytime, measureGridRatio } from './helpers';

// Consolidated navbar layout spec — covers control-height uniformity,
// orientation stability, compact-landscape constraints, and grid
// density transitions. Replaces the previous four files:
//   - navbar-uniformity.spec.ts
//   - navbar-orientation-uniformity.spec.ts
//   - narrow-landscape.spec.ts (navbar + grid parts)
//   - orientation-flip.spec.ts

// ── Shared measurement helpers ──────────────────────────────────────

// The day picker is now a `.day-pill` row (the `#date-filter` <select> is
// visually-hidden state, not a rendered control). The pills get their own
// group: they share the controls' HEIGHT but ride a slightly smaller font on
// mobile (so all four fit the one-row navbar), so they're checked for height
// uniformity but excluded from the strict font-size equality.
const NAV_CONTROLS = {
  text:  ['.nav-tab', '.nav-tab-login', '.refresh-btn', '.auth-name'],
  input: ['.search-input'],
  pill:  ['.day-pill'],
};

const ALL_CONTROL_SELS = [
  '.nav-tab',
  '.nav-tab-login',
  '.refresh-btn',
  '.day-pill',
  '.search-input',
];

async function measureNavbarControls(page: Page) {
  return page.evaluate((groups) => {
    const navbar = document.querySelector('.navbar');
    if (!navbar) return { byGroup: {} } as const;
    const byGroup: Record<string, { sel: string; fontPx: number; heightPx: number }[]> = {
      text: [], input: [], pill: [],
    };
    const isVisible = (el: HTMLElement) =>
      el.offsetParent !== null && el.getBoundingClientRect().height > 0;
    for (const [group, sels] of Object.entries(groups)) {
      for (const sel of sels) {
        const elements = Array.from(navbar.querySelectorAll(sel)) as HTMLElement[];
        const element = elements.find(isVisible);
        if (!element) continue;
        byGroup[group].push({
          sel,
          fontPx:   parseFloat(getComputedStyle(element).fontSize),
          heightPx: element.getBoundingClientRect().height,
        });
      }
    }
    return { byGroup };
  }, NAV_CONTROLS);
}

async function measureHeights(page: Page): Promise<Record<string, number>> {
  return page.evaluate((sels) => {
    const navbar = document.querySelector('.navbar');
    if (!navbar) return {};
    const out: Record<string, number> = {};
    for (const sel of sels) {
      const candidates = Array.from(navbar.querySelectorAll(sel)) as HTMLElement[];
      const element = candidates.find(
        (e) => e.offsetParent !== null && e.getBoundingClientRect().height > 0,
      );
      if (element) out[sel] = element.getBoundingClientRect().height;
    }
    return out;
  }, ALL_CONTROL_SELS);
}

function expectUniform(
  label: string,
  measurements: { sel?: string; fontPx?: number; heightPx?: number }[],
  field: 'fontPx' | 'heightPx',
  tolerancePx: number,
): void {
  expect(measurements.length, `${label}: expected ≥ 2 measurements`).toBeGreaterThanOrEqual(2);
  const target = measurements[0][field] as number;
  for (const m of measurements) {
    const value = m[field] as number;
    expect(
      Math.abs(value - target),
      `${label} (${(m as { sel?: string }).sel}) — ${value} px vs target ${target} px`,
    ).toBeLessThanOrEqual(tolerancePx);
  }
}

// ── Desktop: font + height uniformity ─────────────────────────────

test.describe('navbar uniformity — desktop', () => {
  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(!testInfo.project.name.includes('desktop'), 'desktop projects only');
    await page.goto('/poznan/');
    await waitForCards(page);
  });

  test('text controls + inputs share a single font-size', async ({ page }) => {
    const { byGroup } = await measureNavbarControls(page);
    const textAndInputs = [...byGroup.text, ...byGroup.input];
    expectUniform('desktop text+input font', textAndInputs, 'fontPx', 0.5);
  });

  test('all interactive controls share a single height', async ({ page }) => {
    const { byGroup } = await measureNavbarControls(page);
    const everything = [...byGroup.text, ...byGroup.input, ...byGroup.pill];
    expectUniform('desktop control height', everything, 'heightPx', 1);
  });
});

// ── Mobile portrait: font + height uniformity ─────────────────────

test.describe('navbar uniformity — mobile portrait', () => {
  test.beforeEach(async ({ page }, testInfo) => {
    const name = testInfo.project.name;
    const isPortraitMobile = !name.includes('desktop') && !name.includes('landscape');
    test.skip(!isPortraitMobile, 'mobile portrait only');
    await page.goto('/poznan/');
    await waitForCards(page);
  });

  test('text controls + inputs share a single font-size', async ({ page }) => {
    const { byGroup } = await measureNavbarControls(page);
    // The day pills are buttons, so (unlike the old <select>) they're free of
    // the Linux-WebKit 16px form-control font floor and ride the navbar's
    // scaled font like every other text control.
    const textAndInputs = [...byGroup.text, ...byGroup.input];
    // Below ~290px (zoomed phones) the navbar drops the search input, and the
    // signed-out "Zaloguj" button is hidden on mobile — leaving only the
    // icon-only Filtry button as a navbar-font control. With a single control
    // there's nothing to compare, so skip there (the same emergency-layout
    // carve-out the avatar-pill test uses for the hidden search box).
    test.skip(
      textAndInputs.length < 2,
      'only one navbar-font control at this width (search + login both hidden)',
    );
    expectUniform('mobile-portrait text+input font', textAndInputs, 'fontPx', 0.5);
  });

  test('all interactive controls share a single height', async ({ page }) => {
    const { byGroup } = await measureNavbarControls(page);
    const everything = [...byGroup.text, ...byGroup.input, ...byGroup.pill];
    expectUniform('mobile-portrait control height', everything, 'heightPx', 1);
  });
});

// ── Mobile portrait: search is a bottom-floating frosted pill ─────
//
// On portrait phones the search box is NOT an inline navbar control — it's a
// translucent, blurred, capsule "pill" pinned to the bottom of the viewport,
// floating over the scrolling grid, mirroring the iOS/Android apps. This locks
// the design in: position, placement, capsule radius, translucent + blurred
// background, and the magnifier glyph that only surfaces inside the pill.

test.describe('mobile portrait — floating search pill', () => {
  test.beforeEach(async ({ page }, testInfo) => {
    const name = testInfo.project.name;
    const isPortraitMobile = !name.includes('desktop') && !name.includes('landscape');
    test.skip(!isPortraitMobile, 'mobile portrait only');
    await page.goto('/poznan/');
    await waitForCards(page);
  });

  test('the search box floats as a near-transparent, barely-blurred capsule low at the bottom', async ({ page }) => {
    const measurements = await page.evaluate(() => {
      const navbar = document.querySelector('.navbar')!.getBoundingClientRect();
      const pill   = document.querySelector('.navbar-search') as HTMLElement | null;
      const icon   = document.querySelector('.navbar-search .search-icon') as HTMLElement | null;
      const input  = document.querySelector('.navbar-search .search-input') as HTMLElement | null;
      if (!pill || !input) return null;
      const pillStyle = getComputedStyle(pill);
      const inputStyle = getComputedStyle(input);
      const pillRect = pill.getBoundingClientRect();
      const iconRect = icon ? icon.getBoundingClientRect() : null;
      return {
        position:        pillStyle.position,
        radius:          parseFloat(pillStyle.borderTopLeftRadius),
        background:      pillStyle.backgroundColor,
        backdrop:        pillStyle.backdropFilter || (pillStyle as unknown as Record<string, string>).webkitBackdropFilter || '',
        inputBackground: inputStyle.backgroundColor,
        navBottom:       navbar.bottom,
        pillTop:         pillRect.top,
        pillBottom:      pillRect.bottom,
        viewportHeight:  window.innerHeight,
        iconWidth:       iconRect ? iconRect.width : -1,
      };
    });
    expect(measurements, 'floating search pill not rendered').not.toBeNull();
    // Floats out of the navbar flow…
    expect(measurements!.position, 'search pill must be position:fixed').toBe('fixed');
    // …pinned LOW at the bottom of the viewport, below the navbar — hugging the
    // bottom edge (only a few px of gap below it).
    expect(measurements!.pillTop, 'pill should sit below the navbar').toBeGreaterThan(measurements!.navBottom);
    expect(measurements!.pillTop, 'pill should be in the lower half of the viewport').toBeGreaterThan(measurements!.viewportHeight / 2);
    expect(measurements!.pillBottom, 'pill should not run off the bottom of the viewport').toBeLessThanOrEqual(measurements!.viewportHeight + 1);
    expect(
      measurements!.viewportHeight - measurements!.pillBottom,
      `pill should hug the bottom edge — gap below it is ${(measurements!.viewportHeight - measurements!.pillBottom).toFixed(1)}px`,
    ).toBeLessThanOrEqual(10);
    // Capsule shape.
    expect(measurements!.radius, `pill border-radius ${measurements!.radius}px should be a capsule (≥20px)`).toBeGreaterThanOrEqual(20);
    // Near-transparent glass: the pill's background alpha is very low (the grid
    // shows straight through) and the blur is just a whisper — not the heavy
    // frosted look. The input itself is fully transparent (the pill IS the
    // background).
    const alpha = (color: string) => {
      const rgbaMatch = color.match(/rgba?\(([^)]+)\)/);
      if (!rgbaMatch) return 1;
      const parts = rgbaMatch[1].split(',').map((value) => parseFloat(value));
      return parts.length >= 4 ? parts[3] : 1;
    };
    const blurPx = parseFloat((measurements!.backdrop.match(/blur\(([\d.]+)px\)/) || [])[1] ?? 'NaN');
    expect(alpha(measurements!.background), `pill background ${measurements!.background} should be almost transparent (alpha < 0.3)`).toBeLessThan(0.3);
    expect(blurPx, `pill backdrop blur "${measurements!.backdrop}" should be a whisper (≤ 6px)`).toBeLessThanOrEqual(6);
    expect(blurPx, `pill should still carry a hint of blur, got "${measurements!.backdrop}"`).toBeGreaterThan(0);
    expect(alpha(measurements!.inputBackground), `input background ${measurements!.inputBackground} should be transparent`).toBe(0);
    // The magnifier glyph surfaces inside the pill.
    expect(measurements!.iconWidth, 'search magnifier icon should be visible inside the pill').toBeGreaterThan(0);
  });
});

// ── Mobile portrait: day-pill highlight hugs its text ─────────────
//
// The active day-pill's highlight is clipped to its content box (inset by the
// pill's horizontal padding), NOT filling the whole flex cell to its edge.
// Without this the active END pill — "Dziś" on the left, "Wszystkie" on the
// right — slams its highlight up against the logo / Filtry while the opposite
// (inactive) end shows only inset text, so the gap reads lopsided. Hugging the
// text makes the highlight sit where the text does, so the row has the same gap
// on both sides whichever day is selected.

test.describe('mobile portrait — active day-pill highlight hugs its text', () => {
  test.beforeEach(async ({ page }, testInfo) => {
    const name = testInfo.project.name;
    const isPortraitMobile = !name.includes('desktop') && !name.includes('landscape');
    test.skip(!isPortraitMobile, 'mobile portrait only');
    await page.goto('/poznan/');
    await waitForCards(page);
  });

  test('the active highlight is content-clipped, so end pills do not jam the logo or Filtry', async ({ page }) => {
    // Activate the FIRST pill (Dziś) and measure its highlight's gap to the logo.
    await page.locator('.day-pill[data-day="today"]').click();
    const left = await page.evaluate(() => {
      const activePill = document.querySelector('.day-pill.active') as HTMLElement;
      const pillStyle = getComputedStyle(activePill);
      const logo = document.querySelector('.navbar-logo')!.getBoundingClientRect();
      const pillRect = activePill.getBoundingClientRect();
      // background-clip:content-box paints the highlight inside the padding box.
      return { clip: pillStyle.backgroundClip, label: activePill.textContent,
               highlightGap: (pillRect.left + parseFloat(pillStyle.paddingLeft)) - logo.right };
    });
    // Activate the LAST pill (Wszystkie) and measure its highlight's gap to Filtry.
    await page.locator('.day-pill[data-day="anytime"]').click();
    const right = await page.evaluate(() => {
      const activePill = document.querySelector('.day-pill.active') as HTMLElement;
      const pillStyle = getComputedStyle(activePill);
      const filtry = document.querySelector('.navbar-filtry')!.getBoundingClientRect();
      const pillRect = activePill.getBoundingClientRect();
      return { label: activePill.textContent,
               highlightGap: filtry.left - (pillRect.right - parseFloat(pillStyle.paddingRight)) };
    });
    // The mechanism: the active pill's background is clipped to its content box.
    expect(left.clip, 'active day-pill highlight must be clipped to its content box').toBe('content-box');
    // Neither end pill's highlight is jammed against its neighbour (the old
    // cell-filling highlight sat ~3px from the edge; content-clipped is more)…
    expect(left.highlightGap, `"${left.label}" highlight gap to logo is ${left.highlightGap.toFixed(1)}px`).toBeGreaterThanOrEqual(5);
    expect(right.highlightGap, `"${right.label}" highlight gap to Filtry is ${right.highlightGap.toFixed(1)}px`).toBeGreaterThanOrEqual(5);
    // …and the two end gaps match (the row reads the same on both sides).
    expect(
      Math.abs(left.highlightGap - right.highlightGap),
      `left gap ${left.highlightGap.toFixed(1)}px vs right gap ${right.highlightGap.toFixed(1)}px — the day-pill row is lopsided`,
    ).toBeLessThanOrEqual(6);
  });
});

// ── Logged-in avatar pill: matches navbar control height ──────────
//
// The live/fixture server renders the navbar logged-OUT (a "Zaloguj"
// button), so the `.auth-menu` avatar pill never appears in the other
// uniformity specs above — they list `.auth-name`, which is
// `display:none` on mobile anyway. The pill's height is pure CSS, so
// we inject the real logged-in markup and measure it. Regression for
// the pill keeping `min-height:35px` on mobile while every other
// control was pinned to 28px (the mobile blocks pinned `.auth-name`,
// the hidden inner span, instead of the `.auth-menu` container).

// IMPORTANT: inject the real `<img class="auth-avatar">` carrying a LARGE
// intrinsic image (like a Google/OAuth photo), NOT the text fallback span. The
// image is the regression surface: as a flex item its default `min-*:auto`
// resolves to the photo's intrinsic size and balloons the 22px avatar unless
// the min-size is pinned — that's the "logged-in icon too tall / misaligned"
// bug on iOS. An earlier version of this test used the fallback span (a
// non-replaced element that never balloons), so it missed the bug entirely.
async function injectAuthMenu(page: Page): Promise<void> {
  await page.evaluate(() => {
    const slot = document.querySelector('.navbar-auth');
    if (!slot) return;
    const big = 'data:image/svg+xml,' + encodeURIComponent(
      "<svg xmlns='http://www.w3.org/2000/svg' width='256' height='256'><rect width='256' height='256' fill='#888'/></svg>");
    slot.innerHTML =
      '<div class="auth-menu" id="auth-menu">' +
      '<img class="auth-avatar" alt="" src="' + big + '">' +
      '<span class="auth-name">Paweł</span>' +
      '<div class="auth-dropdown" id="auth-dropdown">' +
      '<form method="post" action="/auth/logout" class="auth-logout-form">' +
      '<button type="submit" class="auth-logout-btn">Wyloguj się</button>' +
      '</form></div></div>';
  });
  // Wait until the avatar image has loaded its intrinsic size — that's the
  // state that balloons an unpinned flex-item img.
  await page.waitForFunction(() => {
    const image = document.querySelector('.auth-menu .auth-avatar') as HTMLImageElement | null;
    return !!image && image.complete && image.naturalWidth > 0;
  });
}

test.describe('logged-in avatar pill height', () => {
  // Runs on EVERY project — desktop + mobile, Chromium + WebKit + Firefox.
  // The "avatar pinned to the top of a tall pill" misalignment reproduces in
  // desktop Safari too, so gating this to mobile-portrait (as it once was) let
  // the bug slip through on exactly the engine the user hit it on.
  test.beforeEach(async ({ page }) => {
    await page.goto('/poznan/');
    await waitForCards(page);
    await injectAuthMenu(page);
  });

  test('avatar pill is the same height as the search input', async ({ page }) => {
    const heights = await page.evaluate(() => {
      const navbar = document.querySelector('.navbar')!;
      const pill = navbar.querySelector('.auth-menu') as HTMLElement | null;
      const search = navbar.querySelector('.search-input') as HTMLElement | null;
      return {
        pill:   pill   ? pill.getBoundingClientRect().height   : -1,
        search: search ? search.getBoundingClientRect().height : -1,
      };
    });
    expect(heights.pill, 'auth-menu pill not rendered').toBeGreaterThan(0);
    // Below ~290px (e.g. zoomed phones) the navbar hides the search box
    // entirely, so there's no height to compare the pill against — skip there.
    test.skip(heights.search <= 0, 'search box is hidden at this width');
    expect(heights.search, '.search-input not rendered').toBeGreaterThan(0);
    expect(
      Math.abs(heights.pill - heights.search),
      `avatar pill ${heights.pill.toFixed(1)}px ≠ search ${heights.search.toFixed(1)}px`,
    ).toBeLessThanOrEqual(1);
  });

  test('the avatar image stays a pinned 22px circle, never ballooned or misaligned', async ({ page }) => {
    const m = await page.evaluate(() => {
      const navbar = document.querySelector('.navbar')!;
      const image = navbar.querySelector('.auth-menu .auth-avatar') as HTMLElement | null;
      const pill = navbar.querySelector('.auth-menu') as HTMLElement | null;
      // Day pill is the navbar-row reference control. (Search used to be it,
      // but on portrait it floats at the bottom of the viewport, so it's no
      // longer on this row — the day pills are the stable in-row yardstick.)
      const ref = navbar.querySelector('.day-pill') as HTMLElement | null;
      if (!image || !pill) return null;
      const cs = getComputedStyle(image);
      const ir = image.getBoundingClientRect();
      const pr = pill.getBoundingClientRect();
      const sr = ref ? ref.getBoundingClientRect() : null;
      return {
        w: ir.width, h: ir.height, top: ir.top,
        minW: cs.minWidth, minH: cs.minHeight,
        maxW: cs.maxWidth, maxH: cs.maxHeight,
        pillH: pr.height,
        pillMid: pr.top + pr.height / 2,
        refH: sr ? sr.height : -1,
        refMid: sr ? sr.top + sr.height / 2 : null,
        imgMid: ir.top + ir.height / 2,
      };
    });
    expect(m, 'avatar image not rendered').not.toBeNull();
    // THE defense: the min-size must stay pinned. If it reverts to `auto`, a
    // flex-item replaced element grows to the photo's intrinsic size on iOS.
    expect(m!.minW, 'auth-avatar min-width must be pinned (not auto)').toBe('22px');
    expect(m!.minH, 'auth-avatar min-height must be pinned (not auto)').toBe('22px');
    // Hard cap too — min+max+width all 22px means no engine/version/zoom can
    // render it any taller, even where the flex min-size balloon is live.
    expect(m!.maxW, 'auth-avatar max-width must cap at 22px').toBe('22px');
    expect(m!.maxH, 'auth-avatar max-height must cap at 22px').toBe('22px');
    // Stays the intended small square — never ballooned to the photo's size.
    expect(m!.w, `avatar width ${m!.w}px`).toBeLessThanOrEqual(24);
    expect(m!.h, `avatar height ${m!.h}px`).toBeLessThanOrEqual(24);
    // THE misalignment defense: the avatar must sit vertically CENTRED inside
    // its own pill, regardless of how tall the pill grows. The reported bug is
    // the avatar pinned to the TOP of a tall purple pill with empty space
    // below it — i.e. the pill stretched (to the navbar row height) while the
    // 22px avatar, unable to stretch, fell to flex-start. A centred avatar has
    // |imgMid - pillMid| ≈ 0; a top-pinned one in a tall pill blows past this.
    expect(
      Math.abs(m!.imgMid - m!.pillMid),
      `avatar mid ${m!.imgMid.toFixed(1)} not centred in pill mid ${m!.pillMid.toFixed(1)} (pill ${m!.pillH.toFixed(1)}px tall)`,
    ).toBeLessThanOrEqual(1.5);
    // Against the day-pill row reference: never taller than it, and centred on
    // the same row (no vertical misalignment).
    if (m!.refH > 0) {
      expect(m!.h, `avatar ${m!.h}px taller than day pill ${m!.refH}px`).toBeLessThanOrEqual(m!.refH + 1);
      // The pill must share the row's MIDLINE, not merely be "roughly on the
      // same row". A ~1.5px offset — the avatar pill riding above the row
      // because its block wrapper (`.navbar-auth`) reserved line-height
      // descender space below the inline-flex pill, pinning the pill to the
      // wrapper's top — is invisible at 100% but glaring under the pinch-zoom a
      // phone user applies. A 1px bound catches it; the old 6px bound let it
      // through. Fixed by `.navbar-auth { display:flex; align-items:center }`.
      expect(
        Math.abs(m!.pillMid - (m!.refMid as number)),
        `pill mid ${m!.pillMid.toFixed(1)} not on day-pill mid ${(m!.refMid as number).toFixed(1)} — avatar pill vertically misaligned vs the navbar row`,
      ).toBeLessThanOrEqual(1);
    }
  });
});

// ── Avatar pill survives Bootstrap CSS NOT being applied ──────────
//
// The reported "avatar pinned to the top of a tall purple pill" bug only ever
// reproduced in the wild, never in these injected tests — because the test
// page always loaded Bootstrap, and the navbar got its `align-items:center`
// from Bootstrap's `.align-items-center` utility class. In real Safari that
// CSS can be missing at layout time (slow CDN, blocked, a stale cache entry),
// and the navbar falls back to flex's DEFAULT `align-items:stretch`: the auth
// pill stretches to the navbar row height (driven by the taller search box)
// while the 22px avatar — unable to stretch — drops to the top of the pill.
//
// This block reproduces that exact condition by ABORTING the Bootstrap
// stylesheet request, so the only navbar centering left is whatever the
// page's own inline `<style>` provides. The fix is the inlined
// `.navbar { ...; align-items: center; ... }` rule; with Bootstrap blocked
// this test FAILS on the pre-fix CSS (pill stretches, avatar top-pinned) and
// PASSES once the navbar owns its own `align-items:center`.
test.describe('avatar pill without Bootstrap CSS', () => {
  test.beforeEach(async ({ page }) => {
    // Drop the Bootstrap stylesheet on the floor — simulate it never applying.
    await page.route(/bootstrap.*\.css$/, (route) => route.abort());
    await page.goto('/poznan/');
    await waitForCards(page);
    await injectAuthMenu(page);
  });

  test('avatar stays centred in its pill even when Bootstrap never loads', async ({ page }) => {
    const m = await page.evaluate(() => {
      const navbar = document.querySelector('.navbar')!;
      const image = navbar.querySelector('.auth-menu .auth-avatar') as HTMLElement | null;
      const pill = navbar.querySelector('.auth-menu') as HTMLElement | null;
      const search = navbar.querySelector('.search-input') as HTMLElement | null;
      if (!image || !pill) return null;
      const ir = image.getBoundingClientRect();
      const pr = pill.getBoundingClientRect();
      const sr = search ? search.getBoundingClientRect() : null;
      return {
        avatarH: ir.height,
        pillH: pr.height,
        imgMid: ir.top + ir.height / 2,
        pillMid: pr.top + pr.height / 2,
        navAlign: getComputedStyle(navbar as HTMLElement).alignItems,
        searchH: sr ? sr.height : -1,
      };
    });
    expect(m, 'auth-menu / avatar not rendered').not.toBeNull();
    // The navbar must center its own children — not lean on Bootstrap's utility.
    expect(m!.navAlign, '.navbar align-items must be center from inline CSS').toBe('center');
    // The avatar must sit centred in its pill (the bug pins it to the top of a
    // stretched pill). This is the assertion that fails on the pre-fix CSS.
    // (A tall pill is fine as long as the avatar is centred in it — without
    // Bootstrap every navbar control renders at its natural ~40px height and
    // the pill matches them. The BUG is the avatar pinned to the TOP of the
    // pill with empty space below, i.e. align-items falling back to stretch.)
    expect(
      Math.abs(m!.imgMid - m!.pillMid),
      `avatar mid ${m!.imgMid.toFixed(1)} not centred in pill mid ${m!.pillMid.toFixed(1)} (pill ${m!.pillH.toFixed(1)}px, avatar ${m!.avatarH.toFixed(1)}px, search ${m!.searchH.toFixed(1)}px)`,
    ).toBeLessThanOrEqual(1.5);
  });
});

// ── Avatar pill matches the search box on a DESKTOP browser narrowed
//    to mobile width ────────────────────────────────────────────────
//
// This is the case the mobile-emulation projects (webkit-iphone-*) miss:
// a real desktop browser dragged narrow. The reported bug — the avatar pill
// noticeably TALLER than the search box — only shows up at ≤575px, which is
// exactly why "it works at desktop width but breaks when I narrow it" reads
// like a width regression. The cause is the auth pill keeping its 35px
// desktop height at narrow widths while the search box drops to the 28px
// mobile height; the fix is the `@media (max-width:575px) { .navbar
// .auth-menu { height:28px; … } }` pin that makes the pill track the search
// box. Driving an actual desktop project (not a mobile-emulation viewport)
// and resizing it reproduces the user's exact repro. Pre-fix CSS → pill 35px
// vs search 28px (fails); fixed CSS → both 28px (passes).
test.describe('avatar pill on a desktop browser narrowed to mobile width', () => {
  // Desktop projects only — these are non-emulated browsers we can resize to a
  // phone-narrow viewport, the way the user reproduced it in desktop Safari.
  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(!testInfo.project.name.includes('desktop'), 'desktop projects only');
    await page.setViewportSize({ width: 400, height: 800 });
    await page.goto('/poznan/');
    await waitForCards(page);
    await injectAuthMenu(page);
  });

  test('the avatar pill is the same height as the navbar controls at 400px wide', async ({ page }) => {
    // Search is dropped below 480px, so the still-visible day pills are the
    // reference control the avatar pill must match in height + midline.
    const m = await page.evaluate(() => {
      const navbar = document.querySelector('.navbar')!;
      const pill = navbar.querySelector('.auth-menu') as HTMLElement | null;
      const ref = navbar.querySelector('.day-pill') as HTMLElement | null;
      const image = navbar.querySelector('.auth-menu .auth-avatar') as HTMLElement | null;
      if (!pill || !image) return null;
      const pr = pill.getBoundingClientRect();
      const rr = ref ? ref.getBoundingClientRect() : null;
      const ir = image.getBoundingClientRect();
      return {
        pillH: pr.height,
        refH: rr ? rr.height : -1,
        imgMid: ir.top + ir.height / 2,
        pillMid: pr.top + pr.height / 2,
        refMid: rr ? rr.top + rr.height / 2 : null,
        vw: window.innerWidth,
      };
    });
    expect(m, 'auth-menu / avatar not rendered').not.toBeNull();
    expect(m!.vw, 'expected a narrow (mobile-width) viewport').toBeLessThanOrEqual(575);
    expect(m!.refH, 'day pill should be visible at 400px').toBeGreaterThan(0);
    // The headline assertion: the pill must NOT stand taller than the other
    // mobile controls. Pre-fix it stayed 35px while they dropped to 28px.
    expect(
      Math.abs(m!.pillH - m!.refH),
      `avatar pill ${m!.pillH.toFixed(1)}px ≠ day pill ${m!.refH.toFixed(1)}px at ${m!.vw}px wide`,
    ).toBeLessThanOrEqual(1);
    // …and the avatar stays centred within whatever height the pill is.
    expect(
      Math.abs(m!.imgMid - m!.pillMid),
      `avatar mid ${m!.imgMid.toFixed(1)} not centred in pill mid ${m!.pillMid.toFixed(1)}`,
    ).toBeLessThanOrEqual(1.5);
    // …and the pill itself shares the control row's MIDLINE. This is the
    // misalignment the user hit: the pill rode ~1.5px above because
    // `.navbar-auth` (a block wrapper around an inline-flex pill) reserved
    // line-height descender space below it. Fixed by
    // `.navbar-auth { display:flex; align-items:center }`.
    expect(m!.refMid, 'day pill not visible at 400px').not.toBeNull();
    expect(
      Math.abs(m!.pillMid - (m!.refMid as number)),
      `pill mid ${m!.pillMid.toFixed(1)} not on control mid ${(m!.refMid as number).toFixed(1)}`,
    ).toBeLessThanOrEqual(1);
  });
});

// ── Tablet portrait: focusing search must not reflow the date row ─
//
// iPad Pro 11" portrait is 834px wide — above the 575px mobile
// breakpoint, so it runs the DESKTOP navbar layout (one wrapping row:
// […tabs] [search] [date] [Filtry] [auth], `flex-wrap: wrap`).
// The search input grows 160px → 220px on `:focus`. At this tablet
// width the right cluster already fills the row, so the +60px tips
// the navbar over its width budget and `flex-wrap` shoves the date
// stepper onto a second line the moment the user taps the search box.
//
// The contract under test is an INVARIANT, not an absolute layout:
// *focusing the search box must not move the date stepper*. That holds
// on every engine regardless of where the date sits at rest — and it
// has to, because the resting baseline itself drifts by engine: the CI
// Linux Chrome/WebKit render the navbar controls a hair wider than
// macOS, so at 834px they already wrap the date onto row 2 even at
// rest, whereas macOS / real iPad Safari keep it on the search row
// until focus. Asserting "search and date share a row" would encode
// the macOS baseline and fail on CI; asserting "focus doesn't reflow
// the date" reproduces the user's bug (tap search → date jumps down)
// on the macOS-class engines and stays green on the pre-wrapped ones.
// Locked by the `@media (576–1024px) .search-input:focus` width pin.
test.describe('tablet portrait — search focus does not reflow the date row (834×1194)', () => {
  test.use({ viewport: { width: 834, height: 1194 } });

  test.beforeEach(async ({ page }, testInfo) => {
    // Desktop-class projects only — non-emulated browsers we can drive to
    // a tablet-portrait viewport that runs the desktop navbar layout.
    test.skip(!testInfo.project.name.includes('desktop'), 'desktop projects only');
    await page.goto('/poznan/');
    await waitForCards(page);
  });

  test('the date stepper stays put when the search box is focused', async ({ page }) => {
    const read = () => page.evaluate(() => {
      const navbar = document.querySelector('.navbar') as HTMLElement;
      const date = document.querySelector('.navbar-date') as HTMLElement | null;
      if (!navbar || !date) return null;
      const r = date.getBoundingClientRect();
      return { navH: navbar.getBoundingClientRect().height, dateMid: r.top + r.height / 2 };
    });

    const before = await read();
    expect(before, 'navbar / date not found').not.toBeNull();

    await page.locator('.search-input').focus();
    // Give the focus width transition a beat to settle.
    await page.waitForTimeout(100);

    const after = await read();
    // The date stepper must not move vertically — a wrap caused by the
    // focus growth drops it ~40px to a new row.
    expect(
      Math.abs(after!.dateMid - before!.dateMid),
      `date stepper moved ${(after!.dateMid - before!.dateMid).toFixed(1)}px on search focus (mid ${before!.dateMid.toFixed(1)} → ${after!.dateMid.toFixed(1)}); the row reflowed`,
    ).toBeLessThanOrEqual(2);
    // …and the navbar didn't grow a new row on focus either.
    expect(
      after!.navH,
      `navbar grew ${before!.navH.toFixed(1)} → ${after!.navH.toFixed(1)}px on search focus — the row wrapped`,
    ).toBeLessThanOrEqual(before!.navH + 2);
  });
});

// ── Mobile landscape: font + height uniformity ────────────────────

test.describe('navbar uniformity — mobile landscape', () => {
  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(!testInfo.project.name.includes('landscape'), 'landscape projects only');
    await page.goto('/poznan/');
    await waitForCards(page);
  });

  test('text controls + inputs share a single font-size', async ({ page }) => {
    const { byGroup } = await measureNavbarControls(page);
    const textAndInputs = [...byGroup.text, ...byGroup.input];
    expectUniform('mobile-landscape text+input font', textAndInputs, 'fontPx', 0.5);
  });

  test('all interactive controls share a single height', async ({ page }) => {
    const { byGroup } = await measureNavbarControls(page);
    const everything = [...byGroup.text, ...byGroup.input, ...byGroup.pill];
    expectUniform('mobile-landscape control height', everything, 'heightPx', 1);
  });
});

// ── Orientation stability: portrait ↔ landscape height stays 28px ─

test.describe('navbar orientation uniformity', () => {
  test.beforeEach(async ({ page }, testInfo) => {
    const name = testInfo.project.name;
    test.skip(
      name.includes('landscape') || name.includes('desktop'),
      'rotates viewport itself — bare mobile portrait projects only',
    );
    await page.goto('/poznan/');
    await waitForCards(page);
  });

  test('every control keeps its height after rotating to landscape', async ({ page }) => {
    const portrait = page.viewportSize();
    expect(portrait).toBeTruthy();
    const { width, height } = portrait!;

    const portraitHeights = await measureHeights(page);
    // Below ~290px (zoomed phones) the navbar drops the search input and the
    // mobile-hidden "Zaloguj" button, leaving Filtry + day pills (2 controls);
    // wider phones keep search too (3). Assert the realistic floor for the
    // width so the rotation comparison below still has ≥2 controls.
    const minControls = width <= 290 ? 2 : 3;
    expect(Object.keys(portraitHeights).length).toBeGreaterThanOrEqual(minControls);

    await page.setViewportSize({ width: height, height: width });
    const landscapeHeights = await measureHeights(page);

    const diffs: { sel: string; portrait: number; landscape: number }[] = [];
    for (const [sel, p] of Object.entries(portraitHeights)) {
      const l = landscapeHeights[sel];
      if (l === undefined) continue;
      if (Math.abs(p - l) > 1) diffs.push({ sel, portrait: p, landscape: l });
    }
    expect(diffs, `height changed across rotation:\n${JSON.stringify(diffs, null, 2)}`).toEqual([]);

    for (const [sel, h] of Object.entries(landscapeHeights)) {
      expect(Math.abs(h - 28), `${sel} (${h.toFixed(1)} px) should be 28 px`).toBeLessThanOrEqual(1);
    }
  });
});

// ── Narrow landscape (760×360): compact navbar + 4-col grid ───────

test.describe('narrow landscape (760×360)', () => {
  test.use({ viewport: { width: 760, height: 360 } });

  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(testInfo.project.name.includes('desktop'), 'mobile-class projects only');
    await page.goto('/poznan/');
    await waitForCards(page);
  });

  test('navbar is compact — height ≤ 42px', async ({ page }) => {
    const height = await page.evaluate(() => {
      const navbar = document.querySelector('.navbar') as HTMLElement;
      return navbar ? navbar.getBoundingClientRect().height : -1;
    });
    expect(height).toBeGreaterThan(0);
    expect(height).toBeLessThanOrEqual(42);
  });

  test('film grid shows 4 cards per row, not 6', async ({ page }) => {
    await pinDateFilterAnytime(page);
    const ratio = await measureGridRatio(page);
    expect(ratio).toBeGreaterThan(0.23);
    expect(ratio).toBeLessThan(0.27);
  });

  test('card gutter is reduced from default 1.5rem', async ({ page }) => {
    const gutterPx = await page.evaluate(() => {
      const grid = document.querySelector('#film-grid') as HTMLElement;
      if (!grid) return -1;
      const raw = getComputedStyle(grid).getPropertyValue('--bs-gutter-x').trim();
      const root = parseFloat(getComputedStyle(document.documentElement).fontSize);
      const val = parseFloat(raw);
      if (raw.endsWith('rem')) return val * root;
      if (raw.endsWith('px')) return val;
      return val;
    });
    expect(gutterPx).toBeGreaterThan(0);
    expect(gutterPx).toBeLessThanOrEqual(16);
  });

  test('all visible navbar controls share the same height', async ({ page }) => {
    const heights = await measureHeights(page);
    const values = Object.values(heights);
    // Logged-out mobile (this is a landscape arm) shows three control
    // types — Filtry, day pills, search. The "Zaloguj" button is hidden
    // on mobile, so don't require a fourth; match the >= 3 floor the
    // portrait orientation-uniformity test uses.
    expect(values.length).toBeGreaterThanOrEqual(3);
    const target = values[0];
    for (const [sel, h] of Object.entries(heights)) {
      expect(h, `${sel} height ${h} ≠ ${target}`).toBeCloseTo(target, 0);
    }
  });
});

// ── Portrait Filtry button containment ────────────────────────────

test.describe('portrait filtry button (360×760)', () => {
  test.use({ viewport: { width: 360, height: 760 } });

  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(testInfo.project.name.includes('desktop'), 'mobile-class projects only');
    await page.goto('/poznan/');
    await waitForCards(page);
  });

  test('Filtry button right edge stays inside viewport', async ({ page }) => {
    const right = await page.evaluate(() => {
      const button = document.querySelector('#format-filter-btn');
      return button ? button.getBoundingClientRect().right : -1;
    });
    expect(right).toBeGreaterThan(0);
    expect(right).toBeLessThanOrEqual(360 + 1);
  });
});

// ── Day pills: all four fit, unclipped, on a common phone ─────────
//
// Regression for the pill row overflowing its container and sliding the
// long "Wszystkie" pill under the Filtry button. The four pills must each
// render at a non-zero width, stay inside the navbar, and not be internally
// clipped (scrollWidth ≈ clientWidth) at a typical phone width.

test.describe('day pills (390×844)', () => {
  test.use({ viewport: { width: 390, height: 844 } });

  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(testInfo.project.name.includes('desktop'), 'mobile-class projects only');
    await page.goto('/poznan/');
    await waitForCards(page);
  });

  test('all four day pills are visible, inside the navbar, and not clipped', async ({ page }) => {
    const pills = await page.evaluate(() => {
      const navRight = document.querySelector('.navbar')!.getBoundingClientRect().right;
      return Array.from(document.querySelectorAll('.day-pill')).map((p) => {
        const element = p as HTMLElement;
        const b = element.getBoundingClientRect();
        return {
          label: element.textContent,
          width: b.width,
          withinNav: b.right <= navRight + 0.5,
          clip: element.scrollWidth - element.clientWidth,
        };
      });
    });
    expect(pills.length, 'expected four day pills').toBe(4);
    for (const p of pills) {
      expect(p.width, `pill "${p.label}" has zero width`).toBeGreaterThan(8);
      expect(p.withinNav, `pill "${p.label}" overflows the navbar`).toBe(true);
      expect(p.clip, `pill "${p.label}" label is clipped`).toBeLessThanOrEqual(1);
    }
  });
});

// ── Day pills: the row spreads to fill the logo→Filtry span ────────
//
// The day-pill row fans out across the whole span between the logo (the 🎬
// "camera" mark) and the Filtry button — like the iOS/Android day-chip row —
// rather than packing tight against Filtry with empty space on the logo side.
// (Search used to bound the right of this span; on portrait it now floats at
// the bottom of the viewport, so the day row runs logo→Filtry.) Pre-change
// `.navbar-date` was `margin-left:auto; flex-shrink:0`, so on a wider phone the
// free space pooled into a wide gap LEFT of the first pill (≈60px at 440px);
// the row now grows (`flex:1`, pills `flex:1 0 auto`) to absorb it, leaving
// only the small navbar inter-item gap. The viewport is 440px on purpose: it
// has real horizontal slack, so the old packed-right CSS shows the big gap
// (test fails) and the spread layout closes it (test passes).
test.describe('day pills spread to fill the bar (440×956)', () => {
  test.use({ viewport: { width: 440, height: 956 } });

  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(testInfo.project.name.includes('desktop'), 'mobile-class projects only');
    await page.goto('/poznan/');
    await waitForCards(page);
  });

  test('the day-pill row fills the gap between the logo and the Filtry button', async ({ page }) => {
    const m = await page.evaluate(() => {
      const logo   = document.querySelector('.navbar-logo')   as HTMLElement | null;
      const date   = document.querySelector('.navbar-date')   as HTMLElement | null;
      const filtry = document.querySelector('.navbar-filtry') as HTMLElement | null;
      const visible = (el: HTMLElement | null) =>
        !!el && el.offsetParent !== null && el.getBoundingClientRect().width > 0;
      if (!visible(logo) || !visible(date) || !visible(filtry)) return null;
      return {
        logoRight:   logo!.getBoundingClientRect().right,
        dateLeft:    date!.getBoundingClientRect().left,
        dateRight:   date!.getBoundingClientRect().right,
        filtryLeft:  filtry!.getBoundingClientRect().left,
      };
    });
    expect(m, 'logo / day-pill row / Filtry not all visible').not.toBeNull();
    // The row starts right after the logo — the old wide margin-left:auto gap
    // (≈60px here) is gone, leaving only the small navbar inter-item gap.
    expect(
      m!.dateLeft - m!.logoRight,
      `gap between logo (right ${m!.logoRight.toFixed(1)}) and day-pill row (left ${m!.dateLeft.toFixed(1)}) is too wide — pills still packed to the right`,
    ).toBeLessThanOrEqual(24);
    // …and it reaches the Filtry button: the row spans essentially the whole span.
    expect(
      m!.filtryLeft - m!.dateRight,
      `day-pill row right (${m!.dateRight.toFixed(1)}) does not reach the Filtry button (left ${m!.filtryLeft.toFixed(1)})`,
    ).toBeLessThanOrEqual(8);
  });
});

// ── Horizontal order: where the search box sits relative to the pills ──
//
// On the DESKTOP navbar layout the inline search input lives to the RIGHT of
// the day-pill row (CSS `order` on `.navbar-date` / `.navbar-search`, with
// `margin-left:auto` holding the cluster right). On the ≤575px PORTRAIT layout
// search leaves the row entirely — it's the bottom-floating frosted pill — so
// "to the right of the pills" no longer applies; instead the pill must sit
// BELOW the navbar, over the scrolling grid, like the iOS/Android apps.

test.describe('navbar order — search position vs the day pills', () => {
  for (const [label, viewport] of [
    ['desktop width', { width: 1280, height: 800 }],
    ['phone width',   { width: 390,  height: 844 }],
  ] as const) {
    test(`search is correctly placed relative to the day pills at ${label}`, async ({ page }, testInfo) => {
      // Desktop-class (non-emulated, resizable) projects only — they run both
      // the desktop and the ≤575px layouts as we size them down, without the
      // mobile-emulation projects' fixed viewports fighting `setViewportSize`.
      test.skip(!testInfo.project.name.includes('desktop'), 'desktop projects only');
      await page.setViewportSize(viewport);
      await page.goto('/poznan/');
      await waitForCards(page);

      if (viewport.width <= 575) {
        // Portrait: search is the bottom-floating pill — not a row control. It
        // must float below the navbar, in the lower half of the viewport.
        const m = await page.evaluate(() => {
          const nav    = document.querySelector('.navbar')!.getBoundingClientRect();
          const search = document.querySelector('.navbar-search') as HTMLElement | null;
          const sr = search ? search.getBoundingClientRect() : null;
          return sr && sr.width > 0
            ? { navBottom: nav.bottom, searchTop: sr.top, vh: window.innerHeight }
            : null;
        });
        expect(m, 'floating search pill not rendered').not.toBeNull();
        expect(
          m!.searchTop,
          `floating search top ${m!.searchTop.toFixed(1)}px should be below the navbar bottom ${m!.navBottom.toFixed(1)}px`,
        ).toBeGreaterThan(m!.navBottom);
        expect(
          m!.searchTop,
          `floating search top ${m!.searchTop.toFixed(1)}px should sit in the lower half of the ${m!.vh}px viewport`,
        ).toBeGreaterThan(m!.vh / 2);
        return;
      }

      // Desktop: the inline search input is entirely to the right of the pills.
      const m = await page.evaluate(() => {
        const date   = document.querySelector('.navbar-date')   as HTMLElement | null;
        const search = document.querySelector('.navbar-search') as HTMLElement | null;
        const visible = (el: HTMLElement | null) =>
          !!el && el.offsetParent !== null && el.getBoundingClientRect().width > 0;
        if (!visible(date) || !visible(search)) return null;
        return {
          dateRight:   date!.getBoundingClientRect().right,
          searchLeft:  search!.getBoundingClientRect().left,
        };
      });
      expect(m, 'day pills / search box not both visible').not.toBeNull();
      expect(
        m!.searchLeft,
        `search box left ${m!.searchLeft.toFixed(1)}px should be ≥ day pills right ${m!.dateRight.toFixed(1)}px — search is no longer to the right of the pills`,
      ).toBeGreaterThanOrEqual(m!.dateRight - 0.5);
    });
  }
});

// ── Orientation flip: grid column transitions ─────────────────────

const PORTRAIT  = { width: 440, height: 956 };
const LANDSCAPE = { width: 956, height: 440 };

test.describe('orientation flip: portrait → landscape', () => {
  test.use({ viewport: PORTRAIT });

  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(testInfo.project.name.includes('desktop'), 'mobile projects only');
    await page.goto('/poznan/');
    await waitForCards(page);
  });

  test('navbar controls become uniform 28px and height ≤ 42px after rotating', async ({ page }) => {
    await page.setViewportSize(LANDSCAPE);
    await page.waitForTimeout(200);

    const heights = await measureHeights(page);
    const values = Object.values(heights);
    // Logged-out mobile (this is a landscape arm) shows three control
    // types — Filtry, day pills, search. The "Zaloguj" button is hidden
    // on mobile, so don't require a fourth; match the >= 3 floor the
    // portrait orientation-uniformity test uses.
    expect(values.length).toBeGreaterThanOrEqual(3);
    const target = values[0];
    for (const [sel, h] of Object.entries(heights)) {
      expect(h, `${sel}: ${h} ≠ ${target}`).toBeCloseTo(target, 0);
    }

    const navHeight = await page.evaluate(() =>
      document.querySelector('.navbar')!.getBoundingClientRect().height);
    expect(navHeight).toBeLessThanOrEqual(42);
  });

  test('grid switches from 2 cols in portrait to 6 cols in landscape', async ({ page }) => {
    await pinDateFilterAnytime(page);
    const before = await measureGridRatio(page);
    expect(before).toBeGreaterThan(0.45);
    expect(before).toBeLessThan(0.55);

    await page.setViewportSize(LANDSCAPE);
    await page.waitForTimeout(200);

    const after = await measureGridRatio(page);
    expect(after).toBeGreaterThan(0.15);
    expect(after).toBeLessThan(0.18);
  });
});

test.describe('orientation flip: landscape → portrait', () => {
  test.use({ viewport: LANDSCAPE });

  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(testInfo.project.name.includes('desktop'), 'mobile projects only');
    await page.goto('/poznan/');
    await waitForCards(page);
  });

  test('grid switches from 6 cols in landscape to 2 cols in portrait', async ({ page }) => {
    await pinDateFilterAnytime(page);
    const before = await measureGridRatio(page);
    expect(before).toBeGreaterThan(0.15);
    expect(before).toBeLessThan(0.18);

    await page.setViewportSize(PORTRAIT);
    await page.waitForTimeout(200);

    const after = await measureGridRatio(page);
    expect(after).toBeGreaterThan(0.45);
    expect(after).toBeLessThan(0.55);
  });
});
