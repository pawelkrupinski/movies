import { test, expect, Page } from '@playwright/test';
import { waitForCards } from './helpers';

// Locks in the contract that every visible interactive control in
// the navbar shares the same font-size AND the same height — on
// both desktop and mobile. Glyph-only controls (★ favourite tab,
// the date-nav arrows ‹ ›) carry their own intentionally-larger
// font for icon legibility, so they're excluded from the font
// check; they stay in the HEIGHT check because the visible box
// has to line up with the other pills.
//
// Two breakpoints are exercised:
//
//   Desktop  — gated on project name containing "desktop".
//              Existing landscape uniform-height spec covered
//              mobile landscape but desktop was untested.
//
//   Mobile portrait — gated on project name = (chromium | webkit
//                     | firefox), the matrix's portrait mobile
//                     projects. Mobile landscape (`*-landscape`)
//                     already has its own uniform-height spec in
//                     `narrow-landscape.spec.ts` — not duplicated
//                     here.
//
// `.search-input` and `.filter-select` are pinned to 16 px on
// mobile to keep iOS Safari from auto-zooming on focus
// (`_sharedStyles.scala.html`), while the surrounding nav-tabs
// shrink with `--mobile-scale`. The mobile-portrait font test
// allows for this — it asserts uniform font WITHIN each font-
// rule group, not across the two carve-outs. Same group for both
// inputs (16 px on mobile, .875 rem on desktop); same group for
// nav-tabs / refresh-btn / auth-name (rides `--navbar-fs`).

// Selectors with their *kind* — `text` selectors share a single
// font-size; `glyph` selectors carry their own larger font for
// the icon they render; `input` selectors are clamped to 16 px on
// mobile (iOS zoom workaround). All three groups share a single
// height target (the navbar's 35 px / 28 px landscape pill row).
//
// `.nav-tab` deliberately excludes `.nav-tab-fav` (the ★) since
// that class is layered on top of `.nav-tab` to bump font-size
// for the glyph. Same trick for `.refresh-btn` vs
// `.date-nav-btn` — the date arrows reuse `.refresh-btn`
// styling but stamp a larger glyph font on top via the
// `.date-nav-btn` class.
const NAV_CONTROLS = {
  text:  ['.nav-tab:not(.nav-tab-fav)', '.nav-tab-login', '.refresh-btn:not(.date-nav-btn)', '.auth-name'],
  glyph: ['.nav-tab-fav', '.date-nav-btn'],
  input: ['.search-input', '#date-filter'],
};

async function measureNavbarControls(page: Page) {
  return page.evaluate((groups) => {
    const nav = document.querySelector('.navbar');
    if (!nav) return { byGroup: {} } as const;
    const byGroup: Record<string, { sel: string; fontPx: number; heightPx: number }[]> = {
      text: [], glyph: [], input: [],
    };
    const isVisible = (el: HTMLElement) =>
      el.offsetParent !== null && el.getBoundingClientRect().height > 0;
    for (const [group, sels] of Object.entries(groups)) {
      for (const sel of sels) {
        // `querySelectorAll` + visibility filter — `querySelector`
        // returns the first DOM match, which for `.filter-select`
        // is the `#from-hour` inside the hidden Filtry panel (and
        // for `.refresh-btn` could be a date arrow if the
        // `:not(.date-nav-btn)` guard above weren't there).
        const els = Array.from(nav.querySelectorAll(sel)) as HTMLElement[];
        const el = els.find(isVisible);
        if (!el) continue;
        byGroup[group].push({
          sel,
          fontPx:   parseFloat(getComputedStyle(el).fontSize),
          heightPx: el.getBoundingClientRect().height,
        });
      }
    }
    return { byGroup };
  }, NAV_CONTROLS);
}

function expectUniform(
  label: string,
  measurements: { sel: string; fontPx?: number; heightPx?: number }[],
  field: 'fontPx' | 'heightPx',
  tolerancePx: number,
): void {
  expect(measurements.length, `${label}: expected ≥ 2 measurements; got ${measurements.length}`).toBeGreaterThanOrEqual(2);
  const target = measurements[0][field] as number;
  for (const m of measurements) {
    const value = m[field] as number;
    expect(
      Math.abs(value - target),
      `${label} (${m.sel}) — value ${value} px diverges from target ${target} px by more than ${tolerancePx} px`,
    ).toBeLessThanOrEqual(tolerancePx);
  }
}

// ── Desktop ───────────────────────────────────────────────────

test.describe('navbar uniformity — desktop', () => {
  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(!testInfo.project.name.includes('desktop'),
      'desktop projects only');
    await page.goto('/');
    await waitForCards(page);
  });

  test('text controls + inputs share a single font-size', async ({ page }) => {
    const { byGroup } = await measureNavbarControls(page);
    // Desktop: nav-tab / nav-tab-login / refresh-btn / auth-name
    // + search-input + date-filter all read .875rem (14 px @ 16 px
    // root). Glyph-only controls (★, ‹ ›) are excluded — they carry
    // their own larger font for icon legibility. 0.5 px tolerance
    // covers sub-pixel rounding between engines.
    const textAndInputs = [...byGroup.text, ...byGroup.input];
    expectUniform('desktop text+input font', textAndInputs, 'fontPx', 0.5);
  });

  test('all interactive controls share a single height', async ({ page }) => {
    const { byGroup } = await measureNavbarControls(page);
    // Every navbar pill — text, glyph, input — is supposed to
    // collapse to the same 35 px box on desktop (`min-height:
    // 35px` + `height: 35px` on the underlying refresh-btn /
    // date-nav-btn / filter-select rules). Sub-pixel rounding
    // tolerated at 1 px.
    const everything = [...byGroup.text, ...byGroup.glyph, ...byGroup.input];
    expectUniform('desktop control height', everything, 'heightPx', 1);
  });
});

// ── Mobile portrait ──────────────────────────────────────────

test.describe('navbar uniformity — mobile portrait', () => {
  test.beforeEach(async ({ page }, testInfo) => {
    // Mobile portrait projects = the matrix's bare chromium /
    // webkit / firefox entries (Pixel 7 / iPhone 13 portrait).
    // Landscape projects have their own spec in
    // `narrow-landscape.spec.ts`; desktop projects are excluded.
    const name = testInfo.project.name;
    const isPortraitMobile = !name.includes('desktop') && !name.includes('landscape');
    test.skip(!isPortraitMobile, 'mobile portrait only');
    await page.goto('/');
    await waitForCards(page);
  });

  test('text controls + inputs share a single font-size', async ({ page }) => {
    const { byGroup } = await measureNavbarControls(page);
    // Mobile portrait: nav-tab / nav-tab-login / refresh-btn /
    // auth-name AND the search + date-filter inputs all ride
    // `var(--navbar-fs)` (= .875rem × --mobile-scale, so 11.9 px
    // at the 0.85 floor, 14 px at the breakpoint top). The
    // previous design kept the inputs at 16 px to dodge iOS
    // Safari's auto-zoom-on-focus, but that produced a visible
    // size mismatch with the surrounding tabs; the project chose
    // visual uniformity over the auto-zoom trade-off.
    const textAndInputs = [...byGroup.text, ...byGroup.input];
    expectUniform('mobile-portrait text+input font', textAndInputs, 'fontPx', 0.5);
  });

  test('all interactive controls share a single height', async ({ page }) => {
    const { byGroup } = await measureNavbarControls(page);
    // Every navbar pill on mobile portrait collapses to the
    // compact 28 px box (shared with landscape — see
    // `navbar-orientation-uniformity.spec.ts`). Sub-pixel
    // rounding tolerated at 1 px.
    const everything = [...byGroup.text, ...byGroup.glyph, ...byGroup.input];
    expectUniform('mobile-portrait control height', everything, 'heightPx', 1);
  });
});

// ── Mobile landscape ──────────────────────────────────────────

test.describe('navbar uniformity — mobile landscape', () => {
  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(!testInfo.project.name.includes('landscape'),
      'landscape projects only');
    await page.goto('/');
    await waitForCards(page);
  });

  test('text controls + inputs share a single font-size', async ({ page }) => {
    const { byGroup } = await measureNavbarControls(page);
    // Landscape: every text-bearing control + input renders at
    // `.8rem × --mobile-scale` (.8 × .85 = ~10.88 px). Glyph
    // controls (★, ‹ ›) keep their own larger font as on the
    // other breakpoints. Sub-pixel tolerance 0.5 px.
    const textAndInputs = [...byGroup.text, ...byGroup.input];
    expectUniform('mobile-landscape text+input font', textAndInputs, 'fontPx', 0.5);
  });

  test('all interactive controls share a single height', async ({ page }) => {
    const { byGroup } = await measureNavbarControls(page);
    // 28 px shared with mobile portrait — see
    // `navbar-orientation-uniformity.spec.ts`. This assertion is
    // the per-breakpoint witness; the cross-orientation spec is
    // the rotational witness.
    const everything = [...byGroup.text, ...byGroup.glyph, ...byGroup.input];
    expectUniform('mobile-landscape control height', everything, 'heightPx', 1);
  });
});
