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

const NAV_CONTROLS = {
  text:  ['.nav-tab', '.nav-tab-login', '.refresh-btn:not(.date-nav-btn)', '.auth-name'],
  glyph: ['.date-nav-btn'],
  input: ['.search-input', '#date-filter'],
};

const ALL_CONTROL_SELS = [
  '.nav-tab',
  '.nav-tab-login',
  '.refresh-btn:not(.date-nav-btn)',
  '.date-nav-btn',
  '#date-filter',
  '.search-input',
];

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

async function measureHeights(page: Page): Promise<Record<string, number>> {
  return page.evaluate((sels) => {
    const nav = document.querySelector('.navbar');
    if (!nav) return {};
    const out: Record<string, number> = {};
    for (const sel of sels) {
      const candidates = Array.from(nav.querySelectorAll(sel)) as HTMLElement[];
      const el = candidates.find(
        (e) => e.offsetParent !== null && e.getBoundingClientRect().height > 0,
      );
      if (el) out[sel] = el.getBoundingClientRect().height;
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
    await page.goto('/');
    await waitForCards(page);
  });

  test('text controls + inputs share a single font-size', async ({ page }) => {
    const { byGroup } = await measureNavbarControls(page);
    const textAndInputs = [...byGroup.text, ...byGroup.input];
    expectUniform('desktop text+input font', textAndInputs, 'fontPx', 0.5);
  });

  test('all interactive controls share a single height', async ({ page }) => {
    const { byGroup } = await measureNavbarControls(page);
    const everything = [...byGroup.text, ...byGroup.glyph, ...byGroup.input];
    expectUniform('desktop control height', everything, 'heightPx', 1);
  });
});

// ── Mobile portrait: font + height uniformity ─────────────────────

test.describe('navbar uniformity — mobile portrait', () => {
  test.beforeEach(async ({ page }, testInfo) => {
    const name = testInfo.project.name;
    const isPortraitMobile = !name.includes('desktop') && !name.includes('landscape');
    test.skip(!isPortraitMobile, 'mobile portrait only');
    await page.goto('/');
    await waitForCards(page);
  });

  test('text controls + inputs share a single font-size', async ({ page }) => {
    const { byGroup } = await measureNavbarControls(page);
    const textAndInputs = [...byGroup.text, ...byGroup.input];
    expectUniform('mobile-portrait text+input font', textAndInputs, 'fontPx', 0.5);
  });

  test('all interactive controls share a single height', async ({ page }) => {
    const { byGroup } = await measureNavbarControls(page);
    const everything = [...byGroup.text, ...byGroup.glyph, ...byGroup.input];
    expectUniform('mobile-portrait control height', everything, 'heightPx', 1);
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
    const img = document.querySelector('.auth-menu .auth-avatar') as HTMLImageElement | null;
    return !!img && img.complete && img.naturalWidth > 0;
  });
}

test.describe('logged-in avatar pill height', () => {
  test.beforeEach(async ({ page }, testInfo) => {
    const name = testInfo.project.name;
    const isPortraitMobile = !name.includes('desktop') && !name.includes('landscape');
    test.skip(!isPortraitMobile, 'mobile portrait only');
    await page.goto('/');
    await waitForCards(page);
    await injectAuthMenu(page);
  });

  test('avatar pill is the same height as the search input', async ({ page }) => {
    const heights = await page.evaluate(() => {
      const nav = document.querySelector('.navbar')!;
      const pill = nav.querySelector('.auth-menu') as HTMLElement | null;
      const search = nav.querySelector('.search-input') as HTMLElement | null;
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
      const nav = document.querySelector('.navbar')!;
      const img = nav.querySelector('.auth-menu .auth-avatar') as HTMLElement | null;
      const search = nav.querySelector('.search-input') as HTMLElement | null;
      if (!img) return null;
      const cs = getComputedStyle(img);
      const ir = img.getBoundingClientRect();
      const sr = search ? search.getBoundingClientRect() : null;
      return {
        w: ir.width, h: ir.height, top: ir.top,
        minW: cs.minWidth, minH: cs.minHeight,
        searchH: sr ? sr.height : -1,
        searchMid: sr ? sr.top + sr.height / 2 : null,
        imgMid: ir.top + ir.height / 2,
      };
    });
    expect(m, 'avatar image not rendered').not.toBeNull();
    // THE defense: the min-size must stay pinned. If it reverts to `auto`, a
    // flex-item replaced element grows to the photo's intrinsic size on iOS.
    expect(m!.minW, 'auth-avatar min-width must be pinned (not auto)').toBe('22px');
    expect(m!.minH, 'auth-avatar min-height must be pinned (not auto)').toBe('22px');
    // Stays the intended small square — never ballooned to the photo's size.
    expect(m!.w, `avatar width ${m!.w}px`).toBeLessThanOrEqual(24);
    expect(m!.h, `avatar height ${m!.h}px`).toBeLessThanOrEqual(24);
    // When the search box is visible: never taller than it, and centred on the
    // same row (no vertical misalignment).
    if (m!.searchH > 0) {
      expect(m!.h, `avatar ${m!.h}px taller than search ${m!.searchH}px`).toBeLessThanOrEqual(m!.searchH + 1);
      expect(
        Math.abs(m!.imgMid - (m!.searchMid as number)),
        `avatar mid ${m!.imgMid.toFixed(1)} vs search mid ${(m!.searchMid as number).toFixed(1)}`,
      ).toBeLessThan(6);
    }
  });
});

// ── Mobile landscape: font + height uniformity ────────────────────

test.describe('navbar uniformity — mobile landscape', () => {
  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(!testInfo.project.name.includes('landscape'), 'landscape projects only');
    await page.goto('/');
    await waitForCards(page);
  });

  test('text controls + inputs share a single font-size', async ({ page }) => {
    const { byGroup } = await measureNavbarControls(page);
    const textAndInputs = [...byGroup.text, ...byGroup.input];
    expectUniform('mobile-landscape text+input font', textAndInputs, 'fontPx', 0.5);
  });

  test('all interactive controls share a single height', async ({ page }) => {
    const { byGroup } = await measureNavbarControls(page);
    const everything = [...byGroup.text, ...byGroup.glyph, ...byGroup.input];
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
    await page.goto('/');
    await waitForCards(page);
  });

  test('every control keeps its height after rotating to landscape', async ({ page }) => {
    const portrait = page.viewportSize();
    expect(portrait).toBeTruthy();
    const { width, height } = portrait!;

    const portraitHeights = await measureHeights(page);
    expect(Object.keys(portraitHeights).length).toBeGreaterThanOrEqual(3);

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
    await page.goto('/');
    await waitForCards(page);
  });

  test('navbar is compact — height ≤ 42px', async ({ page }) => {
    const height = await page.evaluate(() => {
      const nav = document.querySelector('.navbar') as HTMLElement;
      return nav ? nav.getBoundingClientRect().height : -1;
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
    expect(values.length).toBeGreaterThan(3);
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
    await page.goto('/');
    await waitForCards(page);
  });

  test('Filtry button right edge stays inside viewport', async ({ page }) => {
    const right = await page.evaluate(() => {
      const btn = document.querySelector('#format-filter-btn');
      return btn ? btn.getBoundingClientRect().right : -1;
    });
    expect(right).toBeGreaterThan(0);
    expect(right).toBeLessThanOrEqual(360 + 1);
  });
});

// ── Day selector width: full selected label when the row has room ─
//
// Regression for the day-picker `<select>` being pinned to a fixed
// `max-width` (6.5rem) on every phone ≤575px, which clipped the
// selected label ("Następne 7 dni" / "Kiedykolwiek") with an ellipsis
// even on roomy phones where row 2 had ample space. The cap is now a
// flex shrink: the select shows its full natural width when there's
// room and only narrows when the row genuinely overflows.

test.describe('day selector width (390×844)', () => {
  test.use({ viewport: { width: 390, height: 844 } });

  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(testInfo.project.name.includes('desktop'), 'mobile-class projects only');
    await page.goto('/');
    await waitForCards(page);
  });

  test('selected day label is not clipped when the row has room', async ({ page }) => {
    const widths = await page.evaluate(() => {
      const sel = document.getElementById('date-filter') as HTMLSelectElement | null;
      if (!sel) return null;
      sel.value = 'week'; // "Następne 7 dni" — the widest static option
      const rendered = sel.getBoundingClientRect().width;
      // Reveal the natural (uncapped) width: lift any width constraint
      // inline and re-measure. If the rendered width is short of this,
      // the selected label is being clipped.
      const prevMax = sel.style.maxWidth;
      const prevW = sel.style.width;
      sel.style.maxWidth = 'none';
      sel.style.width = 'max-content';
      const natural = sel.getBoundingClientRect().width;
      sel.style.maxWidth = prevMax;
      sel.style.width = prevW;
      return { rendered, natural };
    });
    expect(widths, '#date-filter not found').not.toBeNull();
    expect(
      widths!.rendered,
      `day selector clipped: rendered ${widths!.rendered.toFixed(1)}px < natural ${widths!.natural.toFixed(1)}px`,
    ).toBeGreaterThanOrEqual(widths!.natural - 1);
  });
});

// ── Orientation flip: grid column transitions ─────────────────────

const PORTRAIT  = { width: 440, height: 956 };
const LANDSCAPE = { width: 956, height: 440 };

test.describe('orientation flip: portrait → landscape', () => {
  test.use({ viewport: PORTRAIT });

  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(testInfo.project.name.includes('desktop'), 'mobile projects only');
    await page.goto('/');
    await waitForCards(page);
  });

  test('navbar controls become uniform 28px and height ≤ 42px after rotating', async ({ page }) => {
    await page.setViewportSize(LANDSCAPE);
    await page.waitForTimeout(200);

    const heights = await measureHeights(page);
    const values = Object.values(heights);
    expect(values.length).toBeGreaterThan(3);
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
    await page.goto('/');
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

// ── Zoomed landscape: date ›  must not touch / overlap Filtry ────

test.describe('zoomed landscape — date-to-filtry gap', () => {
  test.beforeEach(async ({ page }, testInfo) => {
    test.skip(
      !testInfo.project.name.includes('zoomed-landscape'),
      'zoomed-landscape projects only',
    );
    await page.goto('/');
    await waitForCards(page);
  });

  test('date › button does not touch or overlap the Filtry button', async ({ page }) => {
    const gap = await page.evaluate(() => {
      const btns = Array.from(document.querySelectorAll('.date-nav-btn')) as HTMLElement[];
      const stepFwd = btns[btns.length - 1];
      const filtryBtn = document.getElementById('format-filter-btn');
      if (!stepFwd || !filtryBtn) return null;
      return filtryBtn.getBoundingClientRect().left - stepFwd.getBoundingClientRect().right;
    });
    expect(gap, 'date-nav-btn / filtry button not found').not.toBeNull();
    const vp = page.viewportSize()!;
    expect(
      gap!,
      `date›-to-filtry gap is ${gap!.toFixed(1)}px at ${vp.width}×${vp.height}; need ≥ 2px`,
    ).toBeGreaterThanOrEqual(2);
  });
});
