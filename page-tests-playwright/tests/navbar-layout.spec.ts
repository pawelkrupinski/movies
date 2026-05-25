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
