import { test, expect, type Page } from '@playwright/test';
import { setLocalStorageJson } from './helpers';

// /plan's Ograniczenia section: one note per selected movie describing
// the availability constraint — "od X do Y" for a continuous run,
// "tylko: A, B, C" for a sparse one, plus the cinema clause when one
// cinema plays it. The Playwright suite drives the actual JS against
// the 17-05-2026 fixture corpus so the rule's output shapes are
// pinned end-to-end.

const PL_MONTH_RE =
  /(stycznia|lutego|marca|kwietnia|maja|czerwca|lipca|sierpnia|września|października|listopada|grudnia)/;
const PL_SHORT_DAY_RE = /(nd|pn|wt|śr|cz|pt|sb)/;

function escapeRegex(s: string): string {
  return s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

type Showing = { movie: string; date: string; cinema: string };

type Bucket = { dates: string[]; cinemas: string[] };

async function readShowings(page: Page): Promise<Showing[]> {
  return page.evaluate(() => {
    return (globalThis as unknown as { SHOWINGS: Showing[] }).SHOWINGS ?? [];
  });
}

// Today key in the same shape the page's `todayKey()` produces — must
// be the page's `today`, not the test runner's, because the page is
// what filters past showings out of every render. Read straight from
// the page rather than rebuilding here so DST / TZ quirks can't drift
// the two sides apart.
async function readPageToday(page: Page): Promise<string> {
  return page.evaluate(() =>
    (globalThis as unknown as { todayKey?: () => string }).todayKey?.() ?? '',
  );
}

function indexByMovie(showings: Showing[], today: string): Map<string, Bucket> {
  const byMovie = new Map<string, { dates: Set<string>; cinemas: Set<string> }>();
  for (const s of showings) {
    // Past showings drop out so the index matches what the page's
    // scheduler/limit pass actually sees.
    if (s.date < today) continue;
    let b = byMovie.get(s.movie);
    if (!b) {
      b = { dates: new Set(), cinemas: new Set() };
      byMovie.set(s.movie, b);
    }
    b.dates.add(s.date);
    b.cinemas.add(s.cinema);
  }
  const out = new Map<string, Bucket>();
  for (const [movie, { dates, cinemas }] of byMovie) {
    out.set(movie, {
      dates: [...dates].sort(),
      cinemas: [...cinemas].sort((a, b) => a.localeCompare(b, 'pl')),
    });
  }
  return out;
}

function spanDays(sortedDates: string[]): number {
  const first = new Date(sortedDates[0] + 'T00:00:00').getTime();
  const last = new Date(sortedDates[sortedDates.length - 1] + 'T00:00:00').getTime();
  return Math.round((last - first) / 86400000) + 1;
}

async function selectMovie(page: Page, title: string): Promise<void> {
  await setLocalStorageJson(page, 'selectedMovies', [title]);
  // applyFilters is the page's master render hook — rebuilds picker
  // state from localStorage and rewrites the limits list.
  await page.evaluate(() =>
    (globalThis as unknown as { applyFilters?: () => void }).applyFilters?.(),
  );
}

async function firstLimitText(page: Page): Promise<string> {
  const text = await page.locator('.plan-limit').first().textContent();
  expect(text).not.toBeNull();
  return text!;
}

test.describe('plan availability note', () => {

  test('"tylko <day>" form for a movie playing on exactly one date', async ({ page }) => {
    await page.goto('/plan');
    await page.waitForSelector('.plan-card-col[data-title]', { state: 'attached' });

    const byMovie = indexByMovie(await readShowings(page), await readPageToday(page));
    const pick = [...byMovie.entries()].find(([, b]) => b.dates.length === 1);
    test.skip(!pick, 'fixture has no single-date movie');

    const [title] = pick!;
    await selectMovie(page, title);

    const text = await firstLimitText(page);
    expect(text).toMatch(new RegExp(`^„${escapeRegex(title)}" — tylko ${PL_SHORT_DAY_RE.source} \\d{1,2} ${PL_MONTH_RE.source}`));
  });

  test('"X i Y" form for a movie playing on exactly two dates', async ({ page }) => {
    await page.goto('/plan');
    await page.waitForSelector('.plan-card-col[data-title]', { state: 'attached' });

    const byMovie = indexByMovie(await readShowings(page), await readPageToday(page));
    const pick = [...byMovie.entries()].find(([, b]) => b.dates.length === 2);
    test.skip(!pick, 'fixture has no two-date movie');

    const [title] = pick!;
    await selectMovie(page, title);

    const text = await firstLimitText(page);
    // Two dates: "X i Y" — never "tylko: " (sparse list) or "od…do" (continuous).
    expect(text).toMatch(new RegExp(`^„${escapeRegex(title)}" — ${PL_SHORT_DAY_RE.source} \\d{1,2} ${PL_MONTH_RE.source} i ${PL_SHORT_DAY_RE.source} \\d{1,2} ${PL_MONTH_RE.source}`));
    expect(text).not.toMatch(/tylko: /);
    expect(text).not.toMatch(/^„[^"]+" — od /);
  });

  test('"od X do Y" form for a movie playing continuously over 3+ days', async ({ page }) => {
    await page.goto('/plan');
    await page.waitForSelector('.plan-card-col[data-title]', { state: 'attached' });

    const byMovie = indexByMovie(await readShowings(page), await readPageToday(page));
    const pick = [...byMovie.entries()].find(([, b]) => {
      return b.dates.length >= 3 && spanDays(b.dates) - b.dates.length <= 1;
    });
    test.skip(!pick, 'fixture has no continuous-run movie');

    const [title] = pick!;
    await selectMovie(page, title);

    const text = await firstLimitText(page);
    expect(text).toMatch(new RegExp(`^„${escapeRegex(title)}" — od ${PL_SHORT_DAY_RE.source} \\d{1,2} ${PL_MONTH_RE.source} do ${PL_SHORT_DAY_RE.source} \\d{1,2} ${PL_MONTH_RE.source}`));
  });

  test('"tylko: A, B, C" form for a movie playing on 3+ scattered dates', async ({ page }) => {
    await page.goto('/plan');
    await page.waitForSelector('.plan-card-col[data-title]', { state: 'attached' });

    const byMovie = indexByMovie(await readShowings(page), await readPageToday(page));
    const pick = [...byMovie.entries()].find(([, b]) => {
      return b.dates.length >= 3 && spanDays(b.dates) - b.dates.length > 1;
    });
    test.skip(!pick, 'fixture has no sparse-run movie');

    const [title] = pick!;
    await selectMovie(page, title);

    const text = await firstLimitText(page);
    // Must lead with "tylko: ", carry a comma-separated list, and not
    // collapse into the "od X do Y" continuous form.
    expect(text).toMatch(new RegExp(`^„${escapeRegex(title)}" — tylko: `));
    expect(text).toMatch(/, /);
    expect(text).not.toMatch(/ od .* do /);
  });

  test('appends the cinema constraint after the date constraint when only one cinema plays the movie', async ({ page }) => {
    await page.goto('/plan');
    await page.waitForSelector('.plan-card-col[data-title]', { state: 'attached' });

    const byMovie = indexByMovie(await readShowings(page), await readPageToday(page));
    const pick = [...byMovie.entries()].find(([, b]) => b.cinemas.length === 1);
    test.skip(!pick, 'fixture has no single-cinema movie');

    const [title, { cinemas }] = pick!;
    const cinema = cinemas[0];
    await selectMovie(page, title);

    const text = await firstLimitText(page);
    // Cinema clause lands AFTER the date clause: never the first
    // token after the em-dash, always preceded by " w " with the
    // cinema name in low quotes.
    expect(text).toContain(`, w „${cinema}"`);
    expect(text.indexOf(` w „${cinema}"`)).toBeGreaterThan(text.indexOf(' — ') + 3);
  });
});
