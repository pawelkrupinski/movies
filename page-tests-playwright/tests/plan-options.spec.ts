import { test, expect, type Page } from '@playwright/test';
import { setLocalStorageJson } from './helpers';

// /plan's "Twoje filmy" section: one block per selected movie with a
// summary subtitle ("od X do Y", "tylko: A, B, C", etc.) followed by
// every option (date • time — cinema, room) capped at 8 with a
// "+ N więcej" tail. The Playwright suite drives the actual JS
// against the 17-05-2026 fixture corpus so each shape stays pinned
// end-to-end.

const PL_MONTH_RE =
  /(stycznia|lutego|marca|kwietnia|maja|czerwca|lipca|sierpnia|września|października|listopada|grudnia)/;
const PL_SHORT_DAY_RE = /(nd|pn|wt|śr|cz|pt|sb)/;

type Showing = { movie: string; date: string; cinema: string };
type Bucket = { dates: string[]; cinemas: string[]; count: number };

async function readShowings(page: Page): Promise<Showing[]> {
  return page.evaluate(() =>
    (globalThis as unknown as { SHOWINGS: Showing[] }).SHOWINGS ?? [],
  );
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
  const byMovie = new Map<string, { dates: Set<string>; cinemas: Set<string>; count: number }>();
  for (const s of showings) {
    if (s.date < today) continue;
    let b = byMovie.get(s.movie);
    if (!b) {
      b = { dates: new Set(), cinemas: new Set(), count: 0 };
      byMovie.set(s.movie, b);
    }
    b.dates.add(s.date);
    b.cinemas.add(s.cinema);
    b.count++;
  }
  const out = new Map<string, Bucket>();
  for (const [movie, { dates, cinemas, count }] of byMovie) {
    out.set(movie, {
      dates: [...dates].sort(),
      cinemas: [...cinemas].sort((a, b) => a.localeCompare(b, 'pl')),
      count,
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
  await page.evaluate(() =>
    (globalThis as unknown as { applyFilters?: () => void }).applyFilters?.(),
  );
}

async function firstMovieBlockSummary(page: Page): Promise<string> {
  const text = await page
    .locator('.plan-movie')
    .first()
    .locator('.plan-movie-summary')
    .textContent();
  expect(text).not.toBeNull();
  return text!;
}

test.describe('plan availability summary', () => {

  test('"tylko <day>" form for a movie playing on exactly one date', async ({ page }) => {
    await page.goto('/plan');
    await page.waitForSelector('.plan-card-col[data-title]', { state: 'attached' });

    const byMovie = indexByMovie(await readShowings(page), await readPageToday(page));
    const pick = [...byMovie.entries()].find(([, b]) => b.dates.length === 1);
    test.skip(!pick, 'fixture has no single-date movie');

    const [title] = pick!;
    await selectMovie(page, title);

    const text = await firstMovieBlockSummary(page);
    expect(text).toMatch(new RegExp(`^tylko ${PL_SHORT_DAY_RE.source} \\d{1,2} ${PL_MONTH_RE.source}`));
  });

  test('"X i Y" form for a movie playing on exactly two dates', async ({ page }) => {
    await page.goto('/plan');
    await page.waitForSelector('.plan-card-col[data-title]', { state: 'attached' });

    const byMovie = indexByMovie(await readShowings(page), await readPageToday(page));
    const pick = [...byMovie.entries()].find(([, b]) => b.dates.length === 2);
    test.skip(!pick, 'fixture has no two-date movie');

    const [title] = pick!;
    await selectMovie(page, title);

    const text = await firstMovieBlockSummary(page);
    expect(text).toMatch(new RegExp(`^${PL_SHORT_DAY_RE.source} \\d{1,2} ${PL_MONTH_RE.source} i ${PL_SHORT_DAY_RE.source} \\d{1,2} ${PL_MONTH_RE.source}`));
    expect(text).not.toMatch(/tylko: /);
    expect(text).not.toMatch(/^od /);
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

    const text = await firstMovieBlockSummary(page);
    expect(text).toMatch(new RegExp(`^od ${PL_SHORT_DAY_RE.source} \\d{1,2} ${PL_MONTH_RE.source} do ${PL_SHORT_DAY_RE.source} \\d{1,2} ${PL_MONTH_RE.source}`));
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

    const text = await firstMovieBlockSummary(page);
    expect(text).toMatch(/^tylko: /);
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

    const text = await firstMovieBlockSummary(page);
    expect(text).toContain(`, w „${cinema}"`);
    // Cinema clause is the tail, not the head.
    expect(text.indexOf(` w „${cinema}"`)).toBeGreaterThan(0);
  });
});

test.describe('plan option list', () => {

  test('renders one block per selected movie, each carrying that movie\'s options', async ({ page }) => {
    await page.goto('/plan');
    await page.waitForSelector('.plan-card-col[data-title]', { state: 'attached' });

    const byMovie = indexByMovie(await readShowings(page), await readPageToday(page));
    // Pick two distinct movies — proves multi-movie selection renders
    // ONE block per movie rather than a flat day-by-day schedule.
    const entries = [...byMovie.entries()].filter(([, b]) => b.count >= 1);
    test.skip(entries.length < 2, 'fixture has fewer than two movies');

    const [a, b] = [entries[0][0], entries[1][0]];
    await setLocalStorageJson(page, 'selectedMovies', [a, b]);
    await page.evaluate(() =>
      (globalThis as unknown as { applyFilters?: () => void }).applyFilters?.(),
    );

    await expect(page.locator('.plan-movie')).toHaveCount(2);
    await expect(
      page.locator('.plan-movie .plan-movie-title', { hasText: a }),
    ).toBeVisible();
    await expect(
      page.locator('.plan-movie .plan-movie-title', { hasText: b }),
    ).toBeVisible();
    // Each block lists at least one .plan-option row.
    const optionCount = await page.locator('.plan-movie .plan-option').count();
    expect(optionCount).toBeGreaterThanOrEqual(2);
  });

  test('truncates option lists past 8 entries with a "+ N więcej" tail', async ({ page }) => {
    await page.goto('/plan');
    await page.waitForSelector('.plan-card-col[data-title]', { state: 'attached' });

    const byMovie = indexByMovie(await readShowings(page), await readPageToday(page));
    const pick = [...byMovie.entries()].find(([, b]) => b.count > 8);
    test.skip(!pick, 'fixture has no movie with more than 8 future showings');

    const [title, { count }] = pick!;
    await selectMovie(page, title);

    await expect(page.locator('.plan-movie .plan-option')).toHaveCount(8);
    const more = await page.locator('.plan-movie .plan-options-more').textContent();
    expect(more).toBe(`+ ${count - 8} więcej`);
  });

  test('keeps both movies visible when their best dates collide', async ({ page }) => {
    // Conflict-tolerance: the old scheduler dropped the second movie of
    // a same-day collision into Ograniczenia. The new model shows
    // every option for both films instead.
    await page.goto('/plan');
    await page.waitForSelector('.plan-card-col[data-title]', { state: 'attached' });

    const showings = await readShowings(page);
    const today = await readPageToday(page);
    // Find two movies sharing at least one future date — the most direct
    // collision case the old "one movie per day" cap would have lost.
    const datesByMovie = new Map<string, Set<string>>();
    for (const s of showings) {
      if (s.date < today) continue;
      if (!datesByMovie.has(s.movie)) datesByMovie.set(s.movie, new Set());
      datesByMovie.get(s.movie)!.add(s.date);
    }
    const entries = [...datesByMovie.entries()];
    let pair: [string, string] | null = null;
    for (let i = 0; i < entries.length && !pair; i++) {
      for (let j = i + 1; j < entries.length && !pair; j++) {
        const sharedDate = [...entries[i][1]].find(d => entries[j][1].has(d));
        if (sharedDate) pair = [entries[i][0], entries[j][0]];
      }
    }
    test.skip(!pair, 'fixture has no two movies sharing a date');

    await setLocalStorageJson(page, 'selectedMovies', [pair![0], pair![1]]);
    await page.evaluate(() =>
      (globalThis as unknown as { applyFilters?: () => void }).applyFilters?.(),
    );

    await expect(
      page.locator('.plan-movie .plan-movie-title', { hasText: pair![0] }),
    ).toBeVisible();
    await expect(
      page.locator('.plan-movie .plan-movie-title', { hasText: pair![1] }),
    ).toBeVisible();
  });

  test('orders movie blocks by earliest surviving showing', async ({ page }) => {
    // Picks two movies whose earliest future dates differ. The one that
    // plays sooner must lead in the rendered list regardless of how the
    // titles sort alphabetically.
    await page.goto('/plan');
    await page.waitForSelector('.plan-card-col[data-title]', { state: 'attached' });

    const showings = await readShowings(page);
    const today    = await readPageToday(page);

    const earliestByMovie = new Map<string, string>();
    for (const s of showings) {
      if (s.date < today) continue;
      const cur = earliestByMovie.get(s.movie);
      if (!cur || s.date < cur) earliestByMovie.set(s.movie, s.date);
    }
    const sortedByDate = [...earliestByMovie.entries()].sort((a, b) =>
      a[1].localeCompare(b[1]),
    );
    // Pick the first-playing movie and a later one whose title sorts
    // BEFORE the early one — that's how we know the order came from the
    // earliest-screening rule, not the old alphabetical fallback.
    const early = sortedByDate[0];
    const lateBeforeAlphabetically = sortedByDate
      .slice(1)
      .find(([title, date]) =>
        date > early[1] && title.localeCompare(early[0], 'pl') < 0,
      );
    test.skip(
      !lateBeforeAlphabetically,
      'fixture has no late-screening movie that sorts alphabetically before the earliest one',
    );

    const [earlyTitle]     = early;
    const [laterTitle]     = lateBeforeAlphabetically!;
    await setLocalStorageJson(page, 'selectedMovies', [earlyTitle, laterTitle]);
    await page.evaluate(() =>
      (globalThis as unknown as { applyFilters?: () => void }).applyFilters?.(),
    );

    const titles = await page.locator('.plan-movie .plan-movie-title').allTextContents();
    expect(titles[0]).toBe(earlyTitle);
    expect(titles[1]).toBe(laterTitle);
  });

});

test.describe('plan collapse', () => {

  async function pickTwo(page: Page): Promise<[string, string]> {
    await page.goto('/plan');
    await page.waitForSelector('.plan-card-col[data-title]', { state: 'attached' });
    const showings = await readShowings(page);
    const today    = await readPageToday(page);
    const byMovie  = indexByMovie(showings, today);
    const titles   = [...byMovie.keys()];
    test.skip(titles.length < 2, 'fixture has fewer than two movies');
    await setLocalStorageJson(page, 'selectedMovies', [titles[0], titles[1]]);
    await page.evaluate(() =>
      (globalThis as unknown as { applyFilters?: () => void }).applyFilters?.(),
    );
    return [titles[0], titles[1]];
  }

  test('clicking a movie header collapses just that block', async ({ page }) => {
    const [first, second] = await pickTwo(page);

    // Pre-state: nothing collapsed, both summaries visible.
    await expect(page.locator(`.plan-movie[data-movie="${first}"]`)).not.toHaveClass(/collapsed/);
    await expect(page.locator(`.plan-movie[data-movie="${first}"] .plan-movie-summary`)).toBeVisible();

    await page.locator(`.plan-movie[data-movie="${first}"] .plan-movie-header`).click();

    await expect(page.locator(`.plan-movie[data-movie="${first}"]`)).toHaveClass(/collapsed/);
    // Summary + options drop out of the layout (display: none).
    await expect(page.locator(`.plan-movie[data-movie="${first}"] .plan-movie-summary`)).toBeHidden();
    await expect(page.locator(`.plan-movie[data-movie="${first}"] .plan-options`)).toBeHidden();
    // The other block stays untouched.
    await expect(page.locator(`.plan-movie[data-movie="${second}"]`)).not.toHaveClass(/collapsed/);
  });

  test('"Zwiń wszystko" collapses every block; the button flips to "Rozwiń wszystko" and re-expands', async ({ page }) => {
    const [first, second] = await pickTwo(page);

    const btn = page.locator('#plan-collapse-all');
    await expect(btn).toBeVisible();
    await expect(btn).toHaveText('Zwiń wszystko');

    await btn.click();

    await expect(page.locator(`.plan-movie[data-movie="${first}"]`)).toHaveClass(/collapsed/);
    await expect(page.locator(`.plan-movie[data-movie="${second}"]`)).toHaveClass(/collapsed/);
    await expect(btn).toHaveText('Rozwiń wszystko');

    await btn.click();

    await expect(page.locator(`.plan-movie[data-movie="${first}"]`)).not.toHaveClass(/collapsed/);
    await expect(page.locator(`.plan-movie[data-movie="${second}"]`)).not.toHaveClass(/collapsed/);
    await expect(btn).toHaveText('Zwiń wszystko');
  });

  test('the collapse-all button is hidden when no movies are picked', async ({ page }) => {
    await page.goto('/plan');
    await page.waitForSelector('.plan-card-col[data-title]', { state: 'attached' });
    await setLocalStorageJson(page, 'selectedMovies', []);
    await page.evaluate(() =>
      (globalThis as unknown as { applyFilters?: () => void }).applyFilters?.(),
    );
    await expect(page.locator('#plan-collapse-all')).toBeHidden();
  });

});
