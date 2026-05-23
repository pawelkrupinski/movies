import { test, expect } from '@playwright/test';
import { pinDateFilterAnytime } from './helpers';

// Per-showtime ★ — `.fav-star` inside `.badge-time`. Clicking it
// toggles the screening's id in `favouriteScreenings` localStorage
// and flips the `.is-fav` class. ScreeningId shape is
// `title|cinema|YYYY-MM-DDTHH:MM` (computed by `badgeScreeningId`
// in shared.js).

const firstBadgeContext = (page: import('@playwright/test').Page) =>
  page.evaluate(() => {
    const badge = [...document.querySelectorAll<HTMLElement>('.badge-time')].find(
      (b) => b.style.display !== 'none',
    );
    if (!badge) return null;
    const title  = badge.closest<HTMLElement>('[data-title]')?.dataset.title ?? '';
    const cinema = badge.closest<HTMLElement>('.cinema-group')?.dataset.cinema ?? '';
    const date   = badge.closest<HTMLElement>('.date-group')?.dataset.date ?? '';
    const time   = badge.dataset.time ?? '';
    return { title, cinema, date, time, screeningId: `${title}|${cinema}|${date}T${time}` };
  });

test.describe('per-screening favourite ★', () => {

  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await pinDateFilterAnytime(page);
  });

  test('clicking the ★ persists screeningId to favouriteScreenings localStorage', async ({ page }) => {
    const ctx = await firstBadgeContext(page);
    expect(ctx).not.toBeNull();

    // The ★ is `opacity: 0` at rest (hover-revealed via CSS), so a
    // visibility-checked click fails. The delegated listener routes
    // any `.fav-star` click to `toggleFavScreening` regardless of
    // computed visibility — same path a hover-then-click flow takes.
    await page.evaluate((sid) => {
      const badges = [...document.querySelectorAll<HTMLElement>('.badge-time')];
      const badge = badges.find((b) => {
        const t = b.closest<HTMLElement>('[data-title]')?.dataset.title ?? '';
        const c = b.closest<HTMLElement>('.cinema-group')?.dataset.cinema ?? '';
        const d = b.closest<HTMLElement>('.date-group')?.dataset.date ?? '';
        const tm = b.dataset.time ?? '';
        return `${t}|${c}|${d}T${tm}` === sid;
      });
      const star = badge?.querySelector('.fav-star') as HTMLElement | null;
      if (!star) throw new Error(`no .fav-star for ${sid}`);
      star.dispatchEvent(new MouseEvent('click', { bubbles: true, cancelable: true }));
    }, ctx!.screeningId);

    const stored = await page.evaluate(() => {
      const raw = localStorage.getItem('favouriteScreenings');
      return raw ? (JSON.parse(raw) as string[]) : [];
    });
    expect(stored).toContain(ctx!.screeningId);
  });

  test('the .fav-star flips .is-fav after the click', async ({ page }) => {
    const ctx = await firstBadgeContext(page);
    expect(ctx).not.toBeNull();

    const isFavBefore = await page.evaluate((sid) => {
      const badge = [...document.querySelectorAll<HTMLElement>('.badge-time')].find((b) => {
        const t = b.closest<HTMLElement>('[data-title]')?.dataset.title ?? '';
        const c = b.closest<HTMLElement>('.cinema-group')?.dataset.cinema ?? '';
        const d = b.closest<HTMLElement>('.date-group')?.dataset.date ?? '';
        return `${t}|${c}|${d}T${b.dataset.time}` === sid;
      });
      return badge?.querySelector('.fav-star')?.classList.contains('is-fav') ?? null;
    }, ctx!.screeningId);
    expect(isFavBefore).toBe(false);

    await page.evaluate((sid) => {
      const badge = [...document.querySelectorAll<HTMLElement>('.badge-time')].find((b) => {
        const t = b.closest<HTMLElement>('[data-title]')?.dataset.title ?? '';
        const c = b.closest<HTMLElement>('.cinema-group')?.dataset.cinema ?? '';
        const d = b.closest<HTMLElement>('.date-group')?.dataset.date ?? '';
        return `${t}|${c}|${d}T${b.dataset.time}` === sid;
      });
      const star = badge?.querySelector('.fav-star') as HTMLElement | null;
      star?.dispatchEvent(new MouseEvent('click', { bubbles: true, cancelable: true }));
    }, ctx!.screeningId);

    const isFavAfter = await page.evaluate((sid) => {
      const badge = [...document.querySelectorAll<HTMLElement>('.badge-time')].find((b) => {
        const t = b.closest<HTMLElement>('[data-title]')?.dataset.title ?? '';
        const c = b.closest<HTMLElement>('.cinema-group')?.dataset.cinema ?? '';
        const d = b.closest<HTMLElement>('.date-group')?.dataset.date ?? '';
        return `${t}|${c}|${d}T${b.dataset.time}` === sid;
      });
      return badge?.querySelector('.fav-star')?.classList.contains('is-fav') ?? null;
    }, ctx!.screeningId);
    expect(isFavAfter).toBe(true);
  });

  test('a second click removes the id from favouriteScreenings', async ({ page }) => {
    const ctx = await firstBadgeContext(page);
    expect(ctx).not.toBeNull();

    await page.evaluate((sid) => {
      const badge = [...document.querySelectorAll<HTMLElement>('.badge-time')].find((b) => {
        const t = b.closest<HTMLElement>('[data-title]')?.dataset.title ?? '';
        const c = b.closest<HTMLElement>('.cinema-group')?.dataset.cinema ?? '';
        const d = b.closest<HTMLElement>('.date-group')?.dataset.date ?? '';
        return `${t}|${c}|${d}T${b.dataset.time}` === sid;
      });
      const star = badge?.querySelector('.fav-star') as HTMLElement | null;
      star?.dispatchEvent(new MouseEvent('click', { bubbles: true, cancelable: true }));
      star?.dispatchEvent(new MouseEvent('click', { bubbles: true, cancelable: true }));
    }, ctx!.screeningId);

    const stored = await page.evaluate(() => {
      const raw = localStorage.getItem('favouriteScreenings');
      return raw ? (JSON.parse(raw) as string[]) : [];
    });
    expect(stored).not.toContain(ctx!.screeningId);
  });
});
