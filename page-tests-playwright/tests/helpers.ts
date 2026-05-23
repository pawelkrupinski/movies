import type { Page } from '@playwright/test';

/**
 * Pin the date filter to "anytime" so the visible-card set isn't a
 * function of the runner's wall-clock relative to the fixture's
 * recorded dates. Mirrors the Scala spec's `pinDateFilterAnytime`
 * helper for the same reason.
 */
export async function pinDateFilterAnytime(page: Page): Promise<void> {
  await page.evaluate(() => {
    const sel = document.getElementById('date-filter') as HTMLSelectElement | null;
    if (sel) {
      sel.value = 'anytime';
      (globalThis as unknown as { applyFilters?: () => void }).applyFilters?.();
    }
  });
}

/**
 * First card whose inline `style.display` isn't `none` AND whose
 * `<img>` is still visible (i.e. the `onerror` fallback chain didn't
 * `display:none` it). `applyFilters` re-appends visible cards after
 * hidden ones, so `querySelector` lands on a hidden one — explicitly
 * walking + checking is engine-agnostic.
 */
export async function firstVisibleTitle(page: Page): Promise<string | null> {
  return page.evaluate(() => {
    const cols = [...document.querySelectorAll<HTMLElement>('.col[data-title]')];
    for (const c of cols) {
      if (c.style.display === 'none') continue;
      const img = c.querySelector<HTMLImageElement>('.poster-wrap > a img');
      if (!img || img.style.display === 'none') continue;
      return c.dataset.title ?? null;
    }
    return null;
  });
}
