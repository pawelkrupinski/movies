import { test, expect } from '@playwright/test';
import { waitForCards } from './helpers';

// Filmy (/) and Kina (/kina) switch IN PLACE — clicking a tab (or, on phones,
// swiping) slides between the two views without a full page navigation. The
// shell (navbar/modals/shared.js) persists; only `#view-root` is fetched and
// re-rendered. See shared.js `navigateTo`. These tests require the new
// server-rendered structure (`#view-pager`/`#view-root`), so they run against
// the local fixture server / a freshly-built app, not the deployed site.

const isKina  = (page) => page.evaluate(() => document.getElementById('view-root')?.dataset.view);

test.describe('Filmy ↔ Kina slide-swap (click)', () => {
  // Engine-independent behaviour — one DPR-1 desktop project is enough and
  // keeps assertions deterministic (mobile-emulation projects are exercised by
  // the swipe block below).
  test.beforeEach(({}, testInfo) => {
    test.skip(testInfo.project.name !== 'chrome-desktop',
      'in-place swap is engine-independent — chrome-desktop is enough');
  });

  test('clicking Kina swaps in place — no full reload, DOM + tab + URL update', async ({ page }) => {
    await page.goto('/');
    await waitForCards(page);
    // Sentinel: a full navigation wipes window globals; an in-place swap keeps it.
    await page.evaluate(() => { window.__noReload = 'kept'; });

    await page.locator('.navbar .nav-tab', { hasText: 'Kina' }).click();
    await page.waitForURL(/\/kina$/);
    await page.waitForSelector('#cinema-pills .cinema-pill', { state: 'attached' });

    expect(await page.evaluate(() => window.__noReload)).toBe('kept');           // no reload
    expect(await isKina(page)).toBe('kina');                                     // view flipped
    await expect(page.locator('.navbar .nav-tab.active')).toContainText('Kina'); // tab highlighted
    await expect(page.locator('#film-grid .cinema-section')).not.toHaveCount(0); // cinema-grouped grid
    // Filtry's cinema section is stashed out of the DOM on Kina.
    await expect(page.locator('#filtry-cinema-section')).toHaveCount(0);
    // The cinema-pill CSS lives in _sharedStyles (not Kina's <head>), so the
    // swapped-in pills are actually styled — guards the "pills unstyled after
    // swap" regression. `.cinema-pill { border-radius: 6px }` is shared-only.
    const pillStyle = await page.evaluate(() => {
      const cs = getComputedStyle(document.querySelector('#cinema-pills .cinema-pill'));
      return { radius: cs.borderRadius, cursor: cs.cursor };
    });
    expect(pillStyle.radius).toBe('6px');
    expect(pillStyle.cursor).toBe('pointer');
  });

  test('clicking Filmy swaps back to the film grid', async ({ page }) => {
    await page.goto('/kina');
    await waitForCards(page);
    await page.locator('.navbar .nav-tab', { hasText: 'Filmy' }).click();
    await page.waitForURL((u) => new URL(u).pathname === '/');
    await waitForCards(page);

    expect(await isKina(page)).toBe('films');
    await expect(page.locator('.navbar .nav-tab.active')).toContainText('Filmy');
    await expect(page.locator('#film-grid > .col[data-title]')).not.toHaveCount(0);
    await expect(page.locator('#film-grid .cinema-section')).toHaveCount(0);
    await expect(page.locator('#cinema-pills')).toHaveCount(0);     // pill strip gone with Kina
    await expect(page.locator('#filtry-cinema-section')).toHaveCount(1); // Filtry section restored
    // The `.cinema-label` CSS lives in _sharedStyles (not repertoire's <head>),
    // so the swapped-in film cards' cinema labels are styled — guards the
    // "cinema name unstyled after swap" regression (#66aadd = rgb(102,170,221)).
    const label = page.locator('.cinema-label').first();
    if (await label.count()) {
      await expect(label).toHaveCSS('color', 'rgb(102, 170, 221)');
    }
  });

  test('no id collisions survive a swap', async ({ page }) => {
    await page.goto('/');
    await waitForCards(page);
    await page.locator('.navbar .nav-tab', { hasText: 'Kina' }).click();
    await page.waitForURL(/\/kina$/);
    await page.waitForSelector('#cinema-pills .cinema-pill', { state: 'attached' });
    // After the outgoing view-root is removed, exactly one of each shared id.
    await expect(page.locator('#view-root')).toHaveCount(1);
    await expect(page.locator('#film-grid')).toHaveCount(1);
    await expect(page.locator('#film-counter')).toHaveCount(1);
    await expect(page.locator('#no-films')).toHaveCount(1);
  });

  test('back button slides back to Filmy and re-highlights it', async ({ page }) => {
    await page.goto('/');
    await waitForCards(page);
    await page.locator('.navbar .nav-tab', { hasText: 'Kina' }).click();
    await page.waitForURL(/\/kina$/);
    await page.waitForSelector('#cinema-pills .cinema-pill', { state: 'attached' });
    await expect(page.locator('#view-pager')).not.toHaveClass(/view-sliding/); // let swap 1 settle

    await page.goBack();
    await page.waitForURL((u) => new URL(u).pathname === '/');
    // popstate kicks off an async fetch-and-swap — wait for it to settle
    // before asserting (the URL flips before the DOM does).
    await expect(page.locator('#view-root')).toHaveAttribute('data-view', 'films');
    await expect(page.locator('.navbar .nav-tab.active')).toContainText('Filmy');
  });

  test('the cards area keeps pinch-zoom enabled (only horizontal pan is reserved for the swipe)', async ({ page }) => {
    await page.goto('/');
    await waitForCards(page);
    const ta = await page.evaluate(() =>
      getComputedStyle(document.querySelector('#view-pager .container-fluid')).touchAction);
    expect(ta).toContain('pinch-zoom');  // two-finger pinch-to-zoom must still work
    expect(ta).toContain('pan-y');       // vertical scroll still works
    expect(ta).not.toContain('pan-x');   // horizontal stays reserved for swipe-to-switch
  });

  test('reduced-motion still completes the swap (no transition to wait on)', async ({ page }) => {
    await page.emulateMedia({ reducedMotion: 'reduce' });
    await page.goto('/');
    await waitForCards(page);
    await page.locator('.navbar .nav-tab', { hasText: 'Kina' }).click();
    await page.waitForURL(/\/kina$/);
    await page.waitForSelector('#cinema-pills .cinema-pill', { state: 'attached' });
    expect(await isKina(page)).toBe('kina');
  });
});

test.describe('Filmy ↔ Kina slide-swap (swipe)', () => {
  // Swipe-to-switch is gated to coarse pointers (phones). The swipe is driven
  // with REAL touch events via CDP (not synthetic PointerEvents), so it goes
  // through the browser's `touch-action` / scroll-vs-gesture arbitration — the
  // exact path that made the gesture fail before `touch-action: pan-y`. CDP
  // touch injection is chromium-only, so gate to a coarse-pointer chromium
  // project.
  test.beforeEach(async ({ page, browserName }) => {
    await page.goto('/');
    const coarse = await page.evaluate(() => matchMedia('(pointer: coarse)').matches);
    test.skip(!coarse || browserName !== 'chromium',
      'real-touch swipe needs a coarse-pointer chromium project (CDP touch)');
  });

  // Drag a real horizontal touch of `dx` px starting on `selector`, via CDP so
  // it exercises touch-action exactly like a finger would.
  async function swipe(page, selector, dx) {
    const box = await page.locator(selector).first().boundingBox();
    const y = box.y + Math.min(40, box.height / 2);
    const x0 = box.x + box.width / 2;
    const client = await page.context().newCDPSession(page);
    await client.send('Input.dispatchTouchEvent', { type: 'touchStart', touchPoints: [{ x: x0, y }] });
    for (let i = 1; i <= 6; i++) {
      await client.send('Input.dispatchTouchEvent',
        { type: 'touchMove', touchPoints: [{ x: x0 + (dx * i) / 6, y }] });
    }
    await client.send('Input.dispatchTouchEvent', { type: 'touchEnd', touchPoints: [] });
    await client.detach();
  }

  test('swipe left switches Filmy → Kina and highlights Kina', async ({ page }) => {
    await waitForCards(page);
    await swipe(page, '#film-grid', -200);
    await page.waitForURL(/\/kina$/);
    expect(await isKina(page)).toBe('kina');
    await expect(page.locator('.navbar .nav-tab.active')).toContainText('Kina');
  });

  test('swipe right switches Kina → Filmy and highlights Filmy', async ({ page }) => {
    await page.goto('/kina');
    await waitForCards(page);
    await swipe(page, '#film-grid', 200);
    await page.waitForURL((u) => new URL(u).pathname === '/');
    expect(await isKina(page)).toBe('films');
    await expect(page.locator('.navbar .nav-tab.active')).toContainText('Filmy');
  });

  // Regression: tapping a tab leaves a sticky `:hover` on touch devices (the
  // browser parks hover on the last-tapped element until you tap elsewhere).
  // If `.nav-tab:hover` paints the same highlight as `.nav-tab.active`, a
  // subsequent swipe back lights up BOTH tabs — the freshly-active one AND the
  // still-hovered old one. Hover styling must be gated to real hover devices.
  test('a swipe after tapping a tab leaves exactly one tab highlighted', async ({ page }) => {
    await waitForCards(page);
    const HIGHLIGHT = 'rgb(58, 58, 110)';   // #3a3a6e — the .active / :hover fill
    // Only the two view tabs — the login pill carries #3a3a6e as its base fill.
    const litTabs = () => page.evaluate((hl) =>
      [...document.querySelectorAll('.navbar a.nav-tab')]
        .filter((a) => ['/', '/kina'].includes(a.getAttribute('href')))
        .filter((a) => getComputedStyle(a).backgroundColor === hl)
        .map((a) => a.textContent.trim()), HIGHLIGHT);

    // Land on Kina, then park the pointer on its tab — the same sticky-hover
    // state a phone leaves behind after a tap.
    await page.goto('/kina');
    await waitForCards(page);
    await page.locator('.navbar .nav-tab', { hasText: 'Kina' }).hover();
    // Swipe back to Filmy. The pointer never leaves the Kina tab.
    await swipe(page, '#film-grid', 200);
    await page.waitForURL((u) => new URL(u).pathname === '/');
    await expect(page.locator('.navbar .nav-tab.active')).toContainText('Filmy');

    expect(await litTabs()).toEqual(['Filmy']);
  });

  test('the view tracks the finger mid-drag (real pager, not just a thresholded swipe)', async ({ page }) => {
    await waitForCards(page);
    // Warm both prefetch caches deterministically: a tab click runs navigateTo
    // which awaits the fetch, so after a Kina→Filmy round-trip both siblings
    // are cached and a drag will engage live tracking (not the cold fallback).
    await page.locator('.navbar .nav-tab', { hasText: 'Kina' }).click();
    await page.waitForURL(/\/kina$/);
    await page.locator('.navbar .nav-tab', { hasText: 'Filmy' }).click();
    await page.waitForURL((u) => new URL(u).pathname === '/');
    await expect(page.locator('#view-pager > main')).toHaveCount(1);

    const box = await page.locator('#film-grid').boundingBox();
    const y = box.y + Math.min(40, box.height / 2);
    const x0 = box.x + box.width * 0.6;
    const client = await page.context().newCDPSession(page);
    await client.send('Input.dispatchTouchEvent', { type: 'touchStart', touchPoints: [{ x: x0, y }] });
    // Small move locks the axis + begins tracking; a second move drags partway
    // (well under the ~35% commit threshold).
    for (const x of [x0 - 20, x0 - 90]) {
      await client.send('Input.dispatchTouchEvent', { type: 'touchMove', touchPoints: [{ x, y }] });
    }

    const mid = await page.evaluate(() => {
      const panels = document.querySelectorAll('#view-pager > main');
      const live = document.getElementById('view-root');
      const outgoing = [...panels].find((p) => p !== live);
      return {
        panels: panels.length,
        liveView: live && live.dataset.view,
        outgoingMoved: outgoing ? getComputedStyle(outgoing).transform !== 'none' : false,
      };
    });
    expect(mid.panels).toBe(2);            // both views mounted during the drag
    expect(mid.liveView).toBe('kina');     // the incoming view is live + tracking
    expect(mid.outgoingMoved).toBe(true);  // the outgoing view follows the finger

    await client.send('Input.dispatchTouchEvent', { type: 'touchEnd', touchPoints: [] });
    await client.detach();
    // Settles back to a single live view (snap-back or commit — either is fine).
    await expect(page.locator('#view-pager > main')).toHaveCount(1);
  });

  test('the screenings/movies counter does not leak into view while dragging', async ({ page }) => {
    // The "N tytułów · M seansów" counter is hidden on phones (a media-query
    // rule keyed on #film-counter). The swap strips the outgoing view-root's id,
    // which used to drop it out of that rule and flash the count in mid-drag.
    await waitForCards(page);
    // Warm both prefetch caches so the drag engages live tracking.
    await page.locator('.navbar .nav-tab', { hasText: 'Kina' }).click();
    await page.waitForURL(/\/kina$/);
    await page.locator('.navbar .nav-tab', { hasText: 'Filmy' }).click();
    await page.waitForURL((u) => new URL(u).pathname === '/');
    await expect(page.locator('#view-pager > main')).toHaveCount(1);

    // Hidden on mobile at rest (no status line visible on either count).
    await expect(page.locator('#view-pager .grid-status:visible')).toHaveCount(0);

    const box = await page.locator('#film-grid').boundingBox();
    const y = box.y + Math.min(40, box.height / 2);
    const x0 = box.x + box.width * 0.6;
    const client = await page.context().newCDPSession(page);
    await client.send('Input.dispatchTouchEvent', { type: 'touchStart', touchPoints: [{ x: x0, y }] });
    for (const x of [x0 - 20, x0 - 90]) {
      await client.send('Input.dispatchTouchEvent', { type: 'touchMove', touchPoints: [{ x, y }] });
    }

    // Mid-drag: still no status line of EITHER panel shows (the regression was
    // the outgoing counter leaking visible once its id was stripped).
    await expect(page.locator('#view-pager .grid-status:visible')).toHaveCount(0);

    await client.send('Input.dispatchTouchEvent', { type: 'touchEnd', touchPoints: [] });
    await client.detach();
    await expect(page.locator('#view-pager > main')).toHaveCount(1);
    await expect(page.locator('#view-pager .grid-status:visible')).toHaveCount(0);
  });

  test('commit reads the tracked delta, not the (unreliable) pointerup coordinate', async ({ page }) => {
    // On iOS the pointerup from a touch often reports the touchstart point (or
    // 0), so deciding commit from the release coordinate made every drag snap
    // back. The decision must use the delta tracked during pointermove. Driven
    // with synthetic PointerEvents so the release coordinate can be forced
    // stale (CDP touch always carries correct coords and can't reproduce it).
    await waitForCards(page);
    await page.locator('.navbar .nav-tab', { hasText: 'Kina' }).click();
    await page.waitForURL(/\/kina$/);
    await page.locator('.navbar .nav-tab', { hasText: 'Filmy' }).click();
    await page.waitForURL((u) => new URL(u).pathname === '/');
    await expect(page.locator('#view-pager > main')).toHaveCount(1);

    await page.evaluate(() => {
      const el = document.getElementById('film-grid');
      const r = el.getBoundingClientRect();
      const y = r.top + 40;
      const x0 = r.left + r.width * 0.7;
      const ev = (type, x) => el.dispatchEvent(new PointerEvent(type, {
        clientX: x, clientY: y, pointerType: 'touch', pointerId: 1, bubbles: true, cancelable: true,
      }));
      ev('pointerdown', x0);
      for (let i = 1; i <= 6; i++) ev('pointermove', x0 - (200 * i) / 6);  // drag left 200px
      ev('pointerup', x0);   // iOS-style stale release coordinate (back at the start)
    });

    await page.waitForURL(/\/kina$/);
    expect(await page.evaluate(() => document.getElementById('view-root')?.dataset.view)).toBe('kina');
  });

  test('dragging the neighbour in then back to the origin and releasing there stays put', async ({ page }) => {
    // iOS pager: the decision is made on lift by the FINAL position, not by
    // having crossed a threshold at some point mid-drag. Drag Kina well in,
    // then drag all the way back and release at the origin → stay on Filmy.
    await waitForCards(page);
    await page.locator('.navbar .nav-tab', { hasText: 'Kina' }).click();
    await page.waitForURL(/\/kina$/);
    await page.locator('.navbar .nav-tab', { hasText: 'Filmy' }).click();
    await page.waitForURL((u) => new URL(u).pathname === '/');
    await expect(page.locator('#view-pager > main')).toHaveCount(1);

    await page.evaluate(() => {
      const el = document.getElementById('film-grid');
      const r = el.getBoundingClientRect();
      const y = r.top + 40;
      const x0 = r.left + r.width * 0.8;
      const ev = (type, x) => el.dispatchEvent(new PointerEvent(type, {
        clientX: x, clientY: y, pointerType: 'touch', pointerId: 1, bubbles: true, cancelable: true,
      }));
      ev('pointerdown', x0);
      for (let i = 1; i <= 6; i++) ev('pointermove', x0 - (240 * i) / 6);          // drag Kina in (left)
      for (let i = 1; i <= 6; i++) ev('pointermove', x0 - 240 + (240 * i) / 6);    // drag back to origin
      ev('pointerup', x0);                                                          // released at the origin
    });

    await expect(page.locator('#view-pager > main')).toHaveCount(1);   // settled (snapped back)
    expect(await isKina(page)).toBe('films');                          // stayed on Filmy
    await expect(page).toHaveURL((u) => new URL(u).pathname === '/');
  });

  test('the active tab previews the landing page while dragging (past the commit point)', async ({ page }) => {
    // While dragging, the Filmy/Kina highlight should show where a release
    // would land: past ~40% toward Kina lights Kina; dragging back under it
    // lights Filmy again. The pointer is held across evaluate() calls (drag
    // state is module-level), so we can assert mid-gesture.
    await waitForCards(page);
    // Warm caches so the drag engages live tracking.
    await page.locator('.navbar .nav-tab', { hasText: 'Kina' }).click();
    await page.waitForURL(/\/kina$/);
    await page.locator('.navbar .nav-tab', { hasText: 'Filmy' }).click();
    await page.waitForURL((u) => new URL(u).pathname === '/');
    await expect(page.locator('#view-pager > main')).toHaveCount(1);

    const move = (phase) => page.evaluate((phase) => {
      const el = document.getElementById('film-grid');
      const r = el.getBoundingClientRect();
      const y = r.top + 40;
      const x0 = r.left + r.width * 0.85;
      const pe = (type, x) => el.dispatchEvent(new PointerEvent(type, {
        clientX: x, clientY: y, pointerType: 'touch', pointerId: 1, bubbles: true, cancelable: true,
      }));
      if (phase === 'past') {
        pe('pointerdown', x0);
        for (let i = 1; i <= 6; i++) pe('pointermove', x0 - (260 * i) / 6);   // well past 40% toward Kina
      } else if (phase === 'back') {
        for (let i = 1; i <= 6; i++) pe('pointermove', x0 - 260 + (240 * i) / 6); // back near the origin
      } else {
        pe('pointerup', x0 - 20);                                            // release near origin
      }
    }, phase);

    await move('past');
    await expect(page.locator('.navbar .nav-tab.active')).toContainText('Kina');   // previews Kina
    await move('back');
    await expect(page.locator('.navbar .nav-tab.active')).toContainText('Filmy');  // back under → Filmy
    await move('release');
    await expect(page.locator('#view-pager > main')).toHaveCount(1);               // snapped back
    expect(await isKina(page)).toBe('films');
  });

  test('a locked-in horizontal drag claims the touch so the browser cannot scroll-steal it mid-drag', async ({ page }) => {
    // A slow drag let the browser decide the gesture was a vertical scroll,
    // steal it (pointercancel) and snap the page back to the origin without the
    // finger lifting — worst near the start, while the browser is still
    // scroll-detecting. Once a horizontal swipe is locked in, touchmove must be
    // preventDefault-ed so the browser keeps its hands off the gesture.
    await waitForCards(page);
    const prevented = await page.evaluate(() => {
      const el = document.getElementById('film-grid');
      const r = el.getBoundingClientRect();
      const y = r.top + 40;
      const x0 = r.left + r.width * 0.7;
      const pe = (type, x) => el.dispatchEvent(new PointerEvent(type, {
        clientX: x, clientY: y, pointerType: 'touch', pointerId: 1, bubbles: true, cancelable: true,
      }));
      pe('pointerdown', x0);
      pe('pointermove', x0 - 30);
      pe('pointermove', x0 - 60);   // axis locks horizontal
      const touch = new Touch({ identifier: 1, target: el, clientX: x0 - 90, clientY: y });
      const tm = new TouchEvent('touchmove', {
        cancelable: true, bubbles: true, touches: [touch], targetTouches: [touch], changedTouches: [touch],
      });
      el.dispatchEvent(tm);
      const result = tm.defaultPrevented;
      pe('pointerup', x0 - 90);     // cleanup
      return result;
    });
    expect(prevented).toBe(true);
  });

  test('swipe starting on the cinema-pill strip does NOT switch view', async ({ page }) => {
    await page.goto('/kina');
    await waitForCards(page);
    await page.waitForSelector('#cinema-pills .cinema-pill', { state: 'attached' });
    await swipe(page, '#cinema-pills .cinema-pill', -200);
    // Still on Kina — the strip owns the horizontal gesture.
    expect(await isKina(page)).toBe('kina');
    await expect(page).toHaveURL(/\/kina$/);
  });
});
