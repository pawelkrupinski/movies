// The finger swipe on the listing's grid: following the finger with the neighbour day's column,
// then sliding on to that day or snapping back when it lifts (the slide itself: `day-carousel.js`).

/* global pageState */  // page-state.js
/* global highlightDayPill, syncDayPills */  // dates.js
/* global armTrack, commitDay, dayTrack, dismissSwipeHint, neighborDay, pagerWidth, retireSwipeHint, setTrack,
   slideArmedTo, slideInFlight, swipeAnimMs, unmountNeighbors, usesInstantDayChange */  // day-carousel.js

// Swipe-gesture tuning.
const COMMIT_FRACTION   = 0.4;   // drag past this fraction of the width → commit
const FLICK_VX          = 0.4;   // px/ms — a quick flick commits a shorter drag
const FLICK_MIN_PX      = 24;    // ignore micro-flicks
const SWIPE_DEADZONE_PX = 10;    // horizontal travel before we lock to a swipe
// Axis lock is biased toward HORIZONTAL so a swipe that starts with a little
// vertical jitter isn't misread as a scroll and killed. We concede to vertical
// scrolling only when it CLEARLY dominates.
const SWIPE_VBAIL_PX    = 16;    // vertical must travel at least this far to even consider bailing
const SWIPE_VBIAS_RATIO = 1.6;   // …and beat horizontal by this factor → it's a real vertical scroll


// While a finger drag is in flight, move the day-pill highlight to the day a
// release RIGHT NOW would land on: past the commit boundary → the neighbour
// we'd swipe to (finger left → next, right → previous), otherwise back to the
// current day. Distance-only, matching the boundary the finger feels — the
// grid's `#date-filter` stays put until the swipe actually commits, so this is
// a pure preview of the pending decision. Mirrors the old filmy/kina tab
// swipe, where the tab indicator flipped the moment you crossed the snap line.
function previewDayPillForDrag(dx) {
  const sel = document.getElementById('date-filter');
  if (!sel) return;
  const past = Math.abs(dx) > pagerWidth() * COMMIT_FRACTION;
  highlightDayPill(past ? (neighborDay(dx < 0 ? 1 : -1) || sel.value) : sel.value);
}

// Swipe commit: 'left' = finger went left → next day (dir +1). The previous/next
// clones are already mounted by the drag (`armTrack` ran at axis-lock), so we
// slide the existing armed track from the live offset.
function commitDaySwipe(dir, fromPx) {
  const sel = document.getElementById('date-filter');
  if (!sel) return;
  retireSwipeHint();   // a committed swipe means they've found the gesture
  const dayDir = dir === 'left' ? 1 : -1;   // swipe-left → next day
  const target = neighborDay(dayDir);
  if (target == null) { unmountNeighbors(); return; }
  highlightDayPill(target);   // keep the pill on the target through the slide (covers a sub-threshold flick)
  if (usesInstantDayChange() || matchMedia('(prefers-reduced-motion: reduce)').matches) {
    commitDay(target);
    return;
  }
  slideArmedTo(dayDir, target, fromPx);
}

// A drag that didn't commit: ease the armed track back to its parked centre,
// then tear the clones down. Scroll is left untouched (no committed change).
function snapBack() {
  const track = dayTrack();
  if (!track) return;
  syncDayPills();   // the drag may have previewed a neighbour — return the highlight to the real day
  if (usesInstantDayChange()) { unmountNeighbors(); return; }   // nothing was armed/moved — just clear drag state
  const ms = swipeAnimMs();
  track.style.transition = 'transform ' + ms + 'ms ease';
  setTrack(0);
  let done = false;
  const clear = (event) => {
    if (done || (event && event.target !== track)) return;   // a card's own, as in `slideArmedTo`
    done = true;
    track.removeEventListener('transitionend', clear);
    unmountNeighbors();
  };
  track.addEventListener('transitionend', clear);
  setTimeout(clear, ms + 60);
}

document.addEventListener('pointerdown', (e) => {
  if (e.pointerType === 'mouse' || slideInFlight()) return;
  if (!matchMedia('(pointer: coarse)').matches) return;
  if (!document.getElementById('view-root')) return;     // not the listing grid
  if (!document.getElementById('date-filter')) return;   // no day axis to step
  pageState.drag = { x0: e.clientX, y0: e.clientY, axis: null, lastDx: 0, vx: 0, lastT: e.timeStamp };
}, { passive: true });

// Claim a single-finger HORIZONTAL drag so the browser can't decide mid-drag
// that it's a vertical scroll, steal the gesture (pointercancel) and snap the
// page back without the finger lifting. NON-passive so `preventDefault` is
// allowed; it fires only for a horizontal-leaning single touch, so vertical
// scroll and two-finger pinch-zoom still pass through untouched.
document.addEventListener('touchmove', (e) => {
  if (!pageState.drag || !e.cancelable || e.touches.length !== 1) return;
  if (pageState.drag.axis === 'x') { e.preventDefault(); return; }   // already a swipe — keep it
  const tdx = Math.abs(e.touches[0].clientX - pageState.drag.x0);
  const tdy = Math.abs(e.touches[0].clientY - pageState.drag.y0);
  if (tdy >= SWIPE_VBAIL_PX && tdy > tdx * SWIPE_VBIAS_RATIO) return;
  if (tdx > tdy) e.preventDefault();
}, { passive: false });

document.addEventListener('pointermove', (e) => {
  if (!pageState.drag) return;
  const dx = e.clientX - pageState.drag.x0, dy = e.clientY - pageState.drag.y0;
  if (pageState.drag.axis === null) lockDragAxis(dx, dy);
  if (!pageState.drag || pageState.drag.axis !== 'x') return;
  // Track the latest delta + a smoothed velocity HERE — `pointerup`'s clientX
  // is unreliable on touch (iOS often reports the touchstart point or 0), so
  // the release decision reads `lastDx`/`vx`, never the pointerup coordinate.
  const dt = Math.max(1, e.timeStamp - pageState.drag.lastT);
  pageState.drag.vx = (dx - pageState.drag.lastDx) / dt;   // px per ms
  pageState.drag.lastDx = dx;
  pageState.drag.lastT = e.timeStamp;
  // The track follows the finger 1:1 (on top of the parked -100vw), revealing
  // the neighbour day's column from whichever edge the finger pulls in; the
  // pill highlight previews the day a release would land on. A large city has
  // no neighbour mounted to reveal, so it skips the track transform and just
  // keeps the pill preview — the grid itself changes only on commit.
  if (pageState.drag.armed) {
    if (!usesInstantDayChange()) setTrack(dx);
    previewDayPillForDrag(dx);
  }
}, { passive: true });

// A drag not yet locked to an axis, moved by (dx, dy): a clear vertical scroll
// yields to the browser (the drag is dropped); a clear horizontal one locks the
// swipe; anything else stays ambiguous until the next move.
function lockDragAxis(dx, dy) {
  const adx = Math.abs(dx), ady = Math.abs(dy);
  // Clear vertical scroll — enough vertical travel AND vertical dominates → yield to the browser.
  if (ady >= SWIPE_VBAIL_PX && ady > adx * SWIPE_VBIAS_RATIO) { pageState.drag = null; return; }
  // Horizontal intent — past the deadzone and at least as horizontal as vertical → lock the swipe.
  if (adx < SWIPE_DEADZONE_PX || adx < ady) return;
  pageState.drag.axis = 'x';
  dismissSwipeHint();                    // they're swiping — get the nudge out of the way
  // Large cities skip the neighbour-clone preview (see `usesInstantDayChange`)
  // — nothing to mount, but the gesture is still live and can still commit.
  pageState.drag.armed = usesInstantDayChange()
    // Mount BOTH neighbour columns so either drag direction reveals the
    // right day from the screen edge, parked at -100vw. The day ring wraps,
    // so every direction has a destination.
    ? true
    : armTrack(neighborDay(-1), neighborDay(1));
}

// Decision happens ONLY when the finger lifts (or the gesture cancels): step
// the day if the drag ended past ~40% of the width OR left with a quick flick;
// otherwise snap back. Reads the tracked `lastDx`/`vx`.
function endDrag() {
  const drag = pageState.drag;
  pageState.drag = null;
  if (!drag || drag.axis !== 'x') return;
  if (!drag.armed) { unmountNeighbors(); return; }   // arming failed — just clean up
  const dx    = drag.lastDx;
  const w     = pagerWidth();
  const flick = Math.abs(drag.vx) > FLICK_VX && Math.abs(dx) > FLICK_MIN_PX;
  if (Math.abs(dx) > w * COMMIT_FRACTION || flick) commitDaySwipe(dx < 0 ? 'left' : 'right', dx);
  else snapBack();
}
document.addEventListener('pointerup', endDrag, { passive: true });
document.addEventListener('pointercancel', endDrag, { passive: true });
