// Changing the listing's day: the arrow keys, a pill, the hidden select and a swipe all slide the
// grid there through one carousel, with the first-run swipe hint.

/* global readStoredFlag, storeFlag */  // local-store.js
/* global noteDayChange */  // search.js
/* global undoTruncation */  // showings.js
/* global highlightDayPill, pageToday, syncDayPills */  // dates.js
/* exported maybeShowSwipeHint, onDateChange, onDateSelect, rememberDisplayedDay, stepDateWrap */

// ── Keyboard navigation ───────────────────────────────────────────────────

document.addEventListener('keydown', e => {
  if (e.target.tagName === 'INPUT' || e.target.tagName === 'SELECT' || e.target.tagName === 'TEXTAREA') return;
  if (e.key === 'ArrowLeft')  stepDate(-1);
  if (e.key === 'ArrowRight') stepDate(1);
});

// The day the centre grid is currently rendering. Tracked separately from the
// `#date-filter` value because the carousel moves the dropdown to the target
// up front (at slide start) while the centre grid keeps showing the old day
// until the slide commits — `animateToDay` derives the slide direction from
// THIS, not from the dropdown's already-moved index.
let _appliedDay = null;

// The day at the HEAD of the user's intent: the day the latest key-press /
// pick is heading toward, which can sit ahead of `_appliedDay` while a slide
// is still in flight (or while follow-on steps are queued). A keyboard step
// advances from THIS, so a second arrow press stacked on an unfinished slide
// moves a further day instead of recomputing the same one. Resync'd to
// `_appliedDay` whenever the carousel finally settles with nothing queued.
let _headDay = null;

// The day the boot's first filter pass draws: whatever `#date-filter` says once the URL is applied.
function rememberDisplayedDay() {
  const sel = document.getElementById('date-filter');
  if (sel) _appliedDay = sel.value;
}

// The Left/Right keys: step one day and slide there like a swipe. Clamps at
// the ends (no wrap) — same reach the dropdown gives.
function stepDate(dir) {
  const sel  = document.getElementById('date-filter');
  if (!sel) return;
  const ring = dayRing();
  if (ring.length === 0) return;
  // Step from the day we're HEADING to (the in-flight / queued destination),
  // not the day currently rendered — so a second arrow press fired before the
  // first slide settles advances a further day rather than recomputing the
  // same one (the press used to be dropped by the `_animating` guard). Falls
  // back to the displayed day, then the dropdown value, when idle.
  const base = _headDay   != null ? _headDay
             : _appliedDay != null ? _appliedDay
             : sel.value;
  const from = ring.indexOf(base);
  if (from < 0) return;
  const next = from + dir;
  if (next < 0 || next >= ring.length) return;   // clamp at the ends — no wrap
  const targetValue = ring[next];
  // If a day pill currently holds focus (a prior mouse tap), carry that focus
  // to the destination pill before sliding. A keyboard step flips the browser's
  // focus-visible heuristic on, so leaving focus on the old pill paints its
  // `:focus-visible` border there while the `.active` background moves to the
  // new day — the old pill looks "stuck" with a border but no fill.
  const focused = document.activeElement;
  if (focused && focused.classList && focused.classList.contains('day-pill')) {
    const dest = [...document.querySelectorAll('#day-pills .day-pill')]
      .find(p => p.dataset.day === targetValue);
    if (dest) dest.focus();
  }
  animateToDay(targetValue, dir);
}

// The `#date-filter` dropdown changed — slide to the chosen day (one slide
// toward the index delta, even for a multi-step jump). The select has already
// moved to the new value by the time `onchange` fires; `animateToDay` reads
// the still-displayed day from `_appliedDay`.
//
// Unlike the arrow/keyboard/swipe paths (which wrap and want the shorter way
// round the ring), a dropdown pick is a LINEAR list choice: the slide enters
// from the right when the target sits after the current option in list order,
// from the left when it sits before. Derive that linear `dir` from the same
// ordered ring `animateToDay` uses and pass it explicitly; fall back to the
// wrap-shortest default (null) only when either day isn't in the list.
function onDateSelect() {
  const sel = document.getElementById('date-filter');
  if (!sel) return;
  const ring    = dayRing();
  const fromIdx = ring.indexOf(_appliedDay);
  const toIdx   = ring.indexOf(sel.value);
  const dir = (fromIdx < 0 || toIdx < 0) ? null : (toIdx > fromIdx ? 1 : -1);
  animateToDay(sel.value, dir);
}

// "Apply the current day NOW" — no animation. Rebuilds the visible grid the
// same way every other filter does (`applyFilters` also calls `syncDateToURL`,
// date-only) and records the displayed day. The carousel's `commitDay`, the
// wrap helper, and reduced-motion paths funnel through here after they've set
// the dropdown to the target.
function onDateChange() {
  const sel = document.getElementById('date-filter');
  if (sel) {
    noteDayChange(_appliedDay, sel.value);
    _appliedDay = sel.value;
  }
  syncDayPills();
  applyFilters();
}

// ── Day carousel: swipe / arrows / keys / dropdown all slide ────────────────
//
// The films grid is the centre column of a three-column carousel
// (previous | current | next). The neighbouring columns are CLONES of `#film-grid`
// filtered to their day (honouring every other active filter), mounted into
// `#day-track` only while a slide is in flight. A horizontal swipe translates
// the track 1:1 with the finger so the neighbour day is revealed from the
// screen edge; on release past ~40% of the width (or a quick flick) the track
// slides the rest of the way, the centre grid's day is committed, the page
// scrolls to top, and the clones are removed. A shorter drag snaps back and
// leaves scroll untouched.
//
// The SAME slide animation backs the arrow buttons, the Left/Right keys, and
// the `#date-filter` dropdown — all four route through `animateToDay`. Because
// the three columns are normal block flow in one flex row (no inner overflow),
// they share the page's single vertical scroll, so a revealed neighbour lines
// up at the same `scrollY` offset for free. Listeners are passive except the
// `touchmove` claim that holds the browser off its scroll-vs-gesture arbitration.

// Swipe-gesture tuning.
const COMMIT_FRACTION   = 0.4;   // drag past this fraction of the width → commit
const FLICK_VX          = 0.4;   // px/ms — a quick flick commits a shorter drag
const FLICK_MIN_PX      = 24;    // ignore micro-flicks
const SWIPE_DEADZONE_PX = 10;    // horizontal travel before we lock to a swipe
const SWIPE_ANIM_MS     = 220;   // base slide-out / slide-in duration (touch/mobile)
// Desktop (fine pointer) takes a longer, more deliberate glide — 2.5× the base —
// while touch/mobile keeps the snappy base. A finger-flick wants an immediate
// response; a mouse-driven arrow / keyboard / dropdown step reads better slower.
const DESKTOP_ANIM_FACTOR = 2.5;
function swipeAnimMs() {
  return matchMedia('(pointer: coarse)').matches
    ? SWIPE_ANIM_MS
    : Math.round(SWIPE_ANIM_MS * DESKTOP_ANIM_FACTOR);
}
// Axis lock is biased toward HORIZONTAL so a swipe that starts with a little
// vertical jitter isn't misread as a scroll and killed. We concede to vertical
// scrolling only when it CLEARLY dominates.
const SWIPE_VBAIL_PX    = 16;    // vertical must travel at least this far to even consider bailing
const SWIPE_VBIAS_RATIO = 1.6;   // …and beat horizontal by this factor → it's a real vertical scroll

let _drag = null;
let _animating = false;   // guards re-entrancy while a commit animation runs
let _queuedDay = null;    // a day-step requested mid-slide → run as a follow-on slide on commit

// A large city (`MovieControllerService.LargeCityShowtimeThreshold`, stamped
// as `data-large-city` on `#view-root`) ships every showtime for every day
// in one page load. On a phone, the carousel's clone-and-slide preview
// (`buildDayColumn`, below) clones that WHOLE tree — up to two clones per
// swipe, at gesture start — which is cheap for a normal city but visibly
// stutters once a city's showtime count runs into the tens of thousands
// (Salt Lake City: ~17.5k DOM nodes per clone). These cities skip the
// clone/slide machinery on touch entirely and re-filter the LIVE grid in
// place instead — the same `applyFilters` pass every day-change ends with
// anyway, just with no preview and no travel. Desktop is unaffected: it
// never enters the touch gesture path, and pill/dropdown/keyboard clicks
// there still animate (see `animateToDay`'s own check, mirrored here so
// pill click and swipe agree).
function usesInstantDayChange() {
  const root = document.getElementById('view-root');
  return !!root && root.dataset.largeCity === 'true' && matchMedia('(pointer: coarse)').matches;
}

// Step the day dropdown by `dir` (+1 = next day, -1 = previous), WRAPPING
// around its full option list, then re-render via the normal date-change path
// (`onDateChange` → `applyFilters` → `syncDateToURL`). Exposed for the swipe
// gesture and unit tests.
function stepDateWrap(direction) {
  const sel = document.getElementById('date-filter');
  if (!sel || sel.options.length === 0) return;
  const optionCount = sel.options.length;
  sel.selectedIndex = ((sel.selectedIndex + direction) % optionCount + optionCount) % optionCount;
  onDateChange();
}

// ── First-run swipe hint ────────────────────────────────────────────────────
// Reveal "Przesuń, aby zmienić dzień" once per calendar day on a touch device,
// until the first real swipe retires it for good — the same rule the iOS and
// Android apps follow. Device-local (plain localStorage, NOT the server-synced
// store): it's a per-device onboarding nudge, not user state worth syncing.
const SWIPE_HINT_DAY  = 'kinowoSwipeHintDay';   // last calendar day (Warsaw) it was shown
const SWIPE_HINT_DONE = 'kinowoSwipeHintDone';  // set on the first swipe → never show again
const SWIPE_HINT_MS   = 3000;                   // auto-hide after this long if untouched
let _swipeHintTimer = null;

// Reveal the hint if this is a touch device, the user hasn't swiped before,
// and it hasn't already shown today. Called once at boot.
function maybeShowSwipeHint() {
  if (!matchMedia('(pointer: coarse)').matches) return;   // desktop never sees it
  const element = document.getElementById('swipe-hint');
  if (!element) return;                                        // not the listing page
  if (readStoredFlag(SWIPE_HINT_DONE)) return;                 // retired by a past swipe
  const today = pageToday();                            // the city's day, see dateBounds
  if (readStoredFlag(SWIPE_HINT_DAY) === today) return;        // already shown today
  storeFlag(SWIPE_HINT_DAY, today);
  element.classList.add('visible');
  clearTimeout(_swipeHintTimer);
  _swipeHintTimer = setTimeout(() => element.classList.remove('visible'), SWIPE_HINT_MS);
}

// Hide the hint without retiring it (the user has started a horizontal drag).
function dismissSwipeHint() {
  clearTimeout(_swipeHintTimer);
  document.getElementById('swipe-hint')?.classList.remove('visible');
}

// Retire the hint for good (the user committed a day-swipe — they've got it).
function retireSwipeHint() {
  dismissSwipeHint();
  storeFlag(SWIPE_HINT_DONE, '1');
}

// ── Carousel track plumbing ────────────────────────────────────────────────

function dayTrack() { return document.getElementById('day-track'); }
function pagerWidth() {
  const pager = document.getElementById('view-pager');
  return (pager && pager.offsetWidth) || window.innerWidth;
}

// Ordered list of `#date-filter` option values — the day ring the carousel and
// every entry point step through (wrap-around). Empty when there's no selector.
function dayRing() {
  const sel = document.getElementById('date-filter');
  if (!sel) return [];
  return [...sel.options].map(o => o.value);
}

// The day value `dir` steps from the current selection, wrapping the ring.
// dir = +1 → next day, -1 → previous.
function neighborDay(dir) {
  const sel = document.getElementById('date-filter');
  const ring = dayRing();
  if (!sel || ring.length === 0) return null;
  const ringLength = ring.length;
  return ring[((sel.selectedIndex + dir) % ringLength + ringLength) % ringLength];
}

// Build a carousel column: a `.day-col` wrapper around a clone of `#film-grid`
// filtered to `dayValue` (honouring all other active filters via the view's
// `applyFiltersForDay`). Returns null when there's no grid to clone.
function buildDayColumn(dayValue) {
  const grid = document.getElementById('film-grid');
  if (!grid) return null;
  const col = document.createElement('div');
  col.className = 'day-col';
  col.setAttribute('aria-hidden', 'true');
  const clone = grid.cloneNode(true);
  clone.removeAttribute('id');   // keep `#film-grid` unique to the real centre
  // The clone is a transient preview — strip ids so nothing inside collides
  // with the live DOM the filter helpers query by id.
  clone.querySelectorAll('[id]').forEach(element => element.removeAttribute('id'));
  // The live grid's folded-away rows come along in the clone. A preview is
  // untruncated (it shows one day, not ten), and `applyFiltersForDay` only
  // speaks inline display — so drop the class rather than leave rows hidden
  // by a decision that was made for a different day's budget.
  undoTruncation(clone);
  col.appendChild(clone);
  if (typeof applyFiltersForDay === 'function') applyFiltersForDay(clone, dayValue);
  return col;
}

// Arm the track: park it at -100vw with the centre `#view-root` flanked by a
// previous (left) and next (right) column for the given day values. A missing
// value (e.g. a directed slide that only needs one side) leaves that flank as
// a spacer so the centre stays centred. Re-arming first tears down any prior
// clones so we never stack columns.
function armTrack(previousDay, nextDay) {
  const track = dayTrack();
  const root  = document.getElementById('view-root');
  if (!track || !root) return false;
  unmountNeighbors();
  const previous = (previousDay != null ? buildDayColumn(previousDay) : null) || spacerColumn();
  const next = (nextDay != null ? buildDayColumn(nextDay) : null) || spacerColumn();
  track.insertBefore(previous, root);
  track.appendChild(next);
  track.classList.add('day-track--armed');
  track.style.transition = 'none';
  setTrack(0);
  // Force a reflow so a following transition animates from the parked
  // position rather than jumping.
  void track.offsetWidth;
  return true;
}

function spacerColumn() {
  const col = document.createElement('div');
  col.className = 'day-col';
  col.setAttribute('aria-hidden', 'true');
  return col;
}

// Remove the previous/next clones and disarm the track, returning to the resting
// single-column layout.
function unmountNeighbors() {
  const track = dayTrack();
  if (!track) return;
  track.querySelectorAll(':scope > .day-col').forEach(c => c.remove());
  track.classList.remove('day-track--armed');
  track.style.transition = '';
  track.style.transform  = '';
}

// Live drag offset while armed: parked at -100vw, plus the finger delta.
function setTrack(dx) {
  const track = dayTrack();
  if (track) track.style.transform = 'translateX(calc(-100vw + ' + dx + 'px))';
}

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

// ── Unified day-change slide ────────────────────────────────────────────────

// Commit the day change once a slide has carried the target column into view:
// set the dropdown, fire the normal date-change render, scroll to top (the
// committed-change behaviour), then tear the clones down.
function commitDay(targetValue) {
  const sel = document.getElementById('date-filter');
  if (sel) { sel.value = targetValue; onDateChange(); }   // sets `_appliedDay = targetValue`, re-syncs pills
  window.scrollTo(0, 0);
  unmountNeighbors();
  _animating = false;
  // A further day-step was requested mid-slide? Continue toward it now — one
  // follow-on slide from the just-committed day to the newest requested day —
  // so a rapid double-press advances twice instead of dropping the second
  // press. `onDateChange` just reset the pill to the intermediate day, so
  // re-flip it to the destination (synchronous → no flicker).
  if (_queuedDay != null && _queuedDay !== _appliedDay) {
    const next = _queuedDay;
    _queuedDay = null;
    highlightDayPill(next);
    const ring = dayRing();
    const from = ring.indexOf(_appliedDay);
    const to   = ring.indexOf(next);
    const dir  = (from < 0 || to < 0) ? 1 : (to >= from ? 1 : -1);
    runSlide(next, dir);
    return;
  }
  _queuedDay = null;
  _headDay = _appliedDay;
}

// Animate the armed track to the dir side and commit `targetValue` when it
// settles. `dir` = +1 (slide to the next/right column) or -1 (previous/left).
// `fromPx` continues a live drag smoothly from where the finger left off.
function slideArmedTo(dir, targetValue, fromPx) {
  const track = dayTrack();
  if (!track) { commitDay(targetValue); return; }
  const w   = pagerWidth();
  const end = dir > 0 ? -2 * w : 0;   // -200vw reveals next, 0 reveals previous
  const ms  = swipeAnimMs();
  _animating = true;
  track.style.transition = 'none';
  setTrack(fromPx || 0);
  void track.offsetWidth;
  let done = false;
  // `transitionend` bubbles: a card's own transition inside the track (the
  // hover lift, as the grid slides under a resting mouse) is not the slide's.
  const finish = (event) => {
    if (done || (event && event.target !== track)) return;
    done = true;
    track.removeEventListener('transitionend', finish);
    commitDay(targetValue);
  };
  // The frame that starts the slide also arms its commit. A throttled or
  // background tab, or a stalled renderer, can deliver this frame long after
  // `ms`: a fallback timer armed before it would commit first, and the late
  // frame would then translate the bare, committed `#view-root` off-screen —
  // a blank grid. Armed here, the fallback always runs `ms + 60` after the
  // slide actually began. A frame that finds the track already torn down
  // (something else disarmed it meanwhile) just commits in place.
  requestAnimationFrame(() => {
    if (!track.classList.contains('day-track--armed')) { finish(); return; }
    track.style.transition = 'transform ' + ms + 'ms ease';
    track.style.transform  = 'translateX(' + end + 'px)';
    track.addEventListener('transitionend', finish);
    setTimeout(finish, ms + 60);   // fallback if transitionend is missed
  });
}

// Mount the target day's column on the slide-in side and animate the armed
// track to it, committing the day when it settles. Reduced motion skips
// straight to the committed change. The shared tail of every directed slide
// (keyboard/dropdown entry AND the mid-slide queued continuation).
function runSlide(targetValue, dir) {
  if (matchMedia('(prefers-reduced-motion: reduce)').matches) {
    commitDay(targetValue);
    return;
  }
  // Mount only the side we're sliding toward with the TARGET day's grid; the
  // opposite flank is a spacer (we never reveal it on a directed slide).
  const armed = dir > 0 ? armTrack(null, targetValue) : armTrack(targetValue, null);
  if (!armed) { commitDay(targetValue); return; }
  slideArmedTo(dir, targetValue, 0);
}

// THE single entry point every day change funnels through. `targetValue` is a
// `#date-filter` option value; `dir` (optional) forces a slide direction —
// when omitted it's derived from the ring index delta between the CURRENTLY
// DISPLAYED day (`_appliedDay`) and the target (a multi-step dropdown jump
// does ONE slide toward the delta's sign, not an animation through every
// intermediate day). Wrap-around picks the shorter visual direction. Reduced
// motion skips straight to the committed change.
function animateToDay(targetValue, dir) {
  const sel = document.getElementById('date-filter');
  if (!sel) return;
  // Flip the day-pill highlight to the destination IMMEDIATELY, before the
  // slide — a keyboard step or dropdown pick should land the highlight up
  // front and let the grid animate to catch up, rather than holding the old
  // day lit until partway through the travel (the old mid-slide timer). The
  // finger-drag path keeps its own boundary-crossing preview.
  highlightDayPill(targetValue);
  _headDay = targetValue;
  if (targetValue === _appliedDay && !_animating) {   // already showing, nothing in flight
    if (sel.value !== targetValue) { sel.value = targetValue; onDateChange(); }
    _queuedDay = null;
    return;
  }
  retireSwipeHint();   // any deliberate day change means they've got the gesture
  if (usesInstantDayChange()) { commitDay(targetValue); return; }
  if (_animating) {
    // A slide is already running: don't drop this step. Remember the latest
    // destination; the in-flight commit (`commitDay`) continues the slide on
    // to it. Rapid presses collapse to "go to the newest day" in one
    // follow-on slide.
    _queuedDay = targetValue;
    return;
  }
  if (dir == null) {
    const ring = dayRing();
    const from = ring.indexOf(_appliedDay);
    const to   = ring.indexOf(targetValue);
    if (from < 0 || to < 0) { commitDay(targetValue); return; }
    const ringLength = ring.length;
    const forwardSteps = ((to - from) % ringLength + ringLength) % ringLength;   // steps walking forward (right)
    dir = forwardSteps <= ringLength - forwardSteps ? 1 : -1;           // shorter way round the ring
  }
  runSlide(targetValue, dir);
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
  if (e.pointerType === 'mouse' || _animating) return;
  if (!matchMedia('(pointer: coarse)').matches) return;
  if (!document.getElementById('view-root')) return;     // not the listing grid
  if (!document.getElementById('date-filter')) return;   // no day axis to step
  _drag = { x0: e.clientX, y0: e.clientY, axis: null, lastDx: 0, vx: 0, lastT: e.timeStamp };
}, { passive: true });

// Claim a single-finger HORIZONTAL drag so the browser can't decide mid-drag
// that it's a vertical scroll, steal the gesture (pointercancel) and snap the
// page back without the finger lifting. NON-passive so `preventDefault` is
// allowed; it fires only for a horizontal-leaning single touch, so vertical
// scroll and two-finger pinch-zoom still pass through untouched.
document.addEventListener('touchmove', (e) => {
  if (!_drag || !e.cancelable || e.touches.length !== 1) return;
  if (_drag.axis === 'x') { e.preventDefault(); return; }   // already a swipe — keep it
  const tdx = Math.abs(e.touches[0].clientX - _drag.x0);
  const tdy = Math.abs(e.touches[0].clientY - _drag.y0);
  if (tdy >= SWIPE_VBAIL_PX && tdy > tdx * SWIPE_VBIAS_RATIO) return;
  if (tdx > tdy) e.preventDefault();
}, { passive: false });

document.addEventListener('pointermove', (e) => {
  if (!_drag) return;
  const dx = e.clientX - _drag.x0, dy = e.clientY - _drag.y0;
  if (_drag.axis === null) {
    const adx = Math.abs(dx), ady = Math.abs(dy);
    // Clear vertical scroll — enough vertical travel AND vertical dominates → yield to the browser.
    if (ady >= SWIPE_VBAIL_PX && ady > adx * SWIPE_VBIAS_RATIO) { _drag = null; return; }
    // Horizontal intent — past the deadzone and at least as horizontal as vertical → lock the swipe.
    if (adx >= SWIPE_DEADZONE_PX && adx >= ady) {
      _drag.axis = 'x';
      dismissSwipeHint();                    // they're swiping — get the nudge out of the way
      // Large cities skip the neighbour-clone preview (see `usesInstantDayChange`)
      // — nothing to mount, but the gesture is still live and can still commit.
      _drag.armed = usesInstantDayChange()
        // Mount BOTH neighbour columns so either drag direction reveals the
        // right day from the screen edge, parked at -100vw. The day ring wraps,
        // so every direction has a destination.
        ? true
        : armTrack(neighborDay(-1), neighborDay(1));
    }
    // Otherwise still ambiguous → wait for the next move.
    else return;
  }
  if (_drag.axis !== 'x') return;
  // Track the latest delta + a smoothed velocity HERE — `pointerup`'s clientX
  // is unreliable on touch (iOS often reports the touchstart point or 0), so
  // the release decision reads `lastDx`/`vx`, never the pointerup coordinate.
  const dt = Math.max(1, e.timeStamp - _drag.lastT);
  _drag.vx = (dx - _drag.lastDx) / dt;   // px per ms
  _drag.lastDx = dx;
  _drag.lastT = e.timeStamp;
  // The track follows the finger 1:1 (on top of the parked -100vw), revealing
  // the neighbour day's column from whichever edge the finger pulls in; the
  // pill highlight previews the day a release would land on. A large city has
  // no neighbour mounted to reveal, so it skips the track transform and just
  // keeps the pill preview — the grid itself changes only on commit.
  if (_drag.armed) {
    if (!usesInstantDayChange()) setTrack(dx);
    previewDayPillForDrag(dx);
  }
}, { passive: true });

// Decision happens ONLY when the finger lifts (or the gesture cancels): step
// the day if the drag ended past ~40% of the width OR left with a quick flick;
// otherwise snap back. Reads the tracked `lastDx`/`vx`.
function endDrag() {
  const drag = _drag;
  _drag = null;
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
