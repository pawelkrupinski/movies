// Changing the listing's day: the arrow keys, a pill, the hidden select and a swipe (`day-swipe.js`)
// all slide the grid there through one carousel, with the first-run swipe hint.

/* global readStoredFlag, storeFlag */  // local-store.js
/* global noteDayChange */  // search-day.js
/* global undoTruncation */  // showings.js
/* global highlightDayPill, pageToday, syncDayPills */  // dates.js
/* exported armTrack, commitDay, dayTrack, dismissSwipeHint, maybeShowSwipeHint, neighborDay, onDateChange,
   onDateSelect, pagerWidth, pickDay, rememberDisplayedDay, retireSwipeHint, setTrack, slideArmedTo, slideInFlight,
   stepDateWrap, swipeAnimMs, unmountNeighbors, usesInstantDayChange */

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
  const from = ring.indexOf(_headDay ?? _appliedDay ?? sel.value);
  if (from < 0) return;
  const next = from + dir;
  if (next < 0 || next >= ring.length) return;   // clamp at the ends — no wrap
  const targetValue = ring[next];
  carryPillFocusTo(targetValue);
  animateToDay(targetValue, dir);
}

// If a day pill currently holds focus (a prior mouse tap), carry that focus
// to the destination pill before sliding. A keyboard step flips the browser's
// focus-visible heuristic on, so leaving focus on the old pill paints its
// `:focus-visible` border there while the `.active` background moves to the
// new day — the old pill looks "stuck" with a border but no fill.
function carryPillFocusTo(targetValue) {
  const focused = document.activeElement;
  if (!focused || !focused.classList || !focused.classList.contains('day-pill')) return;
  const dest = [...document.querySelectorAll('#day-pills .day-pill')]
    .find(p => p.dataset.day === targetValue);
  if (dest) dest.focus();
}

// A day pill was tapped — move the hidden select to that day and slide there
// exactly like the old dropdown pick (linear direction from the list order).
function pickDay(value) {
  const sel = document.getElementById('date-filter');
  if (!sel || sel.value === value) return;
  sel.value = value;
  syncDayPills();   // highlight immediately; the slide commits the grid
  onDateSelect();
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

// Slide timing.
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
let _animating = false;   // guards re-entrancy while a commit animation runs
function slideInFlight() { return _animating; }
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
  const slideDir = dir ?? shorterWayRound(targetValue);
  if (slideDir == null) { commitDay(targetValue); return; }
  runSlide(targetValue, slideDir);
}

// The direction from the displayed day to `targetValue` the shorter way round
// the day ring (+1 forward/right, -1 back), or null when either is not in it.
function shorterWayRound(targetValue) {
  const ring = dayRing();
  const from = ring.indexOf(_appliedDay);
  const to   = ring.indexOf(targetValue);
  if (from < 0 || to < 0) return null;
  const ringLength = ring.length;
  const forwardSteps = ((to - from) % ringLength + ringLength) % ringLength;   // steps walking forward (right)
  return forwardSteps <= ringLength - forwardSteps ? 1 : -1;
}
