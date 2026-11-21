// The search field: its iOS zoom lock, the floating pill's keyboard inset and tap-away dismiss, and
// the day a search moves the listing to and back from.

/* global applyFiltersDebounced */  // filters.js
/* global onDateChange */  // day-carousel.js
/* exported beginSearchAtBoot, lockSearchZoom, noteDayChange, onSearchInput, restoreSearchDay, searchDayState,
   searchOriginDay, unlockSearchZoom, urlDay */

// iOS Safari auto-zooms the viewport when focus lands on an input whose
// font-size is below 16px — and the navbar search is intentionally ~12.5px
// (`--navbar-fs`, a deliberate trade-off for navbar typography uniformity,
// see `_sharedStyles`). Worse, Safari does NOT zoom back out on blur, leaving
// the page stuck wider than the screen.
//
// PREVENT it rather than undo it: while the field is focused, pin
// `maximum-scale=1` so iOS never zooms in the first place; on blur, restore
// the original content so pinch-zoom works everywhere else (leaving it pinned
// would disable zoom for good — a WCAG no-no). Undoing an already-applied
// zoom proved unreliable on real MobileSafari; preventing it is robust.
const _viewportMeta = () => document.querySelector('meta[name="viewport"]');
let _searchBaseViewport = null;
function lockSearchZoom() {
  const viewport = _viewportMeta();
  if (!viewport || _searchBaseViewport !== null) return;   // already locked — no-op
  _searchBaseViewport = viewport.getAttribute('content');
  viewport.setAttribute('content', _searchBaseViewport + ', maximum-scale=1');
}
function unlockSearchZoom() {
  const viewport = _viewportMeta();
  if (!viewport || _searchBaseViewport === null) return;
  viewport.setAttribute('content', _searchBaseViewport);
  _searchBaseViewport = null;
}

// On mobile portrait the search field is a floating pill pinned to the bottom
// of the viewport (`position: fixed; bottom: …` in `_sharedStyles`). iOS
// Safari's on-screen keyboard shrinks only the *visual* viewport, not the
// *layout* viewport the pill is pinned to — so without help the pill, and the
// text being typed into it, hides BEHIND the keyboard as results filter and
// the page settles. `window.visualViewport` reports the keyboard's height as
// the gap between the layout-viewport bottom (`innerHeight`) and the visible
// region (`height + offsetTop`); we feed that into the `--keyboard-inset`
// custom property the pill's `bottom` adds, lifting it above the keyboard.
//
// Gated on the search field being focused: an unrelated pinch-zoom also
// shrinks the visual viewport, and we don't want that to nudge the pill.
// Reset to 0 on blur / keyboard close so the pill drops back down.
function searchKeyboardInset() {
  const viewport = window.visualViewport;
  const search = document.getElementById('search-input');
  if (!viewport || !search || document.activeElement !== search) return 0;
  return Math.max(0, window.innerHeight - viewport.height - viewport.offsetTop);
}
function applySearchKeyboardInset() {
  document.documentElement.style.setProperty('--keyboard-inset', searchKeyboardInset() + 'px');
}
if (window.visualViewport) {
  window.visualViewport.addEventListener('resize', applySearchKeyboardInset);
  window.visualViewport.addEventListener('scroll', applySearchKeyboardInset);
}
// Re-evaluate on the field's own focus/blur too: `resize` alone can lag the
// first focus, and on blur it snaps the inset back to 0 immediately rather
// than waiting for the keyboard's close animation to fire its own resize.
// Delegated (focusin/focusout bubble) so it works regardless of when the
// #search-input element mounts relative to this script.
document.addEventListener('focusin',  applySearchKeyboardInset);
document.addEventListener('focusout', applySearchKeyboardInset);

// On mobile portrait the search field lives in a floating pill pinned to the
// bottom of the viewport, so when its keyboard is up the rest of the screen —
// film cards, poster/title links — sits right behind it. A tap "away" to
// dismiss the keyboard would otherwise also land on whatever is under the
// finger and navigate to /movie. Make that first tap a pure dismiss: blur the
// field (which drops the keyboard) and swallow the click so nothing else acts
// on it. A second, deliberate tap then behaves normally.
//
// Keyed off `pointerdown`, not the `click` itself: by the time the click
// fires the browser has already moved focus off the field, so
// `document.activeElement` no longer points at it. `pointerdown` runs before
// that focus shift — the only place we can reliably tell the field WAS focused
// when the tap began. Scoped to the same breakpoint as the floating pill
// (`_sharedStyles`); on wider / landscape layouts the field is inline and
// outside taps behave normally. Both listeners are capture-phase so they
// pre-empt the card-tap and dropdown-dismiss handlers further down.
const _floatingSearchMq = window.matchMedia('(max-width: 575px) and (orientation: portrait)');
let _dismissSearchOnClick = false;
document.addEventListener('pointerdown', e => {
  const search = document.getElementById('search-input');
  _dismissSearchOnClick =
    _floatingSearchMq.matches &&
    !!search && document.activeElement === search &&
    e.target instanceof Element && !e.target.closest('.navbar-search');
}, true);
document.addEventListener('click', e => {
  if (!_dismissSearchOnClick) return;
  _dismissSearchOnClick = false;
  e.preventDefault();
  e.stopImmediatePropagation();
  const search = document.getElementById('search-input');
  if (search) search.blur();
}, true);

// ── A search's day ────────────────────────────────────────────────────────
//
// A search looks through EVERY day: the first letter typed moves the listing to
// "All" (no slide) and remembers the day it was on; clearing the search goes back
// there -- unless a day was picked while it ran (a tap, a swipe, a key), which then
// stays. While the search shows every day, the days it brought in are drawn lighter
// (`outside-day`, decided by the filter pass from `searchOriginDay`).
let _searchOrigin = null;      // the day the listing was on when the search began; null: no search
let _searchDayPicked = false;  // a day was picked while the search ran
let _searchMovingDay = false;  // the search itself is moving the day, which is no pick

/** The day a running search began from, while nothing has been picked since; else null. */
function searchOriginDay() { return _searchOrigin !== null && !_searchDayPicked ? _searchOrigin : null; }

// The listing's day moved from `previousDay` to `nextDay`. By anything but the search itself,
// mid-search, that is a pick: it stays when the search is cleared. Not the first apply at boot
// (`previousDay` null: nothing was on screen to move from).
function noteDayChange(previousDay, nextDay) {
  if (_searchOrigin !== null && !_searchMovingDay && previousDay !== null && previousDay !== nextDay)
    _searchDayPicked = true;
}

// The running search's day, for the history entry: where clearing it would go. Null: no search.
function searchDayState() {
  return _searchOrigin === null ? null : { origin: _searchOrigin, picked: _searchDayPicked };
}

// Put back a search's day recorded by `searchDayState`; the listing it was on showed "All"
// unless a day was picked since.
function restoreSearchDay(searchDay) {
  if (!searchDay || typeof searchDay.origin !== 'string') return;
  _searchOrigin = searchDay.origin;
  _searchDayPicked = !!searchDay.picked;
  // The URL carried the searched-from day (`urlDay`); the listing was on "All".
  const sel = document.getElementById('date-filter');
  if (sel && !_searchDayPicked) sel.value = 'anytime';
}

function moveDayForSearch(value) {
  const sel = document.getElementById('date-filter');
  if (!sel || sel.value === value) return;
  sel.value = value;
  _searchMovingDay = true;
  try { onDateChange(); } finally { _searchMovingDay = false; }
}

function currentQuery() {
  return (document.getElementById('search-input')?.value || '').trim();
}

// The search box's `input` (typing, and the native clear X, which fires it too).
function onSearchInput() {
  const sel = document.getElementById('date-filter');
  const query = currentQuery();
  if (sel && query && _searchOrigin === null) {
    _searchOrigin = sel.value;
    _searchDayPicked = false;
    moveDayForSearch('anytime');
  } else if (!query && _searchOrigin !== null) {
    const back = _searchDayPicked ? null : _searchOrigin;
    _searchOrigin = null;
    _searchDayPicked = false;
    if (back) moveDayForSearch(back);
  }
  applyFiltersDebounced();
}

// The day the URL carries: the searched-from day while the search itself is showing
// "All", so a reload (which starts with an empty search) and Back land on the day the
// listing was really on, not on the search's "All".
function urlDay(sel) { return searchOriginDay() ?? sel.value; }

// A page opened on a query with no remembered search (a shared `?q=` link): the
// search begins from the link's day, showing every day.
function beginSearchAtBoot() {
  const sel = document.getElementById('date-filter');
  if (!sel || !currentQuery() || _searchOrigin !== null) return;
  _searchOrigin = sel.value;
  _searchDayPicked = false;
  sel.value = 'anytime';
}
