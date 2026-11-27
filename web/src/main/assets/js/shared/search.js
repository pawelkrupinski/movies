// The search field: its iOS zoom lock, the floating pill's keyboard inset and tap-away dismiss, and
// moving the listing's day as a search begins and ends.

/* global pageState */  // page-state.js
/* global applyFiltersDebounced */  // filters.js
/* global onDateChange */  // day-carousel.js
/* global beginSearchFrom, currentQuery, endSearch, searchIsRunning, whileSearchMovesDay */  // search-day.js
/* exported lockSearchZoom, onSearchInput, unlockSearchZoom */

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
function lockSearchZoom() {
  const viewport = _viewportMeta();
  if (!viewport || pageState.searchBaseViewport !== null) return;   // already locked — no-op
  pageState.searchBaseViewport = viewport.getAttribute('content');
  viewport.setAttribute('content', pageState.searchBaseViewport + ', maximum-scale=1');
}
function unlockSearchZoom() {
  const viewport = _viewportMeta();
  if (!viewport || pageState.searchBaseViewport === null) return;
  viewport.setAttribute('content', pageState.searchBaseViewport);
  pageState.searchBaseViewport = null;
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
document.addEventListener('pointerdown', e => {
  const search = document.getElementById('search-input');
  pageState.dismissSearchOnClick =
    _floatingSearchMq.matches &&
    !!search && document.activeElement === search &&
    e.target instanceof Element && !e.target.closest('.navbar-search');
}, true);
document.addEventListener('click', e => {
  if (!pageState.dismissSearchOnClick) return;
  pageState.dismissSearchOnClick = false;
  e.preventDefault();
  e.stopImmediatePropagation();
  const search = document.getElementById('search-input');
  if (search) search.blur();
}, true);

// ── A search's day ────────────────────────────────────────────────────────
//
// Typing moves the listing to "All" and clearing goes back (`search-day.js` keeps
// which day that is).

function moveDayForSearch(value) {
  const sel = document.getElementById('date-filter');
  if (!sel || sel.value === value) return;
  sel.value = value;
  whileSearchMovesDay(onDateChange);
}

// The search box's `input` (typing, and the native clear X, which fires it too).
function onSearchInput() {
  const sel = document.getElementById('date-filter');
  const query = currentQuery();
  if (sel && query && !searchIsRunning()) {
    beginSearchFrom(sel.value);
    moveDayForSearch('anytime');
  } else if (!query && searchIsRunning()) {
    const back = endSearch();
    if (back) moveDayForSearch(back);
  }
  applyFiltersDebounced();
}
