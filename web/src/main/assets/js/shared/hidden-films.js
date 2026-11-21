// The films a visitor hid: the per-country list on this device, the Filtry row and modal that
// show it, and hiding, restoring and un-hiding them all.

/* global currentCountryCode */  // city.js
/* global _lsGet, _lsSet */  // local-store.js
/* global updateFormatBtn */  // filters.js
/* global closeDialog, closeOtherPanels, openDialog */  // overlays.js
/* global maybeShowAnonymousNag */  // auth.js
/* global clearHiddenFilmsOnServer, hideFilmOnServer, unhideFilmOnServer */  // hidden-films-sync.js
/* exported closeHiddenModal, filterHiddenModal, getHidden, hideFilm, openHiddenModal, setHidden,
   showAllFilms, updateNavbar */

// Hidden films are kept PER COUNTRY (`hiddenFilms:<cc>`), as the server and
// the apps keep them: showtimes.cc serves /uk, /de, /us and /es from one
// origin, so one list there was every country's at once — a /uk hide was in
// the list the /de page filtered and counted by, and a sign-in on /de had to
// be told (by a marker saying which country the list mirrored) not to union
// /uk's titles into the /de account.
function _hiddenFilmsKey(country) { return 'hiddenFilms:' + country; }
// The single list (and its marker) earlier builds kept — and the legacy
// cookie `_migrate` above still lifts into — moved into the bucket of the
// country it belongs to: the one the marker names, else this page's. Lazy,
// on every read, so a list written under the old key at any point (a
// pre-upgrade tab, the cookie) lands in its bucket rather than being lost.
const LEGACY_HIDDEN_FILMS_KEY = 'hiddenFilms';
const LEGACY_HIDDEN_FILMS_COUNTRY_KEY = 'hiddenFilmsCountry';
function _settleLegacyHiddenFilms() {
  try {
    const legacy = _lsGet(LEGACY_HIDDEN_FILMS_KEY);
    if (legacy === null) return;
    const marker = localStorage.getItem(LEGACY_HIDDEN_FILMS_COUNTRY_KEY);
    const owner  = marker && /^[a-z]{2}$/.test(marker) ? marker : currentCountryCode();
    const bucket = _lsGet(_hiddenFilmsKey(owner)) || [];
    _lsSet(_hiddenFilmsKey(owner), [...new Set([...bucket, ...legacy])]);
    localStorage.removeItem(LEGACY_HIDDEN_FILMS_KEY);
    localStorage.removeItem(LEGACY_HIDDEN_FILMS_COUNTRY_KEY);
    // Every country's cached validators described the one old list, and were
    // only replayed while it mirrored that country: after the move a bucket
    // can be empty that they vouch for. Forget them all, so each country's
    // next reconcile takes the server's list instead of a 304.
    Object.keys(localStorage)
      .filter(k => k.indexOf('hiddenFilmsEtag:') === 0 || k.indexOf('hiddenFilmsLastModified:') === 0)
      .forEach(k => localStorage.removeItem(k));
  } catch {}
}
function getHidden(country) {
  _settleLegacyHiddenFilms();
  return _lsGet(_hiddenFilmsKey(country || currentCountryCode())) || [];
}
// Pure localStorage write. The server round-trip is the CALLER's job now —
// each caller already knows exactly which title it hid/unhid (or that it
// cleared everything), which is what the granular per-title API needs; see
// hideFilmOnServer/unhideFilmOnServer/clearHiddenFilmsOnServer below and
// their call sites (hideFilm/restoreFilm/showAllFilms).
function setHidden(titles, country) {
  _settleLegacyHiddenFilms();
  _lsSet(_hiddenFilmsKey(country || currentCountryCode()), titles);
}

// ── Hidden-films UI ───────────────────────────────────────────────────────
//
// The trigger lives as a row inside the Filtry dropdown
// (`#hidden-row`). Tapping it opens `#hidden-dialog` — a
// centred modal whose body lists every hidden title. Clicking a
// title restores it but leaves the modal open (browse-and-toggle);
// `Wyczyść` clears the whole set and closes the modal. `×`, a
// backdrop click and Escape close without changing the set.

// The hidden-films row and the funnel: every hide / restore / un-hide-all
// lands here, and a hidden film lights the funnel (`filtersActive`).
function updateNavbar() {
  updateFormatBtn();
  const hidden = getHidden();
  const row     = document.getElementById('hidden-row');
  const divider = document.getElementById('hidden-row-divider');
  const count   = document.getElementById('hidden-row-count');
  const list    = document.getElementById('hidden-modal-list');
  if (!row || !list) return;  // hidden-films UI not rendered on this page.
  if (hidden.length > 0) {
    row.style.display = 'flex';
    if (divider) divider.style.display = '';
    count.textContent = hidden.length;
    list.innerHTML = '';
    hidden.forEach(title => {
      const item = document.createElement('div');
      item.className = 'panel-item';
      item.textContent = title;
      item.title = title;
      item.onclick = () => restoreFilm(title);
      list.appendChild(item);
    });
    // Reapply any in-flight search query so restoring one film from
    // a filtered list doesn't suddenly show every other hidden title.
    filterHiddenModal();
  } else {
    row.style.display = 'none';
    if (divider) divider.style.display = 'none';
    // Auto-close if the modal was open with the last title just removed.
    closeHiddenModal();
  }
}

// In-modal title filter. Pure DOM toggle on the already-rendered
// panel-items — no re-render, so the user's typed query keeps focus
// and caret position across keystrokes.
function filterHiddenModal() {
  const input = document.getElementById('hidden-modal-search');
  if (!input) return;
  const query = input.value.trim().toLowerCase();
  document.querySelectorAll('#hidden-modal-list .panel-item').forEach(item => {
    item.style.display = query === '' || item.textContent.toLowerCase().includes(query) ? '' : 'none';
  });
}

function openHiddenModal(event) {
  if (event) event.stopPropagation();
  // Close the Filtry dropdown so the modal doesn't paint behind a half-
  // open dropdown panel on small screens. The other navbar dropdowns
  // get closed too — same idiom as `closeOtherPanels` for opening any
  // dropdown.
  closeOtherPanels(null);
  // Reset the search so every open shows the full list, however the last one
  // was closed (✕, backdrop, Escape, `Wyczyść`).
  const searchInput = document.getElementById('hidden-modal-search');
  if (searchInput) { searchInput.value = ''; filterHiddenModal(); }
  openDialog('hidden-dialog');
}
function closeHiddenModal() {
  closeDialog('hidden-dialog');
}

function hideFilm(button) {
  // No scrollY snapshot needed: hiding only ever removes a card, never
  // reorders the visible set, so the browser's scroll anchor stays put.
  const title = button.closest('[data-title]').dataset.title;
  const hidden = getHidden();
  if (!hidden.includes(title)) {
    hidden.push(title);
    setHidden(hidden);
    hideFilmOnServer(title);
    maybeShowAnonymousNag();  // hide is the other "this will only stick on this device" action
  }
  // Fast path: drop just this card. The full applyFilters() re-walks every
  // card and un-/re-truncates all their showings (~0.5s on a busy city) —
  // wasted work when one card leaves view. Fall back to the full pass if the
  // view didn't expose the single-card hook (e.g. the self-contained browse
  // view) or the index isn't built.
  if (!(window.hideOneFilm && window.hideOneFilm(title))) applyFilters();
  updateNavbar();
}

function restoreFilm(title) {
  preserveScroll(() => {
    setHidden(getHidden().filter(t => t !== title));
    unhideFilmOnServer(title);
    applyFilters();
    updateNavbar();
  });
}

function showAllFilms() {
  preserveScroll(() => {
    setHidden([]);
    clearHiddenFilmsOnServer();
    applyFilters();
    updateNavbar();
  });
  // `updateNavbar` will close the modal when the hidden set is empty
  // (last-title-removed path covers per-row restoreFilm too), but be
  // explicit here so the intent reads at the call site: `Wyczyść`
  // always closes.
  closeHiddenModal();
}

// Wraps a DOM-rebuilding action so the viewport stays put. Used by the
// restore-film paths where a card moves from hidden back into the sorted
// grid — appendChild detaches+reattaches every visible card and the
// browser's scroll anchor can't survive that. The hide path doesn't need
// this because its rebuild is skipped via the subsequence check.
function preserveScroll(fn) {
  const y = window.scrollY;
  fn();
  window.scrollTo(0, y);
}
