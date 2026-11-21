// The Filtry panel: what each control asks for (format, start time, sort, the submenus), the
// funnel's active state, the folds, the lazily built submenu lists, and Wyczyść.

/* global clampPanel, closeOtherPanels, setPanelOpen */  // overlays.js
/* exported SUBMENU_KEYS, applyFiltersDebounced, badgeFormatMatch, compareCards, ensurePanel, getCastFilter,
   getCountryFilter, getDirectorFilter, getFormatFilter, getFromMinutes, getSortBy, getSubmenuFilter,
   gridScope, isSubsequence, makeFoldHeader, onFormatChange, onSortChange, registerNarrowing,
   resetFormatFilter, resetSubmenuPanels, toggleFormatPanel, toggleSubmenu, updateFormatBtn, updateRoomCinemaCount,
   updateSubmenuCount */

// ── Format-filter dropdown ────────────────────────────────────────────────

// Three axes live inside one panel: Wymiar (radio, 2D/3D), Wersja (radio,
// the country's own subtitled/dubbed tokens, absent where nothing marks
// either), IMAX (checkbox). Each visible badge must carry every selected
// token on its data-format attribute. Empty selection on an axis = no
// constraint for that axis.
function getFormatFilter() {
  const dim  = (document.querySelector('input[name="format-dim"]:checked')  || {}).value || '';
  const lang = (document.querySelector('input[name="format-lang"]:checked') || {}).value || '';
  // The Filtry panel (and the `#format-imax` checkbox inside it) isn't
  // rendered on all pages, nor on a listing without an IMAX showtime —
  // treat a missing checkbox as "unchecked / no IMAX filter applied"
  // rather than throwing.
  const imaxEl = document.getElementById('format-imax');
  const imax = imaxEl && imaxEl.checked ? 'IMAX' : '';
  return [dim, lang, imax].filter(Boolean);
}

// From-hour filter: returns the lower-bound time as minutes-since-midnight
// (e.g. 18:30 → 1110), or null when the hour dropdown is set to "Dowolna"
// (any). Both dropdowns are read together; the minute default is 00.
function getFromMinutes() {
  const hourString = (document.getElementById('from-hour') || {}).value;
  if (hourString == null || hourString === '') return null;
  const hours = parseInt(hourString, 10);
  const minutes = parseInt((document.getElementById('from-minute') || {}).value, 10) || 0;
  return hours * 60 + minutes;
}

// ── Sort axis ─────────────────────────────────────────────────────────────
//
// The grid orders by one of two axes, picked in the Filtry panel's "Sortuj"
// select: earliest screening (the default) or weighted rating. `rating`
// sorts biggest-first and falls back to earliest-screening for ties; the
// select is absent on pages with no grid — treat that as the default axis.
// The sort key (`data-rating`) is pre-parsed onto each card's INDEX entry by
// the per-page buildIndex, so the comparator never re-reads the DOM.
function getSortBy() {
  var element = document.getElementById('sort-by');
  var sortValue = element ? element.value : 'earliest';
  return (sortValue === 'rating') ? sortValue : 'earliest';
}

// Order two visible cards for the active sort axis. `a`/`b` are
// { earliest, rating, idx }: `earliest` an ISO "<date>T<HH:MM>" string
// (never null here — only already-visible cards reach the sort), `rating` a
// number (0 when the film has no ratings). `idx` (the card's original DOM
// position) is the stable final tiebreak so equal keys preserve server order.
function compareCards(sortBy, a, b) {
  if (sortBy === 'rating' && a.rating !== b.rating) return b.rating - a.rating;
  if (a.earliest !== b.earliest) return a.earliest < b.earliest ? -1 : 1;
  return a.idx - b.idx;
}

// Sort select changed — re-run the page's filter+sort pass. Guarded for
// pages that render the navbar but have no real grid (the stub
// applyFilters is harmless, but a page without one at all mustn't throw).
function onSortChange() {
  if (typeof applyFilters === 'function') applyFilters();
}

// requiredTokens may be empty → fast-path. Otherwise checks a pre-built Set
// attached to each indexed badge (so we don't re-parse `dataset.format` on
// every filter pass).
function badgeFormatMatch(badgeFormatSet, requiredTokens) {
  for (let i = 0; i < requiredTokens.length; i++) {
    if (!badgeFormatSet.has(requiredTokens[i])) return false;
  }
  return true;
}

// Debounce wrapper for fast-firing inputs (the search box). Discrete inputs
// like select/radio/checkbox still call applyFilters() directly.
let _filterDebounce = 0;
function applyFiltersDebounced() {
  clearTimeout(_filterDebounce);
  _filterDebounce = setTimeout(applyFilters, 80);
}

// The Filtry submenus, each a checkbox list `#<key>-list` under its row `#<key>-row`.
const SUBMENU_KEYS = ['country', 'genre', 'director', 'cast', 'room'];

// Beyond the panel's own controls, the cinema picker (`cinemas.js`) and the
// hidden films (`hidden-films.js`) narrow the listing too. Each registers, as
// it loads, how to tell that it narrows it (`isActive`) and how to put it back
// (`clear`, which does nothing when there is nothing to put back), so the
// funnel lights for it and "Wyczyść" clears it without this module knowing it.
const _otherNarrowings = [];
function registerNarrowing(isActive, clear) {
  _otherNarrowings.push({ isActive, clear });
}

// True when anything narrows the listing — i.e. exactly what "Wyczyść"
// (`resetFormatFilter`) puts back. Drives the funnel icon's active state; the
// same rule as iOS `ListingFilters.isActive` and Android
// `FilterFacets.filtersActive`. Sort order is deliberately excluded: it
// reorders, it doesn't filter anything out, so the funnel staying neutral when
// you only re-sort is the honest cue (search has its own field).
function filtersActive() {
  return getFormatFilter().length > 0          // Wymiar / Wersja / IMAX
    || getFromMinutes() !== null               // Od godziny
    || SUBMENU_KEYS.some(key => getSubmenuFilter(key) !== null)
    || _otherNarrowings.some(narrowing => narrowing.isActive());
}

function updateFormatBtn() {
  const button = document.getElementById('format-filter-btn');
  if (!button) return;  // Filtry button not rendered on this page.
  button.classList.toggle('filters-active', filtersActive());
}

function onFormatChange() {
  updateFormatBtn();
  applyFilters();
}

function toggleFormatPanel(event) {
  event.stopPropagation();
  const panel = document.getElementById('format-panel');
  const opening = panel.style.display === 'none';
  if (opening) ensureSubmenuPanels();   // lazily build the grid-scanned lists
  closeOtherPanels(opening ? panel : null);
  setPanelOpen(panel, opening);
  if (opening) clampPanel(panel);
}

// ── Folds ─────────────────────────────────────────────────────────────────
//
// A fold is a header row over a `.submenu-list` body: the Filtry submenus
// (`#<key>-row` over `#<key>-list`, in `_navbar`) and the cinema / area
// groups built at runtime. The header is a `role="button"` row, so it is
// focusable and says whether its body is open; Enter or Space on it does
// what a click does (the delegated handler below, which serves every
// `role="button"` row in the panel — the hidden-films and city rows too).
function setFoldOpen(header, body, open) {
  body.style.display = open ? '' : 'none';
  header.setAttribute('aria-expanded', String(open));
  const chevron = header.querySelector('.submenu-chevron');
  if (chevron) chevron.classList.toggle('open', open);
}
function toggleFold(header, body) { setFoldOpen(header, body, body.style.display === 'none'); }
// A header built at runtime, made the same kind of control `_navbar`'s are.
function makeFoldHeader(header, body) {
  header.setAttribute('role', 'button');
  header.tabIndex = 0;
  header.setAttribute('aria-expanded', 'false');
  header.onclick = () => toggleFold(header, body);
}
document.addEventListener('keydown', e => {
  if (e.key !== 'Enter' && e.key !== ' ') return;
  if (!(e.target instanceof Element) || !e.target.matches('#format-panel [role="button"]')) return;
  e.preventDefault();   // Space would otherwise scroll the panel
  e.target.click();
});

function resetFormatFilter() {
  document.querySelector('input[name="format-dim"][value=""]').checked  = true;
  // The version radios are only rendered for a country that MARKS its
  // subtitled/dubbed screenings — see `_navbar`; the UK and US carry no row.
  const langAll = document.querySelector('input[name="format-lang"][value=""]');
  if (langAll) langAll.checked = true;
  // Rendered only where the listing has an IMAX showtime -- see `_navbar`.
  const imaxEl = document.getElementById('format-imax');
  if (imaxEl) imaxEl.checked = false;
  document.getElementById('from-hour').value       = '';
  document.getElementById('from-minute').value     = '0';
  var sortSel = document.getElementById('sort-by');
  if (sortSel) sortSel.value = 'earliest';
  SUBMENU_KEYS.forEach(function(key) {
    var list = document.getElementById(key + '-list');
    if (list) {
      list.querySelectorAll('input[type="checkbox"]').forEach(function(checkbox) { checkbox.checked = true; });
      setFoldOpen(document.getElementById(key + '-row'), list, false);
    }
    updateSubmenuCount(key);
  });
  // Re-enable every cinema in this city and un-hide every hidden film, as the
  // iOS and Android Wyczyść do.
  _otherNarrowings.forEach(narrowing => narrowing.clear());
  setPanelOpen(document.getElementById('format-panel'), false);
  onFormatChange();
}

// ── Submenu filters (country / director / cast) ─────────────────────────
//
// All three use the same pattern: a collapsible checkbox list with a
// "Wszystkie" toggle, a count badge on the header row, and an entry
// per unique value with a film count. By default every checkbox is
// checked (all included); unchecking narrows the visible set.

function getSubmenuFilter(key) {
  var list = document.getElementById(key + '-list');
  if (!list) return null;
  var boxes = [...list.querySelectorAll('input[type="checkbox"]:not(.submenu-all)')];
  var checked = boxes.filter(function(checkbox) { return checkbox.checked; });
  if (checked.length === boxes.length) return null;
  return checked.map(function(checkbox) { return checkbox.value; });
}

function getCountryFilter()  { return getSubmenuFilter('country'); }
function getDirectorFilter() { return getSubmenuFilter('director'); }
function getCastFilter()     { return getSubmenuFilter('cast'); }

function toggleSubmenu(key) {
  var list = document.getElementById(key + '-list');
  if (list) toggleFold(document.getElementById(key + '-row'), list);
}

function updateSubmenuCount(key) {
  var list = document.getElementById(key + '-list');
  var badge = document.getElementById(key + '-row-count');
  if (!badge || !list) return;
  var boxes = [...list.querySelectorAll('input[type="checkbox"]:not(.submenu-all)')];
  var unchecked = boxes.filter(function(checkbox) { return !checkbox.checked; }).length;
  if (unchecked > 0) {
    badge.textContent = boxes.length - unchecked + '/' + boxes.length;
    badge.style.display = '';
  } else {
    badge.style.display = 'none';
  }
  var allCb = list.querySelector('.submenu-all');
  if (allCb) allCb.checked = unchecked === 0;
}

// Root for grid-wide DOM scans: the listing's `#view-root` when present,
// else the whole document (pages with no `#view-root`).
function gridScope() {
  return document.getElementById('view-root') || document;
}

function buildSubmenuPanel(key, dataAttr, splitter) {
  var valueCounts = {};
  gridScope().querySelectorAll('.col[' + dataAttr + ']').forEach(function(col) {
    splitter(col.dataset[dataAttr.replace('data-', '')] || '').forEach(function(v) {
      valueCounts[v] = (valueCounts[v] || 0) + 1;
    });
  });
  var entries = Object.keys(valueCounts).sort(function(a, b) {
    return (valueCounts[b] - valueCounts[a]) || a.localeCompare(b, 'pl');
  }).map(function(v) { return { value: v, label: v, count: valueCounts[v] }; });
  renderSubmenuCheckboxes(key, entries);
}

// A submenu's "Wszystkie" row: checking or unchecking it does the same to
// every entry in `list`, then `afterToggle` re-counts and re-filters. Its
// `submenu-all` class keeps it out of `getSubmenuFilter` reads.
function appendAllToggle(list, afterToggle) {
  var allLabel = document.createElement('label');
  allLabel.className = 'panel-label';
  allLabel.style.borderBottom = '1px solid #3a3a6e';
  allLabel.style.marginBottom = '4px';
  allLabel.style.paddingBottom = '8px';
  var allCb = document.createElement('input');
  allCb.type = 'checkbox';
  allCb.autocomplete = 'off';   // see KEEPING BUILT CONTROLS OUT OF THE RESTORE QUEUE
  allCb.checked = true;
  allCb.className = 'submenu-all';
  allCb.onchange = function() {
    list.querySelectorAll('input[type="checkbox"]:not(.submenu-all)').forEach(function(checkbox) {
      checkbox.checked = allCb.checked;
    });
    afterToggle();
  };
  allLabel.appendChild(allCb);
  allLabel.appendChild(document.createTextNode(' Wszystkie'));
  list.appendChild(allLabel);
}

// Render a submenu's "Wszystkie" + per-value checkbox rows from a sorted
// entries list. Shared by `buildSubmenuPanel` (country / director / cast,
// values pulled from `.col` data attrs) and `buildRoomPanel` (values pulled
// from `.badge-time[data-room]` × cinema-group, with a composite key
// "Cinema|Room" that displays as "Cinema — Room").
function renderSubmenuCheckboxes(key, entries) {
  var list = document.getElementById(key + '-list');
  if (!list) return;
  list.innerHTML = '';

  appendAllToggle(list, function() {
    updateSubmenuCount(key); updateFormatBtn(); applyFilters();
  });

  entries.forEach(function(entry) {
    list.appendChild(submenuEntryRow(entry.value, entry.label, entry.count, function() {
      updateSubmenuCount(key); updateFormatBtn(); applyFilters();
    }));
  });
}

// One submenu entry, checked: its checkbox carrying `value`, its label and its
// "(count)" of films (or, under Sale, showtimes).
function submenuEntryRow(value, labelText, count, onchange) {
  var label = document.createElement('label');
  label.className = 'panel-label';
  var checkbox = document.createElement('input');
  checkbox.type = 'checkbox';
  checkbox.autocomplete = 'off';
  checkbox.value = value;
  checkbox.checked = true;
  checkbox.onchange = onchange;
  label.appendChild(checkbox);
  label.appendChild(document.createTextNode(' ' + labelText));
  var countSpan = document.createElement('span');
  countSpan.className = 'submenu-film-count';
  countSpan.textContent = '(' + count + ')';
  label.appendChild(countSpan);
  return label;
}

// The submenu panels (country/genre/director/cast/room) scan the WHOLE grid
// to tally values, but they populate dropdowns hidden until the user opens
// Filtry. So build them lazily — on first Filtry-open, or on demand when a
// shared `?genre=…`/`?room=…` link needs one applied at boot — instead of
// eagerly at load. `_panelBuilt` is reset per view (see `bootView`) so each
// view's grid gets a fresh tally.
const _panelBuilt = {};
function ensurePanel(key) {
  if (_panelBuilt[key]) return;
  switch (key) {
    case 'country':  buildCountryPanel();  break;
    case 'genre':    buildGenrePanel();    break;
    case 'director': buildDirectorPanel(); break;
    case 'cast':     buildCastPanel();     break;
    case 'room':     buildRoomPanel();     break;
    default: return;
  }
  _panelBuilt[key] = true;
}
function ensureSubmenuPanels() {
  SUBMENU_KEYS.forEach(ensurePanel);
}
function resetSubmenuPanels() {
  Object.keys(_panelBuilt).forEach(function(k) { delete _panelBuilt[k]; });
}

function buildCountryPanel() {
  buildSubmenuPanel('country', 'data-countries', function(s) { return s.split('|').filter(Boolean); });
}
function buildGenrePanel() {
  buildSubmenuPanel('genre', 'data-genres', function(s) { return s.split('|').filter(Boolean); });
}
function buildDirectorPanel() {
  buildSubmenuPanel('director', 'data-director', function(s) { return s.split(',').map(function(v) { return v.trim(); }).filter(Boolean); });
}
function buildCastPanel() {
  buildSubmenuPanel('cast', 'data-cast', function(s) { return s.split(',').map(function(v) { return v.trim(); }).filter(Boolean); });
}

// Sale: two-level menu. The outer Sale row expands to a list of cinemas;
// each cinema is itself an expandable header that reveals its rooms,
// sorted naturally ("Sala 10" lands after "Sala 9", not after "Sala 1").
// Room checkboxes carry the composite "Cinema|Room" value so the filter
// logic in `applyFilters` stays unchanged — same value the URL sync writes
// out under `?room=` and reads back on boot. The cinema half is
// load-bearing because the same room name ("Sala 5") exists in multiple
// cinemas and the user typically wants to scope to one of them.
function buildRoomPanel() {
  var list = document.getElementById('room-list');
  if (!list) return;

  var byCinema = tallyRoomsByCinema();
  var cinemas = Object.keys(byCinema).sort(function(a, b) { return a.localeCompare(b, 'pl'); });

  // Hide the Sale row entirely when no badge on the page carries `data-room`
  // (e.g. a fixture day where the scrapers returned no rooms).
  var row = document.getElementById('room-row');
  if (row) row.style.display = cinemas.length === 0 ? 'none' : '';

  list.innerHTML = '';

  // Sale-level "Wszystkie" — flips every room checkbox at once, every
  // cinema's header count following.
  appendAllToggle(list, function() {
    list.querySelectorAll('.room-cinema-header').forEach(function(h) { updateRoomCinemaCount(h); });
    updateSubmenuCount('room'); updateFormatBtn(); applyFilters();
  });

  cinemas.forEach(function(cinema) { appendRoomCinemaFold(list, cinema, byCinema[cinema]); });
}

// { cinema: { room: showtime count } } over the grid's room-carrying badges.
function tallyRoomsByCinema() {
  var byCinema = {};
  gridScope().querySelectorAll('.cinema-group[data-cinema]').forEach(function(cinemaGroup) {
    var cinema = cinemaGroup.dataset.cinema;
    cinemaGroup.querySelectorAll('.badge-time[data-room]').forEach(function(b) {
      var room = b.dataset.room;
      if (!room) return;
      if (!byCinema[cinema]) byCinema[cinema] = {};
      byCinema[cinema][room] = (byCinema[cinema][room] || 0) + 1;
    });
  });
  return byCinema;
}

// One cinema's fold inside Sale: a header with its "x/y" count over its
// rooms' checkboxes, sorted naturally.
function appendRoomCinemaFold(list, cinema, roomCounts) {
  var rooms = Object.keys(roomCounts).sort(function(a, b) {
    // `numeric: true` is the natural-sort knob — without it, the
    // comparator treats "Sala 10" as < "Sala 2" because '1' < '2'
    // lexicographically. With it, embedded numbers compare numerically.
    return a.localeCompare(b, 'pl', { numeric: true });
  });

  var header = document.createElement('div');
  header.className = 'panel-label submenu-row room-cinema-header';
  header.style.cursor = 'pointer';
  var headerLabel = document.createElement('span');
  headerLabel.textContent = cinema;
  header.appendChild(headerLabel);
  var right = document.createElement('span');
  right.className = 'submenu-right';
  var count = document.createElement('span');
  count.className = 'submenu-row-count room-cinema-count';
  count.style.display = 'none';
  right.appendChild(count);
  var chevron = document.createElement('span');
  chevron.className = 'submenu-chevron';
  chevron.innerHTML = '&#8250;';
  right.appendChild(chevron);
  header.appendChild(right);
  list.appendChild(header);

  var inner = document.createElement('div');
  inner.className = 'submenu-list room-cinema-list';
  inner.style.display = 'none';
  inner.style.marginLeft = '12px';

  makeFoldHeader(header, inner);

  rooms.forEach(function(room) {
    inner.appendChild(submenuEntryRow(cinema + '|' + room, room, roomCounts[room], function() {
      updateRoomCinemaCount(header);
      updateSubmenuCount('room'); updateFormatBtn(); applyFilters();
    }));
  });

  list.appendChild(inner);
}

// Per-cinema header badge inside Sale — surfaces "3/8" when 3 of 8 rooms
// in that cinema are checked, so the user knows which cinemas they've
// narrowed without expanding each one.
function updateRoomCinemaCount(headerEl) {
  var count = headerEl.querySelector('.room-cinema-count');
  if (!count) return;
  var inner = headerEl.nextElementSibling;
  if (!inner) return;
  var boxes = [...inner.querySelectorAll('input[type="checkbox"]')];
  var unchecked = boxes.filter(function(b) { return !b.checked; }).length;
  if (unchecked > 0) {
    count.textContent = (boxes.length - unchecked) + '/' + boxes.length;
    count.style.display = '';
  } else {
    count.style.display = 'none';
  }
}

// True when `sub` (the new desired visible order) is an in-order subset of
// `full` (the order applied at the last rebuild). When true, the visible
// cards' relative DOM order is already correct — some entries just became
// hidden, but display:none keeps them out of layout so their position is
// irrelevant. Skipping the rebuild in that case is what stops the X-button
// from briefly jumping the scroll position to the top of the page.
function isSubsequence(sub, full) {
  let j = 0;
  for (let i = 0; i < full.length && j < sub.length; i++) {
    if (full[i] === sub[j]) j++;
  }
  return j === sub.length;
}
