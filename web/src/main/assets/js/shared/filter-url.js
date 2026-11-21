// The URL and the filters: reading every filter off the URL at boot, keeping the day in it, the
// share link, and putting session-local controls back on a Back.

/* global _updateRoomCinemaCount, ensurePanel, updateFormatBtn, updateSubmenuCount */  // filters.js
/* global _searchDayPicked:writable, _searchOrigin:writable, beginSearchAtBoot, urlDay */  // search.js
/* global buildCinemaPanel, getDisabledCinemas, setDisabledCinemas */  // cinemas.js
/* global formatDateLabelShort */  // dates.js
/* exported applyFiltersFromURL, copyFilterLinkToClipboard, noteFiltersApplied, syncDateToURL */

// ── URL ↔ filter state sync ───────────────────────────────────────────────
//
// `applyFiltersFromURL` reads every supported param on boot and applies it
// to the matching control. The URL is the share/bookmark entry point.
//
// After boot only the day select is reflected back into the URL on change
// (via `syncDateToURL`, called from `applyFilters`) — the other filters
// stay local until the user explicitly hits "Skopiuj link do schowka",
// which calls `copyFilterLinkToClipboard` → `buildShareURL` to materialise
// the full current state into a URL and write it both to the address bar
// and the clipboard.
//
// Param map:
//   date    — date-filter select (today/tomorrow/week/anytime/YYYY-MM-DD)
//   q       — search-input text
//   dim     — format-dim radio  (2D / 3D)
//   lang    — format-lang radio, the country's own subtitled/dubbed token
//             (NAP / DUB in Poland, VOSE / DOB in Spain, OmU / DF in Germany)
//   imax    — format-imax checkbox ("1" when on)
//   from    — from-hour:minute composite (HH:MM)
//   sort    — sort-by select ('rating'; 'earliest' default omitted)
//   country, director, cast, room — repeated `?key=value` entries listing the
//             CHECKED items (the inclusion set). Omitted when every box is
//             ticked — the no-filter default → empty URL.
//   cinema  — same inclusion semantics: enabled cinemas (LS-backed).
//
// Append-per-value (not a comma-joined string) so the browser does exactly
// one round of percent-encoding — the previous shape ran `encodeURIComponent`
// by hand and then `URLSearchParams.set` encoded the result a second time
// (rooms like "Cinema City Kinepolis|Sala 1" ended up as
// `Cinema%2520City%2520Kinepolis%257CSala%25201`).

// Lightweight: only the day select is reflected back to the URL in real
// time, so stepping through days keeps history meaningful without polluting
// the URL with every cast/country tick. The same write records the
// session-local controls in the entry's STATE (`rememberedControlsState`),
// which nothing shares or bookmarks, so Back can put them back at boot.
function syncDateToURL() {
  const dateSel = document.getElementById('date-filter');
  if (!dateSel) return;
  const url = new URL(window.location.href);
  const day = urlDay(dateSel);
  if (day === 'today') url.searchParams.delete('date');
  else url.searchParams.set('date', day);
  history.replaceState(rememberedControlsState(), '', url.pathname + url.search + url.hash);
}

// Build (but don't apply) the URL that captures every current filter — used
// by the Copy-link button. Returns the same `pathname + search + hash`
// shape `history.replaceState` expects.
function buildShareURL() {
  const url = new URL(window.location.href);
  const p   = url.searchParams;
  const setOrDel = (k, v) => { if (v) p.set(k, v); else p.delete(k); };

  const dateSel = document.getElementById('date-filter');
  if (dateSel) { const day = urlDay(dateSel); setOrDel('date', day === 'today' ? '' : day); }

  const search = document.getElementById('search-input');
  setOrDel('q', search ? (search.value || '').trim() : '');

  const dim  = (document.querySelector('input[name="format-dim"]:checked')  || {}).value || '';
  const lang = (document.querySelector('input[name="format-lang"]:checked') || {}).value || '';
  setOrDel('dim',  dim);
  setOrDel('lang', lang);

  const imaxEl = document.getElementById('format-imax');
  setOrDel('imax', imaxEl && imaxEl.checked ? '1' : '');

  const fromH = document.getElementById('from-hour');
  const fromM = document.getElementById('from-minute');
  if (fromH && fromH.value !== '') {
    const hh = String(parseInt(fromH.value, 10)).padStart(2, '0');
    const mm = String(parseInt((fromM && fromM.value) || '0', 10)).padStart(2, '0');
    p.set('from', hh + ':' + mm);
  } else {
    p.delete('from');
  }

  // Sort axis: the default ('earliest') stays out of the URL so a plain
  // share link is clean; 'rating' is materialised as ?sort=.
  const sortSel = document.getElementById('sort-by');
  setOrDel('sort', sortSel && sortSel.value !== 'earliest' ? sortSel.value : '');

  ['country', 'genre', 'director', 'cast', 'room'].forEach(key => {
    p.delete(key);
    const list = document.getElementById(key + '-list');
    if (!list) return;
    const boxes = [...list.querySelectorAll('input[type="checkbox"]:not(.submenu-all)')];
    const checked = boxes.filter(checkbox => checkbox.checked).map(checkbox => checkbox.value);
    if (checked.length === boxes.length) return;  // all-on default → empty URL
    checked.forEach(v => p.append(key, v));
  });

  // Cinema filter lives in the Filtry panel. Gate the `cinema` param on the
  // picker's presence so pages without it don't emit a stray param.
  if (document.getElementById('cinema-list')) {
    p.delete('cinema');
    const disabled = getDisabledCinemas();
    if (disabled.length > 0) {
      ALL_CINEMAS.filter(c => !disabled.includes(c)).forEach(v => p.append('cinema', v));
    }
  }

  return url.pathname + url.search + url.hash;
}

// Filtry-panel "Skopiuj link do schowka" handler: materialise the current
// filter state into the address bar AND the clipboard. Briefly flips the
// button label so the user has visual confirmation it took.
function copyFilterLinkToClipboard(button) {
  const path = buildShareURL();
  history.replaceState(rememberedControlsState(), '', path);
  const url = window.location.origin + path;
  const done = () => {
    if (!button) return;
    const previous = button.textContent;
    button.textContent = window.t('nav.copied');
    setTimeout(() => { button.textContent = previous; }, 1500);
  };
  if (navigator.clipboard && navigator.clipboard.writeText) {
    navigator.clipboard.writeText(url).then(done, done);
  } else {
    // Older Safari / non-secure contexts: fall back to the deprecated
    // execCommand path — still ubiquitous enough that "no clipboard" beats
    // a silent failure.
    const ta = document.createElement('textarea');
    ta.value = url;
    ta.style.position = 'fixed';
    ta.style.opacity  = '0';
    document.body.appendChild(ta);
    ta.select();
    try { document.execCommand('copy'); } catch (_) {}
    document.body.removeChild(ta);
    done();
  }
}

// Put every filter control the markup ships back to its default.
//
// WHY: browsers restore form state across a history navigation, and they
// match the saved values POSITIONALLY against the document as re-parsed --
// which no longer contains the cinema/genre/room checkboxes this page builds
// at runtime. Coming back from a film page therefore replays somebody else's
// "checked" onto the static Filtry controls. Observed in WebKit on
// /uk/manchester/?date=tomorrow: `#format-imax` came back checked on a page
// nobody had touched, `readFilters` reported an IMAX filter, and the badge
// loop hid every non-IMAX showtime. That loop only visits the SELECTED day's
// groups, so the day the visitor was actually on rendered empty while every
// other day still looked untouched -- "only tomorrow is blank".
//
// Only the day rides the URL in real time (see `syncDateToURL`); every other
// filter is session-local and a fresh document starts clear unless the URL
// says otherwise. So boot SETS these rather than inheriting them, and
// `applyFiltersFromURL` below then layers any params on top.
//
// The lazily-built submenu panels need no reset: they do not exist at parse
// time, so there is nothing for the engine to restore into -- being absent is
// exactly why the saved state lands on the static controls instead. Neither
// does `#cinema-all`, the one static checkbox not listed here: the cinema
// filter is read from localStorage, and `buildCinemaPanel` re-derives that box
// from it earlier in `bootView`. It carries `autocomplete="off"` all the same,
// because "harmless" there rests on running order rather than on anything the
// markup says, and the next static checkbox added to that panel would inherit
// the restore slot rather than the reasoning.
// KEEPING BUILT CONTROLS OUT OF THE RESTORE QUEUE is the first half of this,
// and it is done above: every checkbox `buildCinemaPanel` and the submenu
// builders create is marked `autocomplete="off"` as it is made. An engine
// queues saved values per (name, type) and replays them positionally, so it
// was the BUILT boxes -- absent from a freshly parsed document -- that shifted
// the saved values onto the static ones. Measured in WebKit on a page shaped
// like this navbar: marking them removes the shift outright.
//
// WHAT STILL RESETS, AND WHY ONLY THIS MUCH. Measured on the same page: a text
// input, a select and a NAMED radio each restore correctly while an unnamed
// checkbox is being corrupted -- the queues do not interfere. So search, sort
// and the from-time selects are not reset, and pressing Back keeps the search
// you typed -- put back by the page itself at boot, below, because the
// engine's own restore of it lands too late to rely on.
//
// Reset here are only the two that cannot be taken at face value:
//  - the FORMAT group, because `#format-imax` is an unnamed checkbox sharing
//    the queue the built boxes join. `autocomplete="off"` should now keep them
//    out of it, but that attribute is honoured unevenly and a phantom IMAX
//    filter blanks the day being viewed (it hid 636 of tomorrow's 647
//    showtimes in Manchester), so the belt stays on. The whole group goes
//    together: half a restored format filter is worse than none.
//  - `#date-filter`, which has a truth outside the DOM. `syncDateToURL` writes
//    the day to the URL and deletes it for `today`, so a URL with no `date` MEANS
//    today and a restored `anytime` would contradict the address bar.
function resetFilterControls() {
  for (const radio of document.querySelectorAll('input[name="format-dim"], input[name="format-lang"]'))
    radio.checked = radio.value === '';
  const imax = document.getElementById('format-imax');
  if (imax) imax.checked = false;
  const date = document.getElementById('date-filter');
  if (date) date.value = 'today';
}

// ...AND THE ENGINE'S RESTORE LANDS LATE, on its own schedule. The search box
// is still EMPTY at `DOMContentLoaded`, so the pass `bootView` runs sees
// nothing. Measured 2026-10-24 with every image request held, so `load` never
// fires: Chromium fills the box some time after `DOMContentLoaded`, with no
// event to re-filter on, leaving "spider" above the unfiltered grid; WebKit
// leaves it empty until `load`. `load` waits on every eager image, so on a
// slow network either lasted until the last image landed -- seconds on a
// phone (and the history-restore page test's flake on a loaded runner).
//
// So the page keeps these controls itself: every filter pass records them in
// the history entry's state (`syncDateToURL`), and a Back to that entry puts
// them back in `applyFiltersFromURL`, BEFORE the first filter pass draws the
// grid. Only on a back/forward traversal: a reload starts clear in every
// engine, and a new visit has no state. The engine's late restore then writes
// the same values again, a no-op.
//
// The controls are static navbar markup, looked up once: `syncDateToURL` runs
// on every filter pass, which has a fixed DOM-query budget.
const REMEMBERED_CONTROLS = ['search-input', 'sort-by', 'from-hour', 'from-minute'];
let _rememberedElements = null;
function rememberedElements() {
  if (!_rememberedElements)
    _rememberedElements = REMEMBERED_CONTROLS
      .map(id => [id, document.getElementById(id)])
      .filter(([, element]) => element);
  return _rememberedElements;
}
function rememberedControlsState() {
  const controls = {};
  for (const [id, element] of rememberedElements()) controls[id] = element.value;
  // The search's day too: Back to a search restores where clearing it would go.
  const searchDay = _searchOrigin === null ? null : { origin: _searchOrigin, picked: _searchDayPicked };
  return { filterControls: controls, searchDay: searchDay };
}
function restoreRememberedControls() {
  const navigation = performance.getEntriesByType('navigation')[0];
  if (!navigation || navigation.type !== 'back_forward') return;
  const controls = history.state && history.state.filterControls;
  if (!controls) return;
  for (const [id, element] of rememberedElements())
    if (typeof controls[id] === 'string') element.value = controls[id];
  const searchDay = history.state.searchDay;
  if (searchDay && typeof searchDay.origin === 'string') {
    _searchOrigin = searchDay.origin;
    _searchDayPicked = !!searchDay.picked;
    // The URL carried the searched-from day (`urlDay`); the listing was on "All".
    const sel = document.getElementById('date-filter');
    if (sel && !_searchDayPicked) sel.value = 'anytime';
  }
}

// The belt for an entry with no recorded state (one written before this
// shipped): re-filter at `pageshow` when the engine's restore has moved the
// controls away from the pass that drew what is on screen. Comparing, rather
// than re-running unconditionally, keeps an ordinary load to its single pass.
let _appliedSignature = null;
function filterSignature() {
  return rememberedElements().map(([, element]) => element.value).join('\u0000');
}
function noteFiltersApplied() { _appliedSignature = filterSignature(); }
window.addEventListener('pageshow', function () {
  if (_appliedSignature === null || filterSignature() === _appliedSignature) return;
  applyFilters();
  noteFiltersApplied();
});

function applyFiltersFromURL() {
  resetFilterControls();
  const searchParams = new URLSearchParams(window.location.search);

  const dateSel = document.getElementById('date-filter');
  if (dateSel) {
    const val   = searchParams.get('date');
    const ALLOW = ['today', 'tomorrow', 'week', 'anytime'];
    const isIso = val && /^\d{4}-\d{2}-\d{2}$/.test(val);
    if (val && (ALLOW.includes(val) || isIso)) {
      if (isIso && !Array.from(dateSel.options).some(o => o.value === val)) {
        // An old shared/bookmarked link can carry a specific ISO date even
        // though the picker now offers only the four presets — add an option
        // on the fly so the select reflects it rather than snapping to 'today'.
        const option = document.createElement('option');
        option.value = val;
        option.dataset.date = val;
        option.textContent = formatDateLabelShort(val);
        const weekOpt = dateSel.querySelector('option[value="week"]');
        if (weekOpt) dateSel.insertBefore(option, weekOpt); else dateSel.appendChild(option);
      }
      dateSel.value = val;
    }
  }

  const search = document.getElementById('search-input');
  if (search) { const queryValue = searchParams.get('q'); if (queryValue !== null) search.value = queryValue; }

  const dim = searchParams.get('dim');
  if (dim) {
    const element = document.querySelector('input[name="format-dim"][value="' + CSS.escape(dim) + '"]');
    if (element) element.checked = true;
  }
  const lang = searchParams.get('lang');
  if (lang) {
    const element = document.querySelector('input[name="format-lang"][value="' + CSS.escape(lang) + '"]');
    if (element) element.checked = true;
  }
  const imaxEl = document.getElementById('format-imax');
  if (imaxEl && searchParams.has('imax')) imaxEl.checked = searchParams.get('imax') === '1';

  const fromParam = searchParams.get('from');
  if (fromParam && /^\d{1,2}:\d{2}$/.test(fromParam)) {
    const [hours, minutes] = fromParam.split(':');
    const hourSelect = document.getElementById('from-hour');
    const minuteSelect = document.getElementById('from-minute');
    if (hourSelect) hourSelect.value = String(parseInt(hours, 10));
    if (minuteSelect) minuteSelect.value = String(parseInt(minutes, 10));
  }

  const sortSel = document.getElementById('sort-by');
  if (sortSel) {
    const sortParam = searchParams.get('sort');
    if (sortParam === 'rating' || sortParam === 'earliest') sortSel.value = sortParam;
  }

  // URL values are the INCLUSION set (checked items). Empty/absent → all
  // checked (the no-filter default, no-op). Tolerate legacy single-value
  // comma-lists by flattening on `,` so an old shared link still narrows
  // down rather than dropping into a single nonexistent value.
  ['country', 'genre', 'director', 'cast', 'room'].forEach(key => {
    const checked = searchParams.getAll(key).flatMap(v => v.split(','));
    if (checked.length === 0) return;
    ensurePanel(key);   // a shared link references this filter → build it now
                        // so the checkboxes exist for the state to land on
    const list = document.getElementById(key + '-list');
    if (!list) return;
    const checkedSet = new Set(checked);
    list.querySelectorAll('input[type="checkbox"]:not(.submenu-all)').forEach(checkbox => {
      checkbox.checked = checkedSet.has(checkbox.value);
    });
    updateSubmenuCount(key);
    // Room is the two-level submenu — refresh each cinema's "x/y" badge so
    // the user can see which cinemas the URL-applied filter touched without
    // expanding every header by hand.
    if (key === 'room') {
      list.querySelectorAll('.room-cinema-header').forEach(_updateRoomCinemaCount);
    }
  });

  // Cinema filter: write the URL inclusion list into localStorage so the
  // cinema-panel checkboxes (and the Filtry button label) reflect it on
  // first render. The internal LS shape still stores DISABLED cinemas, so
  // invert. Pages without the picker have no `#cinema-list` → no-op.
  if (document.getElementById('cinema-list') && searchParams.has('cinema')) {
    const enabled = searchParams.getAll('cinema').flatMap(v => v.split(','));
    setDisabledCinemas(ALL_CINEMAS.filter(c => !enabled.includes(c)));
    buildCinemaPanel();
  }

  // Last, as the engine's own restore would: what the visitor had on this
  // entry outranks the `q`/`sort`/`from` the entry was first opened with.
  restoreRememberedControls();
  beginSearchAtBoot();
  updateFormatBtn();
}
