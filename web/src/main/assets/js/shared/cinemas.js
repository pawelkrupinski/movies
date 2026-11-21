// The cinema filter: which cinemas this device switched off, the seasonal venues, the Filtry
// cinema and area panels, the first-visit area picker, and the empty state they can cause.

/* global readStoredJson, storeJson */  // local-store.js
/* global makeFoldHeader, registerNarrowing, updateFormatBtn */  // filters.js
/* exported buildCinemaPanel, getDisabledCinemas, maybeShowAreaPicker, setDisabledCinemas, toggleAllCinemas,
   updateEmptyState */

// disabledCinemas is device-local ONLY — no server round-trip at all, ever
// (see `hidden-films-sync.js`, which no longer models this field in either
// direction).
function getDisabledCinemas()  { return readStoredJson('disabledCinemas') || []; }
function setDisabledCinemas(l) { storeJson('disabledCinemas', l); }
// `disabledCinemas` is ONE global list (cinema display-names) shared across
// every city — switching city is a full navigation that doesn't touch it, so
// a cinema you deselected in another city stays in the list. That's
// deliberate (return to that city and your deselection is still there), but
// it means the raw list can name cinemas that don't exist in the *current*
// city. Any count or select-all/indeterminate state MUST be derived from the
// entries that actually belong to this city — otherwise an other-city name
// makes the count read one short and wrongly flips "Wszystkie kina" to
// indeterminate right after a city switch. Membership tests against a single
// card's cinema (e.g. applyFilters) don't need this — a stale name simply
// never matches — so only the aggregate count/state callers scope here.
function disabledCinemasInCity() { return getDisabledCinemas().filter(c => ALL_CINEMAS.includes(c)); }
// Entries naming a cinema in some OTHER city — preserved verbatim when this
// city's select-all toggles the whole set, so a round-trip keeps them.
function disabledCinemasElsewhere() { return getDisabledCinemas().filter(c => !ALL_CINEMAS.includes(c)); }

// ── Seasonal venues ───────────────────────────────────────────────────────

// An open-air screen or drive-in (`window.CINEMA_SEASONS`, by display name):
// `{opens, closes, offSeason}`. `offSeason` is the server's verdict on its own
// clock — read here, never re-derived from the months, so the panel, the apps
// and the worker always agree on which venues are out of season.
function cinemaSeason(cinema) {
  return (window.CINEMA_SEASONS || {})[cinema] || null;
}
function isOffSeason(cinema) {
  const season = cinemaSeason(cinema);
  return !!(season && season.offSeason);
}
// Out-of-season venues sink to the bottom of their list; the order is otherwise kept.
function seasonOrdered(cinemas) {
  return cinemas.filter(c => !isOffSeason(c)).concat(cinemas.filter(isOffSeason));
}
// A translatable span: `applyLanguage` re-words it on a language switch.
function i18nSpan(key, arg0) {
  const span = document.createElement('span');
  span.setAttribute('data-i18n', key);
  if (arg0 != null) span.setAttribute('data-i18n-arg0', arg0);
  span.textContent = window.t(key, arg0 != null ? [arg0] : undefined);
  return span;
}
// "May–Sept", each month its own translatable span.
function appendSeasonWindow(parent, season) {
  parent.appendChild(i18nSpan('season.month.' + season.opens));
  parent.appendChild(document.createTextNode('–'));
  parent.appendChild(i18nSpan('season.month.' + season.closes));
}

// ── Cinema-filter panel ───────────────────────────────────────────────────

// The per-cinema checkbox row (`data-cinema` carries the display-name so
// `refreshCinemaCheckboxes` can re-derive its checked state in place). Shared
// by the flat list and the per-area groups below.
function buildCinemaRow(cinema) {
  const label = document.createElement('label');
  label.className = 'panel-label';
  const checkbox = document.createElement('input');
  checkbox.type = 'checkbox';
  checkbox.autocomplete = 'off';
  checkbox.dataset.cinema = cinema;
  checkbox.checked = !getDisabledCinemas().includes(cinema);
  checkbox.onchange = () => {
    const disabled = getDisabledCinemas();
    if (checkbox.checked) {
      setDisabledCinemas(disabled.filter(c => c !== cinema));
    } else {
      if (!disabled.includes(cinema)) disabled.push(cinema);
      setDisabledCinemas(disabled);
    }
    syncAreaCheckboxes();
    syncAllCheckbox();
    updateFormatBtn();   // cinema count is part of the Filtry label now
    applyFilters();
  };
  label.appendChild(checkbox);
  const name = ' ' + (CINEMA_PILLS[cinema] || cinema);
  if (isOffSeason(cinema)) {
    // Out of season: the name, and under it a muted "back in season · May–Sept".
    const block = document.createElement('span');
    block.className = 'cinema-name';
    block.appendChild(document.createTextNode(name));
    const line = document.createElement('span');
    line.className = 'cinema-season';
    line.appendChild(i18nSpan('season.back'));
    line.appendChild(document.createTextNode(' · '));
    appendSeasonWindow(line, cinemaSeason(cinema));
    block.appendChild(line);
    label.appendChild(block);
  } else {
    label.appendChild(document.createTextNode(name));
  }
  return label;
}

// A collapsible area group: a header row (area checkbox, then the name + chevron fold control) over
// a `submenu-list` body of the area's cinema rows, collapsed by default. Reuses
// the country/genre submenu classes so the fold looks native. The area checkbox
// (de)selects the whole area; clicking anywhere else on the header folds it.
function buildAreaGroup(area) {
  const group = document.createElement('div');
  group.className = 'cinema-area-group';
  group.dataset.areaSlug = area.slug;

  const header = document.createElement('div');
  header.className = 'panel-label submenu-row cinema-area-header';

  const areaCb = document.createElement('input');
  areaCb.type = 'checkbox';
  areaCb.autocomplete = 'off';
  areaCb.className = 'cinema-area-toggle';
  areaCb.onclick = e => e.stopPropagation();          // toggle the area, don't fold
  areaCb.onchange = () => toggleArea(area, areaCb.checked);

  const name = document.createElement('span');
  name.textContent = area.name;

  const chevron = document.createElement('span');
  chevron.className = 'submenu-chevron';
  chevron.innerHTML = '&#8250;';
  const right = document.createElement('span');
  right.className = 'submenu-right';
  right.appendChild(chevron);

  // The fold control is the name + chevron BESIDE the checkbox — never a
  // `role="button"` around it, which would nest one control in another.
  const fold = document.createElement('span');
  fold.className = 'cinema-area-fold';
  fold.style.cssText = 'display:flex; flex:1; align-items:center; gap:6px; min-width:0';
  fold.appendChild(name);
  fold.appendChild(right);

  header.appendChild(areaCb);
  header.appendChild(fold);

  const body = document.createElement('div');
  body.className = 'submenu-list cinema-area-cinemas';
  body.style.display = 'none';                          // collapsed by default
  seasonOrdered(area.cinemas).forEach(c => body.appendChild(buildCinemaRow(c)));

  makeFoldHeader(fold, body);
  // The rest of the row (its padding) still folds on a click, as it always did.
  header.onclick = e => { if (!fold.contains(e.target)) fold.click(); };

  group.appendChild(header);
  group.appendChild(body);
  return group;
}

// Split cities (`window.CINEMA_AREAS` non-empty — e.g. London) render one
// collapsible group per area; flat cities render the plain cinema list.
function buildCinemaPanel() {
  const list = document.getElementById('cinema-list');
  // Pages without the picker have no `#cinema-list` — bail instead of
  // throwing on the null `list`.
  if (!list) return;
  list.innerHTML = '';
  const areas = window.CINEMA_AREAS || [];
  if (areas.length) {
    areas.forEach(area => list.appendChild(buildAreaGroup(area)));
  } else {
    seasonOrdered(ALL_CINEMAS).forEach(cinema => list.appendChild(buildCinemaRow(cinema)));
  }
  syncAreaCheckboxes();
  syncAllCheckbox();
}

// Re-derive every rendered cinema checkbox's checked state from the current
// `disabledCinemas`, in place — so an area/master toggle updates the rows
// without rebuilding (which would collapse any expanded area folds).
function refreshCinemaCheckboxes() {
  const disabled = getDisabledCinemas();
  document.querySelectorAll('#cinema-list input[data-cinema]').forEach(cb => {
    cb.checked = !disabled.includes(cb.dataset.cinema);
  });
}

// Each area checkbox is checked when none of its cinemas are disabled,
// indeterminate when only some are — the area-level mirror of syncAllCheckbox.
function syncAreaCheckboxes() {
  const disabled = getDisabledCinemas();
  const areas = window.CINEMA_AREAS || [];
  document.querySelectorAll('.cinema-area-group').forEach(group => {
    const area = areas.find(a => a.slug === group.dataset.areaSlug);
    const cb = group.querySelector('.cinema-area-toggle');
    if (!area || !cb) return;
    const disabledCount = area.cinemas.filter(c => disabled.includes(c)).length;
    cb.checked = disabledCount === 0;
    cb.indeterminate = disabledCount > 0 && disabledCount < area.cinemas.length;
  });
}

// (De)select every cinema in one area, leaving other areas / other cities
// untouched. Updates the rows in place so the fold state survives.
function toggleArea(area, checked) {
  const rest = getDisabledCinemas().filter(c => !area.cinemas.includes(c));
  setDisabledCinemas(checked ? rest : rest.concat(area.cinemas));
  refreshCinemaCheckboxes();
  syncAreaCheckboxes();
  syncAllCheckbox();
  updateFormatBtn();
  applyFilters();
}

function syncAllCheckbox() {
  const allCb = document.getElementById('cinema-all');
  if (!allCb) return;  // pages without the picker have no Wszystkie-kina checkbox.
  const disabled = disabledCinemasInCity();
  allCb.checked = disabled.length === 0;
  allCb.indeterminate = disabled.length > 0 && disabled.length < ALL_CINEMAS.length;
}

// Cinema picker now lives inside the Filtry dropdown; no standalone
// open/close handler. `buildCinemaPanel` populates the same `#cinema-list`
// element (just re-parented into `#format-panel`), so no changes needed
// there. The cinema-count summary surfaces in `updateFormatBtn` instead.

function toggleAllCinemas(checked) {
  // Only flip THIS city's cinemas; leave deselections made in other cities
  // untouched (the list is global — see `disabledCinemasInCity`). "Select
  // all" drops just this city's entries; "deselect all" adds every cinema of
  // this city on top of whatever other cities already disabled. Update the
  // rows in place (not a rebuild) so expanded area folds stay open.
  const others = disabledCinemasElsewhere();
  setDisabledCinemas(checked ? others : others.concat(ALL_CINEMAS));
  refreshCinemaCheckboxes();
  syncAreaCheckboxes();
  syncAllCheckbox();
  updateFormatBtn();
  applyFilters();
}

// A cinema of THIS city switched off narrows the listing: it lights the funnel,
// and "Wyczyść" switches every one back on. Only where the picker is rendered.
registerNarrowing(
  () => !!document.getElementById('cinema-list') && disabledCinemasInCity().length > 0,
  () => { if (document.getElementById('cinema-list')) toggleAllCinemas(true); });

// ── First-visit area picker (split cities) ─────────────────────────────────
//
// When a split city (e.g. London) is opened for the first time, ask which
// areas to show — all pre-selected (default: everything), matching the flat
// city's "all cinemas enabled" default. Unchecking an area disables its
// cinemas. Shown once per city (a localStorage flag), and never on a flat
// city. Built entirely in JS so it adds nothing to the server-rendered HTML
// and only exists when actually shown.
//
// A master row heads the list, the same control the Filtry cinema panel puts
// above `#cinema-list` (`#cinema-all` / `toggleAllCinemas`): checked flips
// every area on, unchecked flips every area off, indeterminate while only
// some are picked. One control rather than two buttons because a metro like
// the Bay Area has enough areas that "clear them all, then tick the one I
// want" is the fastest way through the sheet — and because it is the control
// this app already uses for exactly this job one screen over.
function areaPickerKey() { return 'areasChosen:' + CURRENT_CITY; }

function maybeShowAreaPicker() {
  if (!(window.CINEMA_AREAS || []).length) return;          // flat city
  try { if (localStorage.getItem(areaPickerKey())) return; } catch (e) {}
  showAreaPicker();
}

function showAreaPicker() {
  const areas = window.CINEMA_AREAS || [];
  if (!areas.length || document.getElementById('area-picker-overlay')) return;

  // A `.modal-dialog` (`_sharedStyles`): focus held in it, the page behind
  // inert. Escape closes it for this visit without remembering a choice, so
  // it asks again on the next one.
  const overlay = document.createElement('dialog');
  overlay.id = 'area-picker-overlay';
  overlay.className = 'modal-dialog';
  overlay.style.padding = '20px';
  overlay.addEventListener('close', () => overlay.remove());
  const modal = document.createElement('div');
  modal.style.cssText = 'width:100%;max-width:420px;max-height:85vh;overflow:auto;background:#14142e;' +
    'border:1px solid #2a2a5e;border-radius:14px;padding:20px;';
  appendAreaPickerHeading(modal);

  const allCb = areaPickerRow(modal, window.t('areaPicker.all') || 'All areas', null);
  allCb.id = 'area-picker-all';
  const boxes = areas.map(area => {
    const cb = areaPickerRow(modal, area.name, area.cinemas.length);
    cb.dataset.areaSlug = area.slug;
    return { cb, area };
  });

  // Select all / deselect all. A click on a partly-filled box reads
  // `checked === true`, so it selects all first — what a user reaching for it
  // while some are ticked means. `indeterminate` is cleared explicitly: the
  // browser does that itself on a real click, but not when the state is set
  // programmatically.
  allCb.onchange = () => {
    allCb.indeterminate = false;
    boxes.forEach(({ cb }) => { cb.checked = allCb.checked; });
  };
  const syncAreaPickerAll = () => {
    const picked = boxes.filter(({ cb }) => cb.checked).length;
    allCb.checked = picked === boxes.length;
    allCb.indeterminate = picked > 0 && picked < boxes.length;
  };
  boxes.forEach(({ cb }) => { cb.onchange = syncAreaPickerAll; });

  modal.appendChild(areaPickerConfirm(overlay, boxes));
  overlay.appendChild(modal);
  document.body.appendChild(overlay);
  overlay.showModal();
}

function appendAreaPickerHeading(modal) {
  const title = document.createElement('h2');
  title.style.cssText = 'font-size:1.25rem;margin:0 0 .25rem;';
  title.textContent = window.t('areaPicker.title') || 'Choose areas';
  const sub = document.createElement('p');
  sub.style.cssText = 'color:#9a9ac0;font-size:.9rem;margin:0 0 1rem;';
  sub.textContent = window.t('areaPicker.subtitle') || '';
  modal.appendChild(title);
  modal.appendChild(sub);
}

// One row of the area picker, its checkbox (returned) pre-checked: the master
// row (bold, no count) when `count` is null, else an area with its cinema count.
function areaPickerRow(modal, name, count) {
  const label = document.createElement('label');
  label.style.cssText = 'display:flex;align-items:center;gap:.6rem;padding:.6rem .2rem;' +
    'font-size:1.05rem;border-bottom:1px solid #23234d;cursor:pointer;' + (count === null ? 'font-weight:600;' : '');
  const cb = document.createElement('input');
  cb.type = 'checkbox';
  cb.autocomplete = 'off';
  cb.checked = true;                                         // all pre-selected
  const nameSpan = document.createElement('span');
  nameSpan.textContent = name;
  label.appendChild(cb);
  label.appendChild(nameSpan);
  if (count !== null) {
    const countSpan = document.createElement('span');
    countSpan.style.cssText = 'margin-left:auto;color:#6f6f9c;font-size:.85rem;';
    countSpan.textContent = count;
    label.appendChild(countSpan);
  }
  modal.appendChild(label);
  return cb;
}

// The picker's confirm button: disables the cinemas of every UNCHECKED area,
// leaving other cities' entries untouched (default, all checked: nothing
// disabled), and remembers that this city's areas were chosen.
function areaPickerConfirm(overlay, boxes) {
  const btn = document.createElement('button');
  btn.style.cssText = 'margin-top:1.1rem;width:100%;padding:.8rem;font:inherit;font-size:1rem;' +
    'font-weight:600;color:#0d0d22;background:#e7b93a;border:none;border-radius:10px;cursor:pointer;';
  btn.textContent = window.t('areaPicker.confirm') || 'Show';
  btn.onclick = () => {
    const disable = [];
    boxes.forEach(({ cb, area }) => { if (!cb.checked) disable.push(...area.cinemas); });
    setDisabledCinemas(disabledCinemasElsewhere().concat(disable));
    try { localStorage.setItem(areaPickerKey(), '1'); } catch (e) {}
    overlay.remove();
    buildCinemaPanel();
    updateFormatBtn();
    applyFilters();
  };
  return btn;
}

// ── Empty state ───────────────────────────────────────────────────────────

function updateEmptyState(visibleCount) {
  const noFilms = document.getElementById('no-films');
  // Every cinema the filter leaves is out of season: say so, and when its season runs,
  // not that there is nothing on.
  const disabled = getDisabledCinemas();
  const left = ALL_CINEMAS.filter(c => !disabled.includes(c));
  if (visibleCount === 0 && left.length && left.every(isOffSeason)) {
    noFilms.removeAttribute('data-i18n');
    noFilms.textContent = '';
    noFilms.appendChild(i18nSpan('season.outOfSeason', left.map(c => CINEMA_PILLS[c] || c).join(', ')));
    noFilms.appendChild(document.createTextNode(' ('));
    appendSeasonWindow(noFilms, cinemaSeason(left[0]));
    noFilms.appendChild(document.createTextNode(')'));
    noFilms.style.display = '';
  } else if (visibleCount === 0) {
    noFilms.setAttribute('data-i18n', 'empty.repertoire');
    noFilms.textContent = window.t('empty.repertoire');
    noFilms.style.display = '';
  } else {
    noFilms.style.display = 'none';
  }
}
