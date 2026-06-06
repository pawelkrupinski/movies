  // Page-level flags surfaced by the server-rendered template so the JS
  // can decide where to read/write personalization state. When LOGGED_IN
  // is true the boot path merges localStorage with the server's state and
  // every subsequent write debounces a PUT to /api/me/state; otherwise
  // localStorage is canonical and (Phase D) a once-per-day toast nags
  // anonymous users.

  // ── Format-filter dropdown ────────────────────────────────────────────────

  // Three axes live inside one panel: Wymiar (radio, 2D/3D), Wersja (radio,
  // NAP/DUB), IMAX (checkbox). Each visible badge must carry every selected
  // token on its data-format attribute. Empty selection on an axis = no
  // constraint for that axis.
  function getFormatFilter() {
    const dim  = (document.querySelector('input[name="format-dim"]:checked')  || {}).value || '';
    const lang = (document.querySelector('input[name="format-lang"]:checked') || {}).value || '';
    // The Filtry panel (and the `#format-imax` checkbox inside it) isn't
    // rendered on /ulubione — the page intentionally has no filter UI.
    // Treat a missing checkbox as "unchecked / no IMAX filter applied"
    // rather than throwing.
    const imaxEl = document.getElementById('format-imax');
    const imax = imaxEl && imaxEl.checked ? 'IMAX' : '';
    return [dim, lang, imax].filter(Boolean);
  }

  function badgeFormatTokens(badge) {
    return new Set((badge.dataset.format || '').split(' ').filter(Boolean));
  }

  // From-hour filter: returns the lower-bound time as minutes-since-midnight
  // (e.g. 18:30 → 1110), or null when the hour dropdown is set to "Dowolna"
  // (any). Both dropdowns are read together; the minute default is 00.
  function getFromMinutes() {
    const hStr = (document.getElementById('from-hour') || {}).value;
    if (hStr == null || hStr === '') return null;
    const h = parseInt(hStr, 10);
    const m = parseInt((document.getElementById('from-minute') || {}).value, 10) || 0;
    return h * 60 + m;
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
    var el = document.getElementById('sort-by');
    var v = el ? el.value : 'earliest';
    return (v === 'rating') ? v : 'earliest';
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
  // pages that render the navbar but have no real grid (/plan's stub
  // applyFilters is harmless, but a page without one at all mustn't throw).
  function onSortChange() {
    if (typeof applyFilters === 'function') applyFilters();
  }

  // City picker (Filtry → Miasto) changed — remember the choice so the bare
  // `/` landing bounces here next time, then navigate to the chosen city's
  // repertoire root. A full navigation (not a view-swap) because the whole
  // corpus changes. No-op when the value is the current city.
  function onCityChange(slug) {
    if (!slug || slug === CURRENT_CITY) return;
    document.cookie = 'city=' + slug + ';path=/;max-age=' + (60 * 60 * 24 * 365);
    window.location.href = '/' + slug + '/';
  }
  window.onCityChange = onCityChange;

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
    const vp = _viewportMeta();
    if (!vp || _searchBaseViewport !== null) return;   // already locked — no-op
    _searchBaseViewport = vp.getAttribute('content');
    vp.setAttribute('content', _searchBaseViewport + ', maximum-scale=1');
  }
  function unlockSearchZoom() {
    const vp = _viewportMeta();
    if (!vp || _searchBaseViewport === null) return;
    vp.setAttribute('content', _searchBaseViewport);
    _searchBaseViewport = null;
  }
  window.lockSearchZoom   = lockSearchZoom;
  window.unlockSearchZoom = unlockSearchZoom;

  function updateFormatBtn() {
    const parts = [...getFormatFilter()];
    const fromMin = getFromMinutes();
    if (fromMin !== null) {
      const h = Math.floor(fromMin / 60), m = fromMin % 60;
      parts.push('od ' + String(h).padStart(2, '0') + ':' + String(m).padStart(2, '0'));
    }
    // Cinema filter is now part of the Filtry panel on most pages;
    // surface the selected-vs-total count in the button label so the
    // navbar still signals "you've narrowed the cinemas" at a glance.
    // On /kina the picker is rendered as a pill row above the grid and
    // the Filtry panel doesn't carry the Kina section at all — the
    // `#cinema-list` presence is the natural signal for "Filtry owns
    // the picker on this page". When it's absent, the count belongs to
    // the pill row, not the Filtry label.
    if (document.getElementById('cinema-list')) {
      const disabled = disabledCinemasInCity();
      if (disabled.length > 0 && disabled.length < ALL_CINEMAS.length) {
        parts.push('kina ' + (ALL_CINEMAS.length - disabled.length) + '/' + ALL_CINEMAS.length);
      }
    }
    var sm = getSubmenuSummaries();
    if (sm.country)  parts.push(sm.country);
    if (sm.director) parts.push(sm.director);
    if (sm.cast)     parts.push(sm.cast);
    const btn = document.getElementById('format-filter-btn');
    if (!btn) return;  // /ulubione: Filtry button not rendered.
    btn.textContent = parts.length === 0 ? 'Filtry' : 'Filtry (' + parts.join(', ') + ')';
  }

  function onFormatChange() {
    updateFormatBtn();
    applyFilters();
  }

  // Hide every navbar dropdown except (optionally) one — used by each toggle handler
  // so opening any panel closes all the others.
  function closeOtherPanels(except) {
    document.querySelectorAll('.dropdown-panel').forEach(p => {
      if (p !== except) p.style.display = 'none';
    });
  }

  function toggleFormatPanel(event) {
    event.stopPropagation();
    const panel = document.getElementById('format-panel');
    const opening = panel.style.display === 'none';
    if (opening) ensureSubmenuPanels();   // lazily build the grid-scanned lists
    closeOtherPanels(opening ? panel : null);
    panel.style.display = opening ? 'block' : 'none';
    if (opening) clampPanel(panel);
  }

  function resetFormatFilter() {
    document.querySelector('input[name="format-dim"][value=""]').checked  = true;
    document.querySelector('input[name="format-lang"][value=""]').checked = true;
    document.getElementById('format-imax').checked   = false;
    document.getElementById('from-hour').value       = '';
    document.getElementById('from-minute').value     = '0';
    var sortSel = document.getElementById('sort-by');
    if (sortSel) sortSel.value = 'earliest';
    ['country', 'genre', 'director', 'cast'].forEach(function(key) {
      var list = document.getElementById(key + '-list');
      if (list) {
        list.querySelectorAll('input[type="checkbox"]').forEach(function(cb) { cb.checked = true; });
        list.style.display = 'none';
      }
      var chevron = document.getElementById(key + '-chevron');
      if (chevron) chevron.classList.remove('open');
      updateSubmenuCount(key);
    });
    document.getElementById('format-panel').style.display = 'none';
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
    var checked = boxes.filter(function(cb) { return cb.checked; });
    if (checked.length === boxes.length) return null;
    return checked.map(function(cb) { return cb.value; });
  }

  function getCountryFilter()  { return getSubmenuFilter('country'); }
  function getDirectorFilter() { return getSubmenuFilter('director'); }
  function getCastFilter()     { return getSubmenuFilter('cast'); }

  function toggleSubmenu(key) {
    var list = document.getElementById(key + '-list');
    var chevron = document.getElementById(key + '-chevron');
    if (!list) return;
    var opening = list.style.display === 'none';
    list.style.display = opening ? '' : 'none';
    if (chevron) chevron.classList.toggle('open', opening);
  }

  function updateSubmenuCount(key) {
    var list = document.getElementById(key + '-list');
    var badge = document.getElementById(key + '-row-count');
    if (!badge || !list) return;
    var boxes = [...list.querySelectorAll('input[type="checkbox"]:not(.submenu-all)')];
    var unchecked = boxes.filter(function(cb) { return !cb.checked; }).length;
    if (unchecked > 0) {
      badge.textContent = boxes.length - unchecked + '/' + boxes.length;
      badge.style.display = '';
    } else {
      badge.style.display = 'none';
    }
    var allCb = list.querySelector('.submenu-all');
    if (allCb) allCb.checked = unchecked === 0;
  }

  function getSubmenuSummaries() {
    var result = {};
    [['country', 'krajów'], ['genre', 'gat.'], ['director', 'reż.'], ['cast', 'aktorów'], ['room', 'sal']].forEach(function(pair) {
      var key = pair[0], suffix = pair[1];
      var filter = getSubmenuFilter(key);
      if (filter !== null) {
        // Room values are "Cinema|Room" composites — the cinema half is
        // implied by the rest of the badge context, so a single-selection
        // summary just shows the room label.
        if (key === 'room' && filter.length === 1) {
          var pipe = filter[0].indexOf('|');
          result[key] = pipe >= 0 ? filter[0].substring(pipe + 1) : filter[0];
        } else {
          result[key] = filter.length === 1 ? filter[0] : filter.length + ' ' + suffix;
        }
      }
    });
    return result;
  }

  // Root for grid-wide DOM scans. Normally the whole document, but on the
  // Filmy/Kina pages it's the active `#view-root` so that during the ~300ms
  // Filmy↔Kina slide-swap (when the outgoing and incoming grids briefly
  // coexist) these scans count only the incoming view — the outgoing root has
  // had its id stripped by `navigateTo`, so `getElementById('view-root')`
  // resolves to the incoming one. Falls back to `document` on pages with no
  // `#view-root` (e.g. /plan), preserving their behaviour unchanged.
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

  // Render a submenu's "Wszystkie" + per-value checkbox rows from a sorted
  // entries list. Shared by `buildSubmenuPanel` (country / director / cast,
  // values pulled from `.col` data attrs) and `buildRoomPanel` (values pulled
  // from `.badge-time[data-room]` × cinema-group, with a composite key
  // "Cinema|Room" that displays as "Cinema — Room").
  function renderSubmenuCheckboxes(key, entries) {
    var list = document.getElementById(key + '-list');
    if (!list) return;
    list.innerHTML = '';

    var allLabel = document.createElement('label');
    allLabel.className = 'panel-label';
    allLabel.style.borderBottom = '1px solid #3a3a6e';
    allLabel.style.marginBottom = '4px';
    allLabel.style.paddingBottom = '8px';
    var allCb = document.createElement('input');
    allCb.type = 'checkbox';
    allCb.checked = true;
    allCb.className = 'submenu-all';
    allCb.onchange = function() {
      list.querySelectorAll('input[type="checkbox"]:not(.submenu-all)').forEach(function(cb) { cb.checked = allCb.checked; });
      updateSubmenuCount(key); updateFormatBtn(); applyFilters();
    };
    allLabel.appendChild(allCb);
    allLabel.appendChild(document.createTextNode(' Wszystkie'));
    list.appendChild(allLabel);

    entries.forEach(function(entry) {
      var label = document.createElement('label');
      label.className = 'panel-label';
      var cb = document.createElement('input');
      cb.type = 'checkbox';
      cb.value = entry.value;
      cb.checked = true;
      cb.onchange = function() { updateSubmenuCount(key); updateFormatBtn(); applyFilters(); };
      label.appendChild(cb);
      label.appendChild(document.createTextNode(' ' + entry.label));
      var cnt = document.createElement('span');
      cnt.className = 'submenu-film-count';
      cnt.textContent = '(' + entry.count + ')';
      label.appendChild(cnt);
      list.appendChild(label);
    });
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
    ['country', 'genre', 'director', 'cast', 'room'].forEach(ensurePanel);
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

    var byCinema = {};
    gridScope().querySelectorAll('.cinema-group[data-cinema]').forEach(function(cg) {
      var cinema = cg.dataset.cinema;
      cg.querySelectorAll('.badge-time[data-room]').forEach(function(b) {
        var room = b.dataset.room;
        if (!room) return;
        if (!byCinema[cinema]) byCinema[cinema] = {};
        byCinema[cinema][room] = (byCinema[cinema][room] || 0) + 1;
      });
    });

    var cinemas = Object.keys(byCinema).sort(function(a, b) { return a.localeCompare(b, 'pl'); });

    // Hide the Sale row entirely when no badge on the page carries `data-room`
    // (e.g. /ulubione, or a fixture day where the scrapers returned no rooms).
    var row = document.getElementById('room-row');
    if (row) row.style.display = cinemas.length === 0 ? 'none' : '';

    list.innerHTML = '';

    // Sale-level "Wszystkie" — flips every room checkbox at once. Same
    // semantics as the existing country/director/cast Wszystkie row; the
    // `submenu-all` class keeps it out of `getSubmenuFilter('room')` reads.
    var allLabel = document.createElement('label');
    allLabel.className = 'panel-label';
    allLabel.style.borderBottom = '1px solid #3a3a6e';
    allLabel.style.marginBottom = '4px';
    allLabel.style.paddingBottom = '8px';
    var allCb = document.createElement('input');
    allCb.type = 'checkbox';
    allCb.checked = true;
    allCb.className = 'submenu-all';
    allCb.onchange = function() {
      list.querySelectorAll('input[type="checkbox"]:not(.submenu-all)').forEach(function(cb) {
        cb.checked = allCb.checked;
      });
      list.querySelectorAll('.room-cinema-header').forEach(function(h) { _updateRoomCinemaCount(h); });
      updateSubmenuCount('room'); updateFormatBtn(); applyFilters();
    };
    allLabel.appendChild(allCb);
    allLabel.appendChild(document.createTextNode(' Wszystkie'));
    list.appendChild(allLabel);

    cinemas.forEach(function(cinema) {
      var rooms = Object.keys(byCinema[cinema]).sort(function(a, b) {
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
      var cnt = document.createElement('span');
      cnt.className = 'submenu-row-count room-cinema-count';
      cnt.style.display = 'none';
      right.appendChild(cnt);
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

      header.onclick = function() {
        var opening = inner.style.display === 'none';
        inner.style.display = opening ? '' : 'none';
        chevron.classList.toggle('open', opening);
      };

      rooms.forEach(function(room) {
        var label = document.createElement('label');
        label.className = 'panel-label';
        var cb = document.createElement('input');
        cb.type = 'checkbox';
        cb.value = cinema + '|' + room;
        cb.checked = true;
        cb.onchange = function() {
          _updateRoomCinemaCount(header);
          updateSubmenuCount('room'); updateFormatBtn(); applyFilters();
        };
        label.appendChild(cb);
        label.appendChild(document.createTextNode(' ' + room));
        var roomCnt = document.createElement('span');
        roomCnt.className = 'submenu-film-count';
        roomCnt.textContent = '(' + byCinema[cinema][room] + ')';
        label.appendChild(roomCnt);
        inner.appendChild(label);
      });

      list.appendChild(inner);
    });
  }

  // Per-cinema header badge inside Sale — surfaces "3/8" when 3 of 8 rooms
  // in that cinema are checked, so the user knows which cinemas they've
  // narrowed without expanding each one.
  function _updateRoomCinemaCount(headerEl) {
    var cnt = headerEl.querySelector('.room-cinema-count');
    if (!cnt) return;
    var inner = headerEl.nextElementSibling;
    if (!inner) return;
    var boxes = [...inner.querySelectorAll('input[type="checkbox"]')];
    var unchecked = boxes.filter(function(b) { return !b.checked; }).length;
    if (unchecked > 0) {
      cnt.textContent = (boxes.length - unchecked) + '/' + boxes.length;
      cnt.style.display = '';
    } else {
      cnt.style.display = 'none';
    }
  }

  // ── Hidden-films + disabled-cinemas storage ───────────────────────────────
  //
  // Backed by localStorage, with one-time migration from the legacy
  // cookie-based storage. Cookies were silently failing once `hiddenFilms`
  // grew past the ~4 KB per-cookie budget (cumulative with Play's session
  // cookie + `disabledCinemas`); the symptom was "click X, card stays
  // visible" — setHidden's write was being dropped by the browser.
  //
  // localStorage has a ~5 MB origin budget, doesn't get sent with every
  // request, and works the same across browsers.

  function _lsGet(key) {
    try {
      const raw = localStorage.getItem(key);
      if (raw === null) return null;
      return JSON.parse(raw);
    } catch { return null; }
  }

  function _lsSet(key, value) {
    try { localStorage.setItem(key, JSON.stringify(value)); } catch {}
  }

  function _cookieRead(name) {
    const match = document.cookie.match(new RegExp('(?:^|;\\s*)' + name + '=([^;]*)'));
    try { return match ? JSON.parse(decodeURIComponent(match[1])) : null; } catch { return null; }
  }

  function _cookieClear(name) {
    document.cookie = name + '=; expires=Thu, 01 Jan 1970 00:00:00 GMT; path=/; SameSite=Lax';
  }

  // One-shot migration: if localStorage is empty and the legacy cookie has a
  // value, lift it across and drop the cookie. Runs once per key; subsequent
  // page loads find localStorage already populated.
  function _migrate(key) {
    if (_lsGet(key) !== null) return;
    const legacy = _cookieRead(key);
    if (legacy !== null) {
      _lsSet(key, legacy);
      _cookieClear(key);
    }
  }
  _migrate('hiddenFilms');
  _migrate('disabledCinemas');

  function getHidden()           { return _lsGet('hiddenFilms')    || []; }
  function setHidden(titles)     { _lsSet('hiddenFilms',    titles); scheduleServerSync(); }
  function getDisabledCinemas()  { return _lsGet('disabledCinemas') || []; }
  function setDisabledCinemas(l) { _lsSet('disabledCinemas', l);    scheduleServerSync(); }
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
  // /plan-side state. `selectedMovies` is the inverse of `hiddenFilms`
  // (titles to schedule, not titles to skip). `favouriteRooms` are
  // composite `"<Cinema displayName>|<Room>"` keys — same shape the
  // grid pages' `data-room` badges and the Filtry → Sale list use, so
  // a future "carry /plan's room picks into / and /kina" cross-page
  // affordance can read the same set without a translation step.
  function getSelectedMovies()   { return _lsGet('selectedMovies') || []; }
  function setSelectedMovies(l)  { _lsSet('selectedMovies', l);    scheduleServerSync(); }
  function getFavouriteRooms()   { return _lsGet('favouriteRooms') || []; }
  function setFavouriteRooms(l)  { _lsSet('favouriteRooms', l);    scheduleServerSync(); }
  window.getSelectedMovies = getSelectedMovies;
  window.setSelectedMovies = setSelectedMovies;
  window.getFavouriteRooms = getFavouriteRooms;
  window.setFavouriteRooms = setFavouriteRooms;

  // Delegated click handler for hide-film buttons and card-tap navigation.
  // Sentry caught `e.target.closest is not a function` on Chrome Mobile —
  // browsers can fire delegated events with a Text node (no `closest`) or
  // even `document` as the target, e.g. on synthetic dispatches or when a
  // pointer leaves between hit-test and dispatch. Gate every handler on
  // `Element` so the chain bails cleanly instead of crashing.
  document.addEventListener('click', e => {
    if (!(e.target instanceof Element)) return;
    const hide = e.target.closest('.hide-btn');
    if (hide) { hideFilm(hide); return; }
    if (e.target.closest('a, button, .showings-more')) return;
    const card = e.target.closest('.card');
    if (card) {
      const col = card.closest('.col[data-title]');
      if (col) window.location.href = CITY_BASE + '/film?title=' + encodeURIComponent(col.dataset.title);
    }
  });


  // ── Showings truncation ─────────────────────────────────────────────────
  //
  // After each filter pass, caps visible showings per card at ~10 visual
  // rows. Hides overflow at cinema-group boundaries and shows a
  // "… +N seansów" link to the /film page. Mirrors the iOS app's collapse.
  //
  // Called from applyFilters() in repertoire + kina after visibility has
  // been set on badges / groups. Walks the already-computed visibility —
  // no extra DOM measurement. The /film page has no applyFilters and
  // doesn't call this, so it renders everything.

  const _MAX_SHOWINGS_ROWS = 10;
  const _PILLS_PER_ROW     = 6;
  const _MIN_HIDDEN         = 3;

  function _showtimeNoun(n) {
    if (n === 1) return 'seans';
    const mod10 = n % 10, mod100 = n % 100;
    if (mod10 >= 2 && mod10 <= 4 && (mod100 < 12 || mod100 > 14)) return 'seanse';
    return 'seansów';
  }

  function undoTruncation() {
    document.querySelectorAll('.date-group, .cinema-group').forEach(el => {
      if (el._truncated) { el.style.display = ''; el._truncated = false; }
    });
  }
  window.undoTruncation = undoTruncation;

  function truncateShowings(cardEl, hasCinemaHeaders) {
    const link = cardEl.querySelector('.showings-more');
    if (!link) return;

    const dateGroups = cardEl.querySelectorAll('.date-group');

    let lineCount = 0;
    let hidden = 0;
    let capped = false;

    for (const dg of dateGroups) {
      if (dg.style.display === 'none') continue;

      const cinemaGroups = dg.querySelectorAll('.cinema-group');
      let dayHasVisible = false;
      const dayLabelRow = 1;
      let dayLines = dayLabelRow;

      for (const cg of cinemaGroups) {
        if (cg.style.display === 'none') continue;

        const visibleBadges = [...cg.querySelectorAll('.badge-time')].filter(
          b => b.style.display !== 'none'
        ).length;
        if (visibleBadges === 0) continue;

        if (capped) {
          hidden += visibleBadges;
          cg.style.display = 'none';
          cg._truncated = true;
          continue;
        }

        const pillRows = Math.max(1, Math.ceil(visibleBadges / _PILLS_PER_ROW));
        const cinemaLines = (hasCinemaHeaders ? 1 : 0) + pillRows;

        if (lineCount + dayLines + cinemaLines <= _MAX_SHOWINGS_ROWS) {
          if (cg._truncated) { cg.style.display = ''; cg._truncated = false; }
          dayHasVisible = true;
          dayLines += cinemaLines;
        } else {
          hidden += visibleBadges;
          cg.style.display = 'none';
          cg._truncated = true;
          capped = true;
        }
      }

      if (dayHasVisible) {
        lineCount += dayLines;
      } else if (capped) {
        if (dg.style.display !== 'none') {
          dg.style.display = 'none';
          dg._truncated = true;
        }
      }
    }

    if (hidden > _MIN_HIDDEN) {
      link.textContent = '… +' + hidden + ' ' + _showtimeNoun(hidden);
      link.style.display = '';
    } else {
      if (hidden > 0) {
        for (const dg of dateGroups) {
          if (dg._truncated) { dg.style.display = ''; dg._truncated = false; }
          for (const cg of dg.querySelectorAll('.cinema-group')) {
            if (cg._truncated) { cg.style.display = ''; cg._truncated = false; }
          }
        }
      }
      link.style.display = 'none';
    }
  }

  function truncateAllShowings(hasCinemaHeaders) {
    gridScope().querySelectorAll('.col[data-title]').forEach(col =>
      truncateShowings(col, hasCinemaHeaders)
    );
  }

  window.truncateAllShowings = truncateAllShowings;


  // ── Hidden-films UI ───────────────────────────────────────────────────────
  //
  // The trigger lives as a row inside the Filtry dropdown
  // (`#hidden-row`). Tapping it opens `#hidden-modal-backdrop` — a
  // centred modal whose body lists every hidden title. Clicking a
  // title restores it but leaves the modal open (browse-and-toggle);
  // `Wyczyść` clears the whole set and closes the modal. `×` and a
  // backdrop click close without changing the set.

  function updateNavbar() {
    const hidden = getHidden();
    const row     = document.getElementById('hidden-row');
    const divider = document.getElementById('hidden-row-divider');
    const count   = document.getElementById('hidden-row-count');
    const list    = document.getElementById('hidden-modal-list');
    if (!row || !list) return;  // /ulubione: hidden-films UI not rendered.
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
    const q = input.value.trim().toLowerCase();
    document.querySelectorAll('#hidden-modal-list .panel-item').forEach(item => {
      item.style.display = q === '' || item.textContent.toLowerCase().includes(q) ? '' : 'none';
    });
  }

  function openHiddenModal(event) {
    if (event) event.stopPropagation();
    // Close the Filtry dropdown so the modal doesn't paint behind a half-
    // open dropdown panel on small screens. The other navbar dropdowns
    // get closed too — same idiom as `closeOtherPanels` for opening any
    // dropdown.
    closeOtherPanels(null);
    const m = document.getElementById('hidden-modal-backdrop');
    if (m) m.classList.add('open');
  }
  function closeHiddenModal() {
    const m = document.getElementById('hidden-modal-backdrop');
    if (m) m.classList.remove('open');
    // Reset the search on close so the next open shows the full list.
    const s = document.getElementById('hidden-modal-search');
    if (s) { s.value = ''; filterHiddenModal(); }
  }

  function clampPanel(panel) {
    panel.style.right = '0px';
    const rect = panel.getBoundingClientRect();
    if (rect.left < 8) panel.style.right = (rect.left - 8) + 'px';
  }

  // ── Auth dropdown + login modal ──────────────────────────────────────────
  //
  // Both surfaces stop event propagation so the document-level "click
  // outside → close" handler defined below doesn't fire-close them on
  // the same click that opens them.

  function toggleAuthMenu(event) {
    event.stopPropagation();
    const menu = document.getElementById('auth-menu');
    if (menu) menu.classList.toggle('open');
  }
  function closeAuthMenu() {
    const menu = document.getElementById('auth-menu');
    if (menu) menu.classList.remove('open');
  }

  function openLoginModal() {
    const m = document.getElementById('login-modal-backdrop');
    if (m) m.classList.add('open');
  }
  function closeLoginModal() {
    const m = document.getElementById('login-modal-backdrop');
    if (m) m.classList.remove('open');
  }
  // ESC closes any open dropdown / modal / menu — same UX every other
  // modal in the world has. `closeOtherPanels(null)` collapses every
  // `.dropdown-panel` (Filtry today; any future dropdown automatically)
  // so this stays correct as the UI grows.
  document.addEventListener('keydown', e => {
    if (e.key === 'Escape') {
      closeLoginModal();
      closeHiddenModal();
      closeAuthMenu();
      closeOtherPanels(null);
    }
  });

  function hideFilm(btn) {
    // No scrollY snapshot needed: the sort function's subsequence check
    // (see `isSubsequence` below) skips the DOM rebuild when cards only got
    // hidden, so the browser's scroll anchor stays put.
    const title = btn.closest('[data-title]').dataset.title;
    const hidden = getHidden();
    if (!hidden.includes(title)) {
      hidden.push(title);
      setHidden(hidden);
      maybeShowAnonymousNag();  // hide is the other "this will only stick on this device" action
    }
    applyFilters();
    updateNavbar();
  }

  function restoreFilm(title) {
    preserveScroll(() => {
      setHidden(getHidden().filter(t => t !== title));
      applyFilters();
      updateNavbar();
    });
  }

  function showAllFilms() {
    preserveScroll(() => {
      setHidden([]);
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

  // ── Cinema-filter panel ───────────────────────────────────────────────────

  function buildCinemaPanel() {
    const list = document.getElementById('cinema-list');
    // /kina hides the Filtry-side Kina section entirely (its pills row
    // owns the same state). Bail when the DOM isn't there instead of
    // throwing on the null `list`.
    if (!list) return;
    const disabled = getDisabledCinemas();
    list.innerHTML = '';
    ALL_CINEMAS.forEach(cinema => {
      const label = document.createElement('label');
      label.className = 'panel-label';
      const cb = document.createElement('input');
      cb.type = 'checkbox';
      cb.checked = !disabled.includes(cinema);
      cb.onchange = () => {
        const dis = getDisabledCinemas();
        if (cb.checked) {
          setDisabledCinemas(dis.filter(c => c !== cinema));
        } else {
          if (!dis.includes(cinema)) dis.push(cinema);
          setDisabledCinemas(dis);
        }
        syncAllCheckbox();
        updateFormatBtn();   // cinema count is part of the Filtry label now
        applyFilters();
      };
      label.appendChild(cb);
      label.appendChild(document.createTextNode(' ' + (CINEMA_PILLS[cinema] || cinema)));
      list.appendChild(label);
    });
    syncAllCheckbox();
  }

  function syncAllCheckbox() {
    const allCb = document.getElementById('cinema-all');
    if (!allCb) return;  // /kina has no Wszystkie-kina checkbox.
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
    // this city on top of whatever other cities already disabled.
    const others = disabledCinemasElsewhere();
    setDisabledCinemas(checked ? others : others.concat(ALL_CINEMAS));
    buildCinemaPanel();
    applyFilters();
  }

  // ── Room tooltip ──────────────────────────────────────────────────────────

  const _roomTooltip = document.createElement('div');
  _roomTooltip.id = 'room-tooltip';
  document.body.appendChild(_roomTooltip);

  function showRoomTooltip(badge) {
    _roomTooltip.textContent = badge.dataset.room;
    _roomTooltip.style.display = 'block';

    const badgeRect = badge.getBoundingClientRect();
    const tipW      = _roomTooltip.offsetWidth;
    const tipH      = _roomTooltip.offsetHeight;
    const cardLeft  = (badge.closest('.card') || badge).getBoundingClientRect().left;

    const left = Math.max(cardLeft, badgeRect.left + badgeRect.width / 2 - tipW / 2);
    _roomTooltip.style.left = left + 'px';
    _roomTooltip.style.top  = (badgeRect.top - tipH - 4) + 'px';
  }

  function hideRoomTooltip() { _roomTooltip.style.display = 'none'; }

  // Desktop hover. `Element` guard mirrors the click handler — a
  // mouseover targeted at a non-Element node (Text, document) used to
  // throw `e.target.closest is not a function` on Chrome Mobile and
  // abort the handler before the tooltip had a chance to hide.
  document.addEventListener('mouseover', e => {
    if (!(e.target instanceof Element)) { hideRoomTooltip(); return; }
    const b = e.target.closest('.badge-time[data-room]');
    if (b) showRoomTooltip(b); else hideRoomTooltip();
  });

  // Mobile long-press
  let _roomTimer = null;

  document.addEventListener('touchstart', e => {
    if (!(e.target instanceof Element)) return;
    const badge = e.target.closest('.badge-time[data-room]');
    if (!badge) return;
    _roomTimer = setTimeout(() => {
      showRoomTooltip(badge);
      setTimeout(hideRoomTooltip, 2000);
    }, 500);
  }, { passive: true });

  document.addEventListener('touchend',  () => { clearTimeout(_roomTimer); _roomTimer = null; }, { passive: true });
  document.addEventListener('touchmove', () => { clearTimeout(_roomTimer); _roomTimer = null; }, { passive: true });

  // ── Keyboard navigation ───────────────────────────────────────────────────

  document.addEventListener('keydown', e => {
    if (e.target.tagName === 'INPUT' || e.target.tagName === 'SELECT' || e.target.tagName === 'TEXTAREA') return;
    if (e.key === 'ArrowLeft')  stepDate(-1);
    if (e.key === 'ArrowRight') stepDate(1);
  });

  // ── Click outside an open dropdown: dismiss only, swallow everything else ──
  //
  // When a `.dropdown-panel` (Filtry) or the auth menu is open, a click
  // anywhere outside it must do exactly ONE thing: close it. Without this the
  // same click also bubbled on to the card-tap handler (which navigates to
  // `/film`) or followed whatever link sat under the cursor — so dismissing
  // the filter accidentally opened a page.
  //
  // We run in the CAPTURE phase (the `true` below) so this fires before any
  // bubble-phase handler and before a link's default navigation, then
  // `stopImmediatePropagation` + `preventDefault` so nothing else acts on the
  // click. Inside-clicks and trigger-clicks are left alone so the panel's own
  // controls (radios, cinema toggles, the Filtry button itself) keep working.
  // When nothing is open this is a no-op, so ordinary card taps and links
  // behave normally.
  //
  // Routing the close through `closeOtherPanels(null)` keeps it robust against
  // future additions/removals of dropdowns.
  document.addEventListener('click', e => {
    if (!(e.target instanceof Element)) return;
    const panelOpen = [...document.querySelectorAll('.dropdown-panel')]
      .some(p => p.style.display !== 'none');
    const menuOpen = !!document.querySelector('.auth-menu.open');
    if (!panelOpen && !menuOpen) return;
    // Clicks on a panel/menu or its trigger drive their own toggle logic.
    if (e.target.closest('.dropdown-panel, #format-filter-btn, .auth-menu')) return;
    e.preventDefault();
    e.stopImmediatePropagation();
    closeOtherPanels(null);
    closeAuthMenu();
  }, true);

  // ── Empty state ───────────────────────────────────────────────────────────

  function updateEmptyState(visibleCount) {
    const noFilms = document.getElementById('no-films');
    if (visibleCount === 0) {
      noFilms.textContent = 'Brak repertuaru.';
      noFilms.style.display = '';
    } else {
      noFilms.style.display = 'none';
    }
  }

  // ── Date helpers ──────────────────────────────────────────────────────────

  function isoAddDays(isoDate, n) {
    const [y, m, d] = isoDate.split('-').map(Number);
    return new Date(y, m - 1, d + n).toLocaleDateString('sv');
  }

  const DAY2   = ['Nie', 'Pon', 'Wto', 'Śro', 'Czw', 'Pią', 'Sob'];
  const MONTHS = ['stycznia', 'lutego', 'marca', 'kwietnia', 'maja', 'czerwca',
                  'lipca', 'sierpnia', 'września', 'października', 'listopada', 'grudnia'];

  let _cachedDay = null, _cachedToday, _cachedTomorrow, _cachedIn7Days;

  function dateBounds() {
    const today = new Date().toLocaleDateString('sv', { timeZone: 'Europe/Warsaw' });
    if (today !== _cachedDay) {
      _cachedDay      = today;
      _cachedToday    = today;
      _cachedTomorrow = isoAddDays(today, 1);
      _cachedIn7Days  = isoAddDays(today, 6);
    }
    return { today: _cachedToday, tomorrow: _cachedTomorrow, in7Days: _cachedIn7Days };
  }

  function populateDayOptions() {
    const select  = document.getElementById('date-filter');
    if (!select) return;  // /ulubione: no date filter UI in the navbar.
    const weekOpt = select.querySelector('option[value="week"]');
    const today   = new Date().toLocaleDateString('sv', { timeZone: 'Europe/Warsaw' });
    for (let i = 2; i <= 6; i++) {
      const iso = isoAddDays(today, i);
      const [y, mo, da] = iso.split('-').map(Number);
      const dow = new Date(y, mo - 1, da).getDay();
      const opt = document.createElement('option');
      opt.value = iso;
      opt.textContent = DAY2[dow] + ' ' + da + ' ' + MONTHS[mo - 1];
      select.insertBefore(opt, weekOpt);
    }
  }

  function stepDate(dir) {
    const sel  = document.getElementById('date-filter');
    const next = sel.selectedIndex + dir;
    if (next >= 0 && next < sel.options.length) { sel.selectedIndex = next; onDateChange(); }
  }

  // Single entry point for "the day selector changed" — funnels into
  // `applyFilters()` so date stepping rebuilds the visible grid the same
  // way every other filter does. `applyFilters` calls `syncDateToURL`
  // (date-only); the other filters wait for the explicit Copy button.
  function onDateChange() {
    applyFilters();
  }

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
  //   lang    — format-lang radio (NAP / DUB)
  //   imax    — format-imax checkbox ("1" when on)
  //   from    — from-hour:minute composite (HH:MM)
  //   sort    — sort-by select ('rating'; 'earliest' default omitted)
  //   country, director, cast, room — repeated `?key=value` entries listing the
  //             CHECKED items (the inclusion set). Omitted when every box is
  //             ticked — the no-filter default → empty URL. Previously stored
  //             UNCHECKED items, which made navigating onto /kina/<one cinema>
  //             carry the rooms of every other cinema into the URL.
  //   cinema  — same inclusion semantics: enabled cinemas (LS-backed; /-only,
  //             /kina uses its URL-path pin instead).
  //
  // Append-per-value (not a comma-joined string) so the browser does exactly
  // one round of percent-encoding — the previous shape ran `encodeURIComponent`
  // by hand and then `URLSearchParams.set` encoded the result a second time
  // (rooms like "Cinema City Kinepolis|Sala 1" ended up as
  // `Cinema%2520City%2520Kinepolis%257CSala%25201`).

  // Lightweight: only the day select is reflected back to the URL in real
  // time, so stepping through days keeps history meaningful without polluting
  // the URL with every cast/country tick.
  function syncDateToURL() {
    const dateSel = document.getElementById('date-filter');
    if (!dateSel) return;
    const url = new URL(window.location.href);
    if (dateSel.value === 'today') url.searchParams.delete('date');
    else url.searchParams.set('date', dateSel.value);
    history.replaceState(null, '', url.pathname + url.search + url.hash);
  }

  // Build (but don't apply) the URL that captures every current filter — used
  // by the Copy-link button. Returns the same `pathname + search + hash`
  // shape `history.replaceState` expects.
  function buildShareURL() {
    const url = new URL(window.location.href);
    const p   = url.searchParams;
    const setOrDel = (k, v) => { if (v) p.set(k, v); else p.delete(k); };

    const dateSel = document.getElementById('date-filter');
    if (dateSel) setOrDel('date', dateSel.value === 'today' ? '' : dateSel.value);

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
      const checked = boxes.filter(cb => cb.checked).map(cb => cb.value);
      if (checked.length === boxes.length) return;  // all-on default → empty URL
      checked.forEach(v => p.append(key, v));
    });

    // Cinema filter only lives in Filtry on / (the /kina page surfaces the
    // same axis as the URL-path pin and ignores `disabledCinemas`). Gate the
    // param on the picker's presence so /kina URLs stay clean.
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
  function copyFilterLinkToClipboard(btn) {
    const path = buildShareURL();
    history.replaceState(null, '', path);
    const url = window.location.origin + path;
    const done = () => {
      if (!btn) return;
      const prev = btn.textContent;
      btn.textContent = 'Skopiowano!';
      setTimeout(() => { btn.textContent = prev; }, 1500);
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

  function applyFiltersFromURL() {
    const p = new URLSearchParams(window.location.search);

    const dateSel = document.getElementById('date-filter');
    if (dateSel) {
      const val   = p.get('date');
      const ALLOW = ['today', 'tomorrow', 'week', 'anytime'];
      const isIso = val && /^\d{4}-\d{2}-\d{2}$/.test(val);
      if (val && (ALLOW.includes(val) || isIso)) {
        if (isIso && !Array.from(dateSel.options).some(o => o.value === val)) {
          // ISO date outside the populateDayOptions range (today+2..today+6):
          // add an option on the fly so the select reflects the chosen value
          // rather than silently snapping back to 'today'.
          const opt = document.createElement('option');
          opt.value = val;
          const [y, mo, da] = val.split('-').map(Number);
          const dow = new Date(y, mo - 1, da).getDay();
          opt.textContent = DAY2[dow] + ' ' + da + ' ' + MONTHS[mo - 1];
          const weekOpt = dateSel.querySelector('option[value="week"]');
          if (weekOpt) dateSel.insertBefore(opt, weekOpt); else dateSel.appendChild(opt);
        }
        dateSel.value = val;
      }
    }

    const search = document.getElementById('search-input');
    if (search) { const q = p.get('q'); if (q !== null) search.value = q; }

    const dim = p.get('dim');
    if (dim) {
      const el = document.querySelector('input[name="format-dim"][value="' + CSS.escape(dim) + '"]');
      if (el) el.checked = true;
    }
    const lang = p.get('lang');
    if (lang) {
      const el = document.querySelector('input[name="format-lang"][value="' + CSS.escape(lang) + '"]');
      if (el) el.checked = true;
    }
    const imaxEl = document.getElementById('format-imax');
    if (imaxEl && p.has('imax')) imaxEl.checked = p.get('imax') === '1';

    const fromParam = p.get('from');
    if (fromParam && /^\d{1,2}:\d{2}$/.test(fromParam)) {
      const [h, m] = fromParam.split(':');
      const hSel = document.getElementById('from-hour');
      const mSel = document.getElementById('from-minute');
      if (hSel) hSel.value = String(parseInt(h, 10));
      if (mSel) mSel.value = String(parseInt(m, 10));
    }

    const sortSel = document.getElementById('sort-by');
    if (sortSel) {
      const s = p.get('sort');
      if (s === 'rating' || s === 'earliest') sortSel.value = s;
    }

    // URL values are the INCLUSION set (checked items). Empty/absent → all
    // checked (the no-filter default, no-op). Tolerate legacy single-value
    // comma-lists by flattening on `,` so an old shared link still narrows
    // down rather than dropping into a single nonexistent value.
    ['country', 'genre', 'director', 'cast', 'room'].forEach(key => {
      const checked = p.getAll(key).flatMap(v => v.split(','));
      if (checked.length === 0) return;
      ensurePanel(key);   // a shared link references this filter → build it now
                          // so the checkboxes exist for the state to land on
      const list = document.getElementById(key + '-list');
      if (!list) return;
      const checkedSet = new Set(checked);
      list.querySelectorAll('input[type="checkbox"]:not(.submenu-all)').forEach(cb => {
        cb.checked = checkedSet.has(cb.value);
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
    // invert. /kina has no `#cinema-list`, so this branch no-ops there.
    if (document.getElementById('cinema-list') && p.has('cinema')) {
      const enabled = p.getAll('cinema').flatMap(v => v.split(','));
      setDisabledCinemas(ALL_CINEMAS.filter(c => !enabled.includes(c)));
      buildCinemaPanel();
    }

    updateFormatBtn();
  }

  function squareDateNavBtns() {
    const sel = document.getElementById('date-filter');
    if (!sel) return;  // /ulubione: no date filter UI in the navbar.
    const h = sel.offsetHeight;
    document.querySelectorAll('.date-nav-btn').forEach(b => {
      b.style.width  = h + 'px';
      b.style.height = h + 'px';
    });
  }

  // ── Server sync for logged-in users ──────────────────────────────────────
  //
  // When the user is logged in, localStorage is still the in-page truth
  // (every read goes there for zero-latency) but every write also
  // debounces a PUT to /api/me/state so the server stays in sync. The
  // boot path does a one-time GET + union merge with whatever's in
  // localStorage so the device's local favourites land on the account
  // immediately ("migrate on page entry" semantics from the spec) and
  // any state added on other devices appears here.

  let _serverSyncTimer = 0;
  function scheduleServerSync() {
    if (!IS_LOGGED_IN) return;
    clearTimeout(_serverSyncTimer);
    // 400ms — long enough to batch a burst of toggles (clicking through
    // 5 stars rapidly produces one PUT), short enough that closing the
    // tab right after a single click still gets the write through.
    _serverSyncTimer = setTimeout(pushStateToServer, 400);
  }

  function pushStateToServer() {
    fetch('/api/me/state', {
      method:  'PUT',
      headers: { 'Content-Type': 'application/json' },
      body:    JSON.stringify({
        hiddenFilms:     getHidden(),
        disabledCinemas: getDisabledCinemas(),
        selectedMovies:  getSelectedMovies(),
        favouriteRooms:  getFavouriteRooms()
      })
    }).catch(() => { /* offline / 401 — localStorage still has the write */ });
  }

  // Boot-time merge: pull server state, union with whatever's currently
  // in localStorage, write the union back to BOTH layers. Runs once on
  // every page load for logged-in users — cheap (one GET, one PUT, both
  // tiny payloads), idempotent (a no-change union still sends the same
  // payload; server-side last-write-wins is fine here), and exactly the
  // behaviour the spec asked for.
  async function bootMergeFromServer() {
    if (!IS_LOGGED_IN) return;
    try {
      const resp = await fetch('/api/me/state', { headers: { 'Accept': 'application/json' } });
      if (!resp.ok) return;
      const remote = await resp.json();
      const merge  = (local, srv) => [...new Set([...(local || []), ...(srv || [])])].sort();

      _lsSet('hiddenFilms',         merge(getHidden(),           remote.hiddenFilms));
      _lsSet('disabledCinemas',     merge(getDisabledCinemas(),  remote.disabledCinemas));
      _lsSet('selectedMovies',      merge(getSelectedMovies(),   remote.selectedMovies));
      _lsSet('favouriteRooms',      merge(getFavouriteRooms(),   remote.favouriteRooms));

      pushStateToServer();
      applyFilters();
    } catch (e) { /* network blew up — localStorage is still usable */ }
  }

  // ── Anonymous nag toast (once per day) ──────────────────────────────────
  //
  // First time an anonymous user favourites or hides something on a given
  // calendar day, surface a one-liner explaining their action is
  // browser-local. The toast self-dismisses on click of × or after 8s;
  // clicking the body opens the login modal. Skipped entirely when no
  // OAuth providers are configured (local dev with no secrets has nothing
  // to log into).

  function maybeShowAnonymousNag() {
    if (IS_LOGGED_IN || !HAS_OAUTH_PROVIDERS) return;
    const lastAt = parseInt(localStorage.getItem('lastAnonymousNagAt') || '0', 10);
    const dayMs  = 24 * 60 * 60 * 1000;
    if (Date.now() - lastAt < dayMs) return;
    localStorage.setItem('lastAnonymousNagAt', Date.now().toString());
    showAnonymousNag();
  }

  let _nagDismissTimer = 0;
  function showAnonymousNag() {
    const toast = document.getElementById('anon-nag-toast');
    if (!toast) return;
    toast.classList.add('open');
    clearTimeout(_nagDismissTimer);
    _nagDismissTimer = setTimeout(closeAnonymousNag, 8000);
  }
  function closeAnonymousNag() {
    const toast = document.getElementById('anon-nag-toast');
    if (toast) toast.classList.remove('open');
  }

  // ── Poster retry with exponential backoff ──────────────────────────────
  //
  // Mirrors the iOS PosterImage retry loop (RetryBackoff.swift). When all
  // fallback URLs are exhausted the img is hidden and a backoff timer
  // schedules a fresh attempt of the entire chain (primary + fallbacks).
  // Sequence: 2s, 6s, 18s, 54s, 162s (then 162s forever) — multiplier 3,
  // same as iOS. A `_kinowo_t=<gen>` cache-buster is appended on retries
  // so the browser (and any upstream CDN / weserv cache) doesn't serve a
  // stale failure. Returning to the tab (visibilitychange → visible)
  // resets the backoff cycle, matching iOS's scenePhase → .active reset.

  const _POSTER_RETRY_MAX = 4;

  function _posterDelay(attempt) {
    var c = Math.max(0, Math.min(attempt, _POSTER_RETRY_MAX));
    var v = 2;
    for (var i = 0; i < c; i++) v *= 3;
    return v;
  }

  function _posterCacheBust(url, gen) {
    if (gen === 0) return url;
    var sep = url.indexOf('?') === -1 ? '?' : '&';
    return url + sep + '_kinowo_t=' + gen;
  }

  function schedulePosterRetry(img) {
    var attempt = parseInt(img.dataset.retryAttempt || '0', 10);
    var delay = _posterDelay(attempt);
    img.dataset.retryAttempt = String(attempt + 1);
    img._posterTimer = setTimeout(function() { restartPosterChain(img); }, delay * 1000);
  }

  function restartPosterChain(img) {
    var gen = (parseInt(img.dataset.retryGen || '0', 10)) + 1;
    img.dataset.retryGen = String(gen);
    var original = img.dataset.originalSrc;
    var originalFb = img.dataset.originalFallbacks || '';
    if (originalFb) img.dataset.fallbacks = originalFb;
    else img.removeAttribute('data-fallbacks');
    img.style.display = '';
    img.nextElementSibling.style.display = 'none';
    img.src = _posterCacheBust(original, gen);
  }

  function cancelPosterRetry(img) {
    if (img._posterTimer) { clearTimeout(img._posterTimer); img._posterTimer = null; }
  }

  document.addEventListener('visibilitychange', function() {
    if (document.visibilityState !== 'visible') return;
    document.querySelectorAll('img[data-retry-attempt]').forEach(function(img) {
      cancelPosterRetry(img);
      img.dataset.retryAttempt = '0';
      restartPosterChain(img);
    });
  });

  // ── Init ──────────────────────────────────────────────────────────────────

  // Reflect the active view (`films`/`kina`) in the navbar tabs.
  function setActiveTab(view) {
    document.querySelectorAll('.navbar .nav-tab').forEach(a => {
      const href = a.getAttribute('href');
      if (href === VIEW_PATHS.films || href === VIEW_PATHS.kina) {
        a.classList.toggle('active', (view === 'films' && href === VIEW_PATHS.films) ||
                                     (view === 'kina'  && href === VIEW_PATHS.kina));
      }
    });
  }

  // The Filtry "Kina" cinema picker (`#filtry-cinema-section`) is owned by the
  // Filmy view — it drives `disabledCinemas`. The Kina view uses its own
  // pill-pin row instead, so while Kina is active we STASH the whole section
  // out of the DOM. That keeps `#cinema-list`-presence the single source of
  // truth for "Filtry owns the cinema picker" — `updateFormatBtn`,
  // `buildShareURL`, `applyFiltersFromURL`, `buildCinemaPanel`/`syncAllCheckbox`
  // all key off it and need no view-awareness of their own.
  let _filtryCinemaSection = null;
  function syncFiltryForView(view) {
    if (view === 'kina') {
      const section = document.getElementById('filtry-cinema-section');
      if (section) { _filtryCinemaSection = section; section.remove(); }
    } else if (_filtryCinemaSection && !document.getElementById('filtry-cinema-section')) {
      // Restore into its original slot (immediately before the country row).
      const countryRow = document.getElementById('country-row');
      if (countryRow) countryRow.parentNode.insertBefore(_filtryCinemaSection, countryRow);
      _filtryCinemaSection = null;
    }
  }

  // Everything that depends on the CURRENT view's grid DOM, factored out so it
  // can re-run after a Filmy↔Kina slide-swap (see `navigateTo`). Reads the
  // view-provided `window.buildIndex`/`applyFilters` (re-assigned by each
  // view's inline IIFE) and the optional per-view `window.__viewInit` hook
  // (Kina sets it to `buildCinemaPills`; Filmy clears it). Must run AFTER the
  // view's inline script has (re)defined those globals.
  // Re-inits everything tied to the CURRENT view's grid DOM. Deliberately does
  // NOT touch the active nav-tab — callers set that at the right moment
  // (immediately on a click/popstate, but only on COMMIT for a finger-drag, so
  // a half-dragged-then-cancelled gesture doesn't leave the wrong tab lit).
  function bootView() {
    const view = document.getElementById('view-root')?.dataset.view || 'films';
    syncFiltryForView(view);   // stash/restore Filtry cinema section BEFORE the
                               // panel/label builders read `#cinema-list`.
    buildIndex();
    // Cinema picker lives in the Filtry dropdown now — populate the list so the
    // first open of Filtry has the checkboxes ready (no-ops on Kina: section
    // stashed, so `#cinema-list` is absent). It's cheap (one row per cinema, no
    // grid scan), so it stays eager.
    buildCinemaPanel();
    // The grid-scanning submenu panels are built lazily (on Filtry-open) — just
    // drop the previous view's build-flags so they re-tally this view's grid.
    // `applyFiltersFromURL` below rebuilds any a shared link actually needs.
    resetSubmenuPanels();
    if (typeof window.__viewInit === 'function') window.__viewInit();
    // URL → controls AFTER the picker is built so the checkbox updates land on
    // real DOM nodes; then a single `applyFilters()` pass renders the view
    // already filtered.
    applyFiltersFromURL();
    updateFormatBtn();
    applyFilters();
  }
  window.bootView = bootView;

  // ── Filmy ↔ Kina slide-swap ────────────────────────────────────────────────
  //
  // The Filmy (/) and Kina (/kina) tabs switch views IN PLACE instead of doing
  // a full navigation: fetch the sibling page, slide the current `#view-root`
  // out and the new one in, swap the DOM, re-run the new view's inline script +
  // `bootView()`, and `pushState` the URL. The navbar/modals/this file are the
  // stable shell and never reload. A horizontal swipe on a phone triggers the
  // same path. Direct loads and `/kina/:cinema` still server-render normally.

  // Every page is under `/{city}/…`, so the swap-managed view paths carry the
  // current city's prefix. CURRENT_CITY is the global from `_sharedJsConfig`.
  const CITY_BASE      = '/' + CURRENT_CITY;
  const VIEW_PATHS     = { films: CITY_BASE + '/', kina: CITY_BASE + '/kina' };
  const PREFETCH_TTL_MS = 5 * 60 * 1000;
  const _prefetch      = new Map();   // path -> { html, ts }
  let   _swapping      = false;
  let   _finishSwap    = null;   // snaps the in-flight swap to its end on demand
  // Per-view vertical scroll memory. Filmy and Kina share one window scroll, so
  // without this every swap would land at the top. We snapshot the outgoing
  // view's `scrollY` on the way out and restore the destination's on the way in
  // — so each column "stays where you left it" instead of both resetting to 0.
  // First visit to a view has no entry → falls back to the top.
  const _viewScroll    = { films: 0, kina: 0 };
  // Swipe-gesture tuning (shared by the live drag-preview and the release
  // decision so the highlighted tab and where it actually lands stay in sync).
  const COMMIT_FRACTION   = 0.4;   // drag past this fraction of the width → commit
  const FLICK_VX          = 0.4;   // px/ms — a quick flick commits a shorter drag
  const FLICK_MIN_PX      = 24;    // ignore micro-flicks
  const SWIPE_DEADZONE_PX = 10;    // horizontal travel before we lock to a swipe
  // Axis lock is biased toward HORIZONTAL so a swipe that starts with a little
  // vertical jitter (a finger rolling on touchdown) isn't misread as a scroll
  // and killed. We only concede to vertical scrolling when it CLEARLY dominates;
  // a tie or a slight vertical lean keeps the gesture alive and waits for the
  // next move to disambiguate.
  const SWIPE_VBAIL_PX    = 16;    // vertical must travel at least this far to even consider bailing
  const SWIPE_VBIAS_RATIO = 1.6;   // …and beat horizontal by this factor → it's a real vertical scroll

  function viewOfPath(path) {
    let p = (path || location.pathname).split('?')[0].split('#')[0];
    // Strip the `/{city}` prefix so the in-city view check is city-agnostic.
    if (p === CITY_BASE) p = '/';
    else if (p.startsWith(CITY_BASE + '/')) p = p.slice(CITY_BASE.length);
    if (p === '/') return 'films';
    if (p === '/kina' || p.startsWith('/kina/')) return 'kina';
    return null;   // a route the swap doesn't manage (e.g. /plan)
  }

  function prefetchView(view) {
    const path = VIEW_PATHS[view];
    if (!path) return;
    const hit = _prefetch.get(path);
    if (hit && Date.now() - hit.ts < PREFETCH_TTL_MS) return;
    // The boot prefetch runs on the idle callback, which can fire just as the
    // page is navigating away (e.g. tapping a film card right after load). In
    // that torn-down context WebKit's `fetch()` THROWS synchronously ("…due to
    // access control checks") rather than rejecting — the `.catch` only handles
    // the async rejection, so without this try the throw escapes as an uncaught
    // pageerror. Swallow it: a prefetch that loses the race is a no-op anyway.
    try {
      fetch(path, { headers: { 'X-Requested-With': 'view-swap' } })
        .then(r => (r.ok ? r.text() : null))
        .then(html => { if (html) _prefetch.set(path, { html, ts: Date.now() }); })
        .catch(() => {});
    } catch (e) { /* page unloading — skip the prefetch */ }
  }

  async function fetchViewHtml(path) {
    const hit = _prefetch.get(path);
    if (hit && Date.now() - hit.ts < PREFETCH_TTL_MS) return hit.html;
    const resp = await fetch(path, { headers: { 'X-Requested-With': 'view-swap' } });
    if (!resp.ok) throw new Error('view fetch failed: ' + resp.status);
    const html = await resp.text();
    _prefetch.set(path, { html, ts: Date.now() });
    return html;
  }

  // <script>s inserted via importNode/innerHTML never execute — swap each for a
  // fresh node so the injected view's inline IIFE actually runs (re-assigning
  // window.buildIndex/applyFilters/__viewInit for the incoming view).
  function runScripts(root) {
    root.querySelectorAll('script').forEach(old => {
      const s = document.createElement('script');
      for (const a of old.attributes) s.setAttribute(a.name, a.value);
      s.textContent = old.textContent;
      old.replaceWith(s);
    });
  }

  // Switch to `destView` (path '/' or '/kina[/cinema]'). `direction` is 'left'
  // when the new view enters from the right (Filmy→Kina) or 'right'
  // (Kina→Filmy). `push` adds a history entry (false when called from popstate,
  // where the browser already moved the URL). `restoreScroll` lands the
  // destination at its remembered offset (tab click / back button); the
  // cold-cache swipe fallback passes false so it behaves like a live swipe and
  // leaves the scroll where the finger left it.
  async function navigateTo(path, destView, direction, push, restoreScroll = true) {
    // If a previous swap is still animating, snap it to its end first (removes
    // the outgoing node, clears the flag) so a fast follow-up — e.g. a back
    // button pressed mid-slide — isn't dropped, which would leave the URL and
    // the view out of sync.
    if (_swapping && _finishSwap) _finishSwap();
    const pager   = document.getElementById('view-pager');
    const current = document.getElementById('view-root');
    if (!pager || !current) { window.location = path; return; }
    if (_swapping || current.dataset.view === destView) return;
    _swapping = true;
    _viewScroll[current.dataset.view] = window.scrollY;   // remember where we left this column

    let incoming;
    try {
      const doc = new DOMParser().parseFromString(await fetchViewHtml(path), 'text/html');
      const found = doc.getElementById('view-root');
      if (!found) throw new Error('no #view-root in response');
      incoming = document.importNode(found, true);
    } catch (e) {
      window.location = path;   // graceful fallback to a real navigation
      return;
    }

    // URL first, so `bootView`→`applyFiltersFromURL` reads the DESTINATION's
    // query. Preserve the current query+hash across the switch (e.g. ?date=
    // persists Filmy↔Kina) — same as the drag-commit path, so a tab click and
    // a swipe behave identically. `path` is always a bare route for push=true
    // callers (the tab href / a sibling path); popstate passes push=false and
    // the browser has already set the URL, so this line doesn't run for it.
    if (push !== false) {
      history.pushState({ view: destView }, '', path + location.search + location.hash);
    }

    // Strip colliding ids from the OUTGOING root so the incoming view's
    // `document.getElementById(...)` and `gridScope()` resolve uniquely during
    // the overlap. The outgoing view's JS is inert from here — never re-queried,
    // so we discard the saved id list (only the drag path needs it, to restore
    // on a snap-back). Same helper as the drag path → one id list, one place.
    detachViewIds(current);

    const reduce = matchMedia('(prefers-reduced-motion: reduce)').matches;
    const exit   = direction === 'left' ? '-100%' : '100%';
    const enter  = direction === 'left' ? '100%'  : '-100%';

    pager.classList.add('view-sliding');
    incoming.classList.add('view-entering');     // position:absolute over the pager
    incoming.style.transform = 'translateX(' + enter + ')';
    pager.appendChild(incoming);

    // Run the incoming inline script + boot it WHILE off-screen, so it's already
    // filtered/sorted/pinned before it slides in (no unfiltered flash).
    runScripts(incoming);
    bootView();
    setActiveTab(destView);   // bootView no longer touches the tab; set it here

    let settled = false;
    const settle = () => {
      if (settled) return;            // guard the transitionend + timeout + snap paths
      settled = true;
      current.remove();
      incoming.classList.remove('view-entering');
      incoming.style.transform = '';
      pager.classList.remove('view-sliding');
      _swapping = false;
      _finishSwap = null;
    };
    _finishSwap = settle;

    if (reduce) {
      settle();
    } else {
      // Double-rAF so the browser paints the start transforms before the
      // transition to the resting positions begins.
      requestAnimationFrame(() => requestAnimationFrame(() => {
        current.style.transform  = 'translateX(' + exit + ')';
        incoming.style.transform = 'translateX(0)';
      }));
      let done = false;
      const onEnd = (e) => {
        if (e && e.target !== incoming) return;
        if (done) return;
        done = true;
        incoming.removeEventListener('transitionend', onEnd);
        settle();
      };
      incoming.addEventListener('transitionend', onEnd);
      setTimeout(onEnd, 450);   // fallback if transitionend is missed
    }

    if (restoreScroll) window.scrollTo(0, _viewScroll[destView] || 0);   // land where this column last was
    prefetchView(destView === 'kina' ? 'films' : 'kina');
  }

  // Intercept Filmy/Kina tab clicks — capture phase so we preempt the
  // bubble-phase close-panels / card-tap handlers.
  document.addEventListener('click', (e) => {
    if (!(e.target instanceof Element)) return;
    if (e.defaultPrevented || e.button !== 0 || e.metaKey || e.ctrlKey || e.shiftKey || e.altKey) return;
    const tab = e.target.closest('.navbar .nav-tab');
    if (!tab) return;
    const href = tab.getAttribute('href');
    if (href !== VIEW_PATHS.films && href !== VIEW_PATHS.kina) return;   // e.g. the Debug tab → normal nav
    const root = document.getElementById('view-root');
    if (!root) return;                               // not a swap-managed page
    e.preventDefault();
    const destView = href === VIEW_PATHS.films ? 'films' : 'kina';
    if (root.dataset.view === destView) return;
    navigateTo(href, destView, destView === 'kina' ? 'left' : 'right', true);
  }, true);

  window.addEventListener('popstate', () => {
    const root     = document.getElementById('view-root');
    const destView = viewOfPath(location.pathname);
    if (!root || !destView || root.dataset.view === destView) return;
    navigateTo(location.pathname + location.search, destView,
               destView === 'kina' ? 'left' : 'right', false);
  });

  // ── Swipe to switch (phones): real finger-tracking ──────────────────────────
  //
  // During a horizontal drag the current view follows the finger and the
  // sibling tracks in from the edge (a real pager); on release we animate to
  // commit (past ~35% of the width) or snap back. All listeners are passive so
  // vertical scrolling is never blocked — a vertical-dominant gesture is
  // abandoned on the first move (and `touch-action: pan-y` on the grid keeps
  // the browser from cancelling a horizontal drag as a scroll). A drag that
  // STARTS on the cinema-pill strip is ignored so the strip's own horizontal
  // scroll wins ("pills win").
  //
  // Tracking needs the sibling in the DOM for the whole drag, so it engages
  // only when the sibling is already prefetched (warm by boot). Cold cache →
  // fall back to a threshold→navigateTo switch on release.
  let _drag = null;

  function detachViewIds(root) {
    const saved = [{ el: root, id: root.id }];
    root.removeAttribute('id');
    ['film-grid', 'no-films', 'cinema-pills'].forEach(id => {
      const el = root.querySelector('#' + id);
      if (el) { saved.push({ el, id }); el.removeAttribute('id'); }
    });
    return saved;
  }
  function restoreViewIds(saved) { saved.forEach(s => { s.el.id = s.id; }); }

  // Mount the prefetched sibling off-screen, make it the live (filtered)
  // #view-root, and freeze the outgoing as an id-less static visual. Returns
  // the drag context, or null when the sibling isn't warm in the cache.
  function beginDrag(destView, w) {
    const hit = _prefetch.get(VIEW_PATHS[destView]);
    if (!hit || Date.now() - hit.ts >= PREFETCH_TTL_MS) return null;
    let incoming;
    try {
      const found = new DOMParser().parseFromString(hit.html, 'text/html').getElementById('view-root');
      if (!found) return null;
      incoming = document.importNode(found, true);
    } catch (e) { return null; }

    const pager   = document.getElementById('view-pager');
    const current = document.getElementById('view-root');
    const dir     = destView === 'kina' ? 'left' : 'right';
    const enter   = dir === 'left' ? w : -w;

    const saved = detachViewIds(current);          // outgoing → frozen visual
    // Snapshot the outgoing view's globals (functions carry their closures, so
    // its built INDEX + live state like `_kinaPinned` survive) — restored on a
    // snap-back instead of re-running its script, which would reset that state.
    const globals = {
      buildIndex:   window.buildIndex,
      applyFilters: window.applyFilters,
      viewInit:     window.__viewInit,
      kinaPinned:   window._kinaPinned,
    };
    incoming.classList.add('view-entering');
    incoming.style.transform = 'translateX(' + enter + 'px)';
    pager.appendChild(incoming);
    runScripts(incoming);   // re-assign window.* to the incoming view…
    bootView();             // …then build + filter it (now the sole #view-root).
                            // No setActiveTab — the tab flips only on commit.
    return { pager, current, incoming, dir, enter, w, destView,
             originView: current.dataset.view, saved, globals };
  }

  function settleDrag(ctx, commit) {
    const { pager, current, incoming, dir, enter, w, destView, originView, saved, globals } = ctx;
    // A drag is horizontal, so window.scrollY is still the origin column's
    // position — record it for a later tab-nav restore. The swipe itself does
    // NOT reposition the scroll on commit: the incoming pane has tracked the
    // finger at this same offset throughout the drag, so leaving it put keeps
    // the gesture visually continuous (a scrollTo here would jump it).
    if (commit) _viewScroll[originView] = window.scrollY;
    const finish = () => {
      pager.classList.remove('view-sliding');
      if (commit) {
        current.remove();
        incoming.classList.remove('view-entering');
        incoming.style.transform = '';
        setActiveTab(destView);
        // Preserve the current query+hash so the URL matches the view we
        // already filtered against it during the drag (e.g. ?date= persists).
        history.pushState({ view: destView }, '',
          VIEW_PATHS[destView] + location.search + location.hash);
        prefetchView(destView === 'kina' ? 'films' : 'kina');
      } else {
        // Snap-back: drop the incoming and restore the outgoing as the live
        // view. Restore its globals (not re-run its script — that would reset
        // runtime state like a Kina pin), re-id it, then rebuild its index.
        incoming.remove();
        restoreViewIds(saved);
        current.style.transform = '';
        window.buildIndex   = globals.buildIndex;
        window.applyFilters = globals.applyFilters;
        window.__viewInit   = globals.viewInit;
        window._kinaPinned  = globals.kinaPinned;
        bootView();
        setActiveTab(current.dataset.view);
      }
      _swapping = false;
    };

    if (matchMedia('(prefers-reduced-motion: reduce)').matches) {
      if (commit) {
        current.style.transform  = 'translateX(' + (dir === 'left' ? -w : w) + 'px)';
        incoming.style.transform = 'translateX(0)';
      }
      finish();
      return;
    }
    pager.classList.add('view-sliding');   // transition transform on both panels
    requestAnimationFrame(() => {
      current.style.transform  = commit ? 'translateX(' + (dir === 'left' ? -w : w) + 'px)' : 'translateX(0)';
      incoming.style.transform = commit ? 'translateX(0)' : 'translateX(' + enter + 'px)';
    });
    let done = false;
    const onEnd = (e) => {
      if (e && e.target !== incoming && e.target !== current) return;
      if (done) return;
      done = true;
      incoming.removeEventListener('transitionend', onEnd);
      finish();
    };
    incoming.addEventListener('transitionend', onEnd);
    setTimeout(onEnd, 450);   // fallback if transitionend is missed
  }

  document.addEventListener('pointerdown', (e) => {
    if (e.pointerType === 'mouse' || _swapping) return;
    if (!matchMedia('(pointer: coarse)').matches) return;
    const root = document.getElementById('view-root');
    if (!root) return;
    if (e.target instanceof Element && e.target.closest('.cinema-nav-row, #cinema-pills')) return;
    _drag = { x0: e.clientX, y0: e.clientY, axis: null, ctx: null, fallbackDest: null,
              lastDx: 0, vx: 0, lastT: e.timeStamp };
    // Warm the sibling the instant a finger lands so even a cold-start swipe can
    // engage live tracking (beginDrag is retried on every move). Set `_drag`
    // first so this stays a pure side-effect — prefetchView never mounts or
    // touches the swap, it only fills the cache (and self-guards on the TTL).
    prefetchView(root.dataset.view === 'films' ? 'kina' : 'films');
  }, { passive: true });

  // Claim a single-finger HORIZONTAL drag so the browser can't decide mid-drag
  // that it's a vertical scroll, steal the gesture (pointercancel) and snap the
  // page back to the origin without the finger lifting — which is what a slow
  // drag triggered, worst near the start while the browser is still
  // scroll-detecting. NON-passive so `preventDefault` is allowed; it fires only
  // for a horizontal-leaning single touch, so vertical scroll and two-finger
  // pinch-zoom still pass through untouched. Keyed off the same `x0/y0` the
  // pointer handler set, and uses the same `|dx|>|dy|` lean test so the very
  // first move is already claimed (before `pointermove` has locked the axis).
  document.addEventListener('touchmove', (e) => {
    if (!_drag || !e.cancelable || e.touches.length !== 1) return;
    if (_drag.axis === 'x') { e.preventDefault(); return; }   // already a swipe — keep it
    const tdx = Math.abs(e.touches[0].clientX - _drag.x0);
    const tdy = Math.abs(e.touches[0].clientY - _drag.y0);
    // Never preventDefault a clear vertical scroll (or a two-finger pinch,
    // guarded above) — but claim a horizontal-leaning drag on the VERY FIRST
    // leaning move, with no deadzone wait. The browser's scroll-vs-gesture
    // arbitration happens in the first few px of a slow drag, so deferring the
    // claim until a 10px deadzone (as an earlier version did) leaves a window
    // where it scroll-steals the gesture — which reads as "the swipe didn't
    // respond at all." pointermove still needs the deadzone to LOCK the axis and
    // start tracking; this only needs to hold the browser off, so it's eager.
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
      if (adx >= SWIPE_DEADZONE_PX && adx >= ady) { _drag.axis = 'x'; }
      // Otherwise still ambiguous (tiny, or leaning vertical but not decisively) → wait for the next move.
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
    // Lazily mount the neighbour the first time the drag points at a real one.
    // Films is the leftmost view and Kina the rightmost, so a drag in the dead
    // direction has no neighbour — DON'T kill the gesture for it (that stopped
    // the finger tracking back and forth, iOS-style); just wait until the
    // finger heads the valid way. Retried each move so a cache that warms
    // mid-drag still starts tracking.
    if (!_drag.ctx) {
      const cur  = document.getElementById('view-root').dataset.view;
      const dest = dx < 0 ? 'kina' : 'films';   // swipe-left → right tab; swipe-right → left tab
      if (dx !== 0 && dest !== cur) {
        const w   = document.getElementById('view-pager').offsetWidth || window.innerWidth;
        const ctx = beginDrag(dest, w);
        if (ctx) { _drag.ctx = ctx; _swapping = true; _drag.fallbackDest = null; }
        else     { _drag.fallbackDest = dest; }   // cold cache → threshold switch on release
      }
    }
    if (_drag.ctx) {
      // The view follows the finger 1:1, clamped to [neighbour, origin] (you
      // can drag the neighbour in and back out, but not past either edge).
      const { current, incoming, dir, enter, w, destView, originView } = _drag.ctx;
      const d = dir === 'left' ? Math.max(-w, Math.min(0, dx)) : Math.min(w, Math.max(0, dx));
      current.style.transform  = 'translateX(' + d + 'px)';
      incoming.style.transform = 'translateX(' + (enter + d) + 'px)';
      // Live preview: light up the tab a release would land on right now — past
      // ~40% toward the neighbour commits, otherwise it snaps back. Mirrors the
      // position half of the release decision (a flick can still commit a
      // shorter drag, but the highlight tracks position). Settle sets the final
      // tab, so this is purely the in-flight hint.
      const preview = Math.abs(d) > w * COMMIT_FRACTION ? destView : originView;
      if (_drag.preview !== preview) { _drag.preview = preview; setActiveTab(preview); }
    }
  }, { passive: true });

  // Decision happens ONLY here, when the finger lifts (or the gesture is
  // cancelled): animate to the neighbour if the finger ended past ~40% of the
  // width toward it OR left with a quick flick that way; otherwise settle back
  // to the original page. Until then the view just tracks the finger. Reads the
  // tracked `lastDx`/`vx`, never the unreliable pointerup coordinate.
  function endDrag() {
    const drag = _drag;
    _drag = null;
    if (!drag || drag.axis !== 'x') return;
    const dx = drag.lastDx;
    if (drag.ctx) {
      const { dir, w } = drag.ctx;
      const toward = dir === 'left' ? dx < 0 : dx > 0;   // ended toward the mounted neighbour?
      const flick  = Math.abs(drag.vx) > FLICK_VX && Math.abs(dx) > FLICK_MIN_PX;
      settleDrag(drag.ctx, toward && (Math.abs(dx) > w * COMMIT_FRACTION || flick));
    } else if (drag.fallbackDest) {
      const toward = drag.fallbackDest === 'kina' ? dx < 0 : dx > 0;
      const flick  = Math.abs(drag.vx) > FLICK_VX && Math.abs(dx) > FLICK_MIN_PX;
      if (toward && (Math.abs(dx) >= window.innerWidth * COMMIT_FRACTION || flick)) {
        navigateTo(VIEW_PATHS[drag.fallbackDest], drag.fallbackDest,
                   drag.fallbackDest === 'kina' ? 'left' : 'right', true, false);
      }
    }
  }
  document.addEventListener('pointerup', endDrag, { passive: true });
  document.addEventListener('pointercancel', endDrag, { passive: true });

  document.addEventListener('DOMContentLoaded', () => {
    // Cache the CURRENT view as-rendered, before bootView mutates it (sort /
    // display:none). The embedded seed below warms the sibling; this warms the
    // view we're on — so swiping (or clicking the tab) BACK to it is instant
    // too, not a cold fetch. Only for the bare routes the swap targets
    // (`/`, `/kina`); a pinned `/kina/:cinema` is left to fetch.
    const liveRoot = document.getElementById('view-root');
    const liveView = liveRoot?.dataset.view;
    if (liveView && location.pathname === VIEW_PATHS[liveView]) {
      _prefetch.set(VIEW_PATHS[liveView], { html: liveRoot.outerHTML, ts: Date.now() });
    }
    // One-time shell init — navbar chrome that survives view swaps and must
    // NOT re-run (date options, square nav buttons, hidden-films badge).
    populateDayOptions();
    squareDateNavBtns();
    updateNavbar();
    // Grid-dependent init — re-runs on every view swap.
    bootView();
    setActiveTab(document.getElementById('view-root')?.dataset.view || 'films');
    bootMergeFromServer();
    // Seed the prefetch cache from the sibling fragment the server embedded in
    // the page (a `type="application/json"` block). This makes the FIRST swipe
    // track live with zero network — no fetch latency, no release-only fallback,
    // even on a slow connection or a cold dev server. The warm below still runs
    // (refreshes the cache / covers the no-embed case).
    try {
      const seedEl = document.getElementById('kinowo-sibling-view');
      if (seedEl) {
        const seed = JSON.parse(seedEl.textContent);
        if (seed && seed.path && seed.html) _prefetch.set(seed.path, { html: seed.html, ts: Date.now() });
      }
    } catch (e) { /* malformed seed — fall back to the network warm below */ }
    // Warm the sibling so the first switch is instant. Fire it one frame after
    // load — off the critical first paint, but PROMPTLY: the earlier idle
    // callback could be starved for up to its timeout on a busy page (every
    // poster loading), and a cold cache is exactly when a swipe can't track the
    // finger and falls back to the less-responsive release-only threshold. The
    // fetch is async and small (a #view-root fragment), so it won't block paint.
    const here = document.getElementById('view-root')?.dataset.view;
    if (here) {
      const warm = () => prefetchView(here === 'films' ? 'kina' : 'films');
      requestAnimationFrame(() => setTimeout(warm, 0));
    }
  });

  // ── Image-fetch uptime tracker ────────────────────────────────────────────
  //
  // Captures browser-side img load/error outcomes — including the
  // `images.weserv.nl` proxy that fronts every cinema poster — and
  // batches them to /uptime/img-event so the uptime page can show
  // per-host reliability bars. Event listeners are registered in the
  // capture phase because `load` / `error` don't bubble on <img>.
  (function() {
    var pending = [];
    var FLUSH_INTERVAL_MS = 10000;
    var BATCH_SIZE_TRIGGER = 50;

    function hostOf(src) {
      try { return new URL(src, window.location.href).host || 'unknown'; }
      catch (e) { return 'unknown'; }
    }

    function record(target, success) {
      if (!target || target.tagName !== 'IMG') return;
      var src = target.currentSrc || target.src;
      if (!src) return;
      var ev = { host: hostOf(src), success: success };
      if (!success) ev.error = 'img load failed (' + src.substring(0, 120) + ')';
      pending.push(ev);
      if (pending.length >= BATCH_SIZE_TRIGGER) flush();
    }

    function flush() {
      if (pending.length === 0) return;
      var body = JSON.stringify({ events: pending });
      pending = [];
      try {
        if (navigator.sendBeacon) {
          navigator.sendBeacon('/uptime/img-event', new Blob([body], { type: 'application/json' }));
        } else {
          fetch('/uptime/img-event', { method: 'POST', body: body, headers: { 'Content-Type': 'application/json' }, keepalive: true });
        }
      } catch (e) { /* tracker must never throw into page code */ }
    }

    document.addEventListener('load',  function(ev) { record(ev.target, true);  }, true);
    document.addEventListener('error', function(ev) { record(ev.target, false); }, true);
    setInterval(flush, FLUSH_INTERVAL_MS);
    window.addEventListener('pagehide', flush);
  })();
