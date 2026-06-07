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
    // Cinema filter lives in the Filtry panel; surface the selected-vs-total
    // count in the button label so the navbar signals "you've narrowed the
    // cinemas" at a glance. Gated on `#cinema-list` so pages without the picker
    // (e.g. /ulubione) don't show a count.
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

  // Root for grid-wide DOM scans: the listing's `#view-root` when present,
  // else the whole document (pages with no `#view-root`, e.g. /plan).
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
    // Pages without the picker (e.g. /ulubione) have no `#cinema-list` — bail
    // instead of throwing on the null `list`.
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
    // invert. Pages without the picker have no `#cinema-list` → no-op.
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

  // Re-inits everything tied to the grid DOM: the DOM index, the Filtry cinema
  // picker, the lazy submenu panels, then a single filtered render. Reads the
  // view-provided `window.buildIndex`/`applyFilters` (assigned by the view's
  // inline IIFE), so it must run AFTER that inline script.
  function bootView() {
    buildIndex();
    // Cinema picker lives in the Filtry dropdown — populate the list so the
    // first open of Filtry has the checkboxes ready. Cheap (one row per cinema,
    // no grid scan), so it stays eager.
    buildCinemaPanel();
    // The grid-scanning submenu panels are built lazily (on Filtry-open) — drop
    // any stale build-flags so they re-tally this grid.
    resetSubmenuPanels();
    // URL → controls AFTER the picker is built so checkbox updates land on real
    // DOM nodes; then one `applyFilters()` pass renders the grid already
    // filtered.
    applyFiltersFromURL();
    updateFormatBtn();
    applyFilters();
  }
  window.bootView = bootView;

  // Every page is under `/{city}/…`. CURRENT_CITY is the global from
  // `_sharedJsConfig`.
  const CITY_BASE = '/' + CURRENT_CITY;
  // ── Swipe to switch days (phones) ───────────────────────────────────────────
  //
  // A horizontal swipe steps the selected day: swipe LEFT for the next day,
  // RIGHT for the previous one, wrapping around the day dropdown's option list.
  // During the drag the grid (`#view-root`) follows the finger; on release past
  // ~40% of the width (or a quick flick) it slides the rest of the way out, the
  // day changes, and the new day's grid slides in from the opposite edge. A
  // shorter drag snaps back. Listeners are passive except the `touchmove` claim
  // that holds the browser off its scroll-vs-gesture arbitration.

  // Swipe-gesture tuning.
  const COMMIT_FRACTION   = 0.4;   // drag past this fraction of the width → commit
  const FLICK_VX          = 0.4;   // px/ms — a quick flick commits a shorter drag
  const FLICK_MIN_PX      = 24;    // ignore micro-flicks
  const SWIPE_DEADZONE_PX = 10;    // horizontal travel before we lock to a swipe
  const SWIPE_ANIM_MS     = 220;   // slide-out / slide-in duration
  // Axis lock is biased toward HORIZONTAL so a swipe that starts with a little
  // vertical jitter isn't misread as a scroll and killed. We concede to vertical
  // scrolling only when it CLEARLY dominates.
  const SWIPE_VBAIL_PX    = 16;    // vertical must travel at least this far to even consider bailing
  const SWIPE_VBIAS_RATIO = 1.6;   // …and beat horizontal by this factor → it's a real vertical scroll

  let _drag = null;
  let _animating = false;   // guards re-entrancy while a commit animation runs

  // Step the day dropdown by `dir` (+1 = next day, -1 = previous), WRAPPING
  // around its full option list, then re-render via the normal date-change path
  // (`onDateChange` → `applyFilters` → `syncDateToURL`). Exposed for the swipe
  // gesture and unit tests.
  function stepDateWrap(dir) {
    const sel = document.getElementById('date-filter');
    if (!sel || sel.options.length === 0) return;
    const n = sel.options.length;
    sel.selectedIndex = ((sel.selectedIndex + dir) % n + n) % n;
    onDateChange();
  }
  window.stepDateWrap = stepDateWrap;

  // ── First-run swipe hint ────────────────────────────────────────────────────
  // Reveal "Przesuń, aby zmienić dzień" once per calendar day on a touch device,
  // until the first real swipe retires it for good — the same rule the iOS and
  // Android apps follow. Device-local (plain localStorage, NOT the server-synced
  // store): it's a per-device onboarding nudge, not user state worth syncing.
  const SWIPE_HINT_DAY  = 'kinowoSwipeHintDay';   // last calendar day (Warsaw) it was shown
  const SWIPE_HINT_DONE = 'kinowoSwipeHintDone';  // set on the first swipe → never show again
  const SWIPE_HINT_MS   = 3000;                   // auto-hide after this long if untouched
  let _swipeHintTimer = null;

  function _hintGet(k) { try { return localStorage.getItem(k); } catch (e) { return null; } }
  function _hintSet(k, v) { try { localStorage.setItem(k, v); } catch (e) { /* private mode — skip */ } }

  // Reveal the hint if this is a touch device, the user hasn't swiped before,
  // and it hasn't already shown today. Called once at boot.
  function maybeShowSwipeHint() {
    if (!matchMedia('(pointer: coarse)').matches) return;   // desktop never sees it
    const el = document.getElementById('swipe-hint');
    if (!el) return;                                        // not the listing page
    if (_hintGet(SWIPE_HINT_DONE)) return;                 // retired by a past swipe
    const today = new Date().toLocaleDateString('sv', { timeZone: 'Europe/Warsaw' });
    if (_hintGet(SWIPE_HINT_DAY) === today) return;        // already shown today
    _hintSet(SWIPE_HINT_DAY, today);
    el.classList.add('visible');
    clearTimeout(_swipeHintTimer);
    _swipeHintTimer = setTimeout(() => el.classList.remove('visible'), SWIPE_HINT_MS);
  }

  // Hide the hint without retiring it (the user has started a horizontal drag).
  function dismissSwipeHint() {
    clearTimeout(_swipeHintTimer);
    document.getElementById('swipe-hint')?.classList.remove('visible');
  }

  // Retire the hint for good (the user committed a day-swipe — they've got it).
  function retireSwipeHint() {
    dismissSwipeHint();
    _hintSet(SWIPE_HINT_DONE, '1');
  }

  // Slide the grid out toward the swipe direction ('left' = swiped left → next
  // day), switch the day while it's off-screen, then slide the refreshed grid in
  // from the opposite edge. `fromPx` is the live drag offset so the commit
  // animation continues smoothly from where the finger left it.
  function commitDaySwipe(dir, fromPx) {
    const root  = document.getElementById('view-root');
    if (!root) return;
    retireSwipeHint();   // a committed swipe means they've found the gesture
    const pager = document.getElementById('view-pager');
    const w     = (pager && pager.offsetWidth) || window.innerWidth;
    const dayDir = dir === 'left' ? 1 : -1;   // swipe-left → next day
    if (matchMedia('(prefers-reduced-motion: reduce)').matches) {
      root.style.transition = '';
      root.style.transform  = '';
      stepDateWrap(dayDir);
      return;
    }
    _animating = true;
    const out    = dir === 'left' ? -w : w;   // exits the way the finger was going
    const inFrom = -out;                       // new grid enters from the opposite edge
    root.style.transition = 'transform ' + SWIPE_ANIM_MS + 'ms ease';
    root.style.transform  = 'translateX(' + (fromPx || 0) + 'px)';
    requestAnimationFrame(() => { root.style.transform = 'translateX(' + out + 'px)'; });
    let slid = false;
    const slideIn = () => {
      if (slid) return;
      slid = true;
      root.removeEventListener('transitionend', slideIn);
      // Off-screen now: change the day, jump to the entry edge with no
      // transition, then animate back to rest.
      root.style.transition = 'none';
      root.style.transform  = 'translateX(' + inFrom + 'px)';
      stepDateWrap(dayDir);
      requestAnimationFrame(() => requestAnimationFrame(() => {
        root.style.transition = 'transform ' + SWIPE_ANIM_MS + 'ms ease';
        root.style.transform  = 'translateX(0)';
        let cleared = false;
        const clear = () => {
          if (cleared) return;
          cleared = true;
          root.style.transition = '';
          root.style.transform  = '';
          _animating = false;
          root.removeEventListener('transitionend', clear);
        };
        root.addEventListener('transitionend', clear);
        setTimeout(clear, SWIPE_ANIM_MS + 60);
      }));
    };
    root.addEventListener('transitionend', slideIn);
    setTimeout(slideIn, SWIPE_ANIM_MS + 60);   // fallback if transitionend is missed
  }

  // A drag that didn't commit: ease the grid back to rest.
  function snapBack() {
    const root = document.getElementById('view-root');
    if (!root) return;
    root.style.transition = 'transform ' + SWIPE_ANIM_MS + 'ms ease';
    root.style.transform  = 'translateX(0)';
    let cleared = false;
    const clear = () => {
      if (cleared) return;
      cleared = true;
      root.style.transition = '';
      root.style.transform  = '';
      root.removeEventListener('transitionend', clear);
    };
    root.addEventListener('transitionend', clear);
    setTimeout(clear, SWIPE_ANIM_MS + 60);
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
        const r = document.getElementById('view-root');
        if (r) r.style.transition = 'none';   // track the finger without lag
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
    // The grid follows the finger 1:1 — the day list wraps, so every direction
    // has a destination and there are no dead edges to clamp.
    const root = document.getElementById('view-root');
    if (root) root.style.transform = 'translateX(' + dx + 'px)';
  }, { passive: true });

  // Decision happens ONLY when the finger lifts (or the gesture cancels): step
  // the day if the drag ended past ~40% of the width OR left with a quick flick;
  // otherwise snap back. Reads the tracked `lastDx`/`vx`.
  function endDrag() {
    const drag = _drag;
    _drag = null;
    if (!drag || drag.axis !== 'x') return;
    const dx    = drag.lastDx;
    const pager = document.getElementById('view-pager');
    const w     = (pager && pager.offsetWidth) || window.innerWidth;
    const flick = Math.abs(drag.vx) > FLICK_VX && Math.abs(dx) > FLICK_MIN_PX;
    if (Math.abs(dx) > w * COMMIT_FRACTION || flick) commitDaySwipe(dx < 0 ? 'left' : 'right', dx);
    else snapBack();
  }
  document.addEventListener('pointerup', endDrag, { passive: true });
  document.addEventListener('pointercancel', endDrag, { passive: true });

  document.addEventListener('DOMContentLoaded', () => {
    // One-time shell init — navbar chrome (date options, square nav buttons,
    // hidden-films badge) — then the grid-dependent boot.
    populateDayOptions();
    squareDateNavBtns();
    updateNavbar();
    bootView();
    bootMergeFromServer();
    maybeShowSwipeHint();   // once-a-day phone nudge, retired on first swipe
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
