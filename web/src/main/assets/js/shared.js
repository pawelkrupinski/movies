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
    // rendered on all pages — treat a missing checkbox as "unchecked / no
    // IMAX filter applied" rather than throwing.
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

  // Country switcher (Filtry → Kraj) changed — each country is its own web
  // deployment on a distinct host, so switch by NAVIGATING to that host's root
  // (its city-chooser lands the visitor, since cities differ per country). Full
  // navigation, not a view-swap. `url` is the chosen deployment's scheme+host.
  function onCountryChange(url) {
    if (!url) return;
    window.location.href = url + '/';
  }
  window.onCountryChange = onCountryChange;

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
  window.lockSearchZoom   = lockSearchZoom;
  window.unlockSearchZoom = unlockSearchZoom;

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
  // Exposed so the page-behaviour spec can drive it directly with a stubbed
  // visual viewport (headless Chrome has no on-screen keyboard to shrink one).
  window.applySearchKeyboardInset = applySearchKeyboardInset;

  // On mobile portrait the search field lives in a floating pill pinned to the
  // bottom of the viewport, so when its keyboard is up the rest of the screen —
  // film cards, poster/title links — sits right behind it. A tap "away" to
  // dismiss the keyboard would otherwise also land on whatever is under the
  // finger and navigate to /film. Make that first tap a pure dismiss: blur the
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

  // True when any filter the Filtry panel exposes is narrowed away from its
  // default — i.e. exactly the axes "Wyczyść" (`resetFormatFilter`) puts back.
  // Drives the funnel icon's active state, the web counterpart of the iOS
  // `filtersActive` flag (FiltersBar) and Android `vm.filtersActive`. Sort
  // order is deliberately excluded: it reorders, it doesn't filter anything
  // out, so the funnel staying neutral when you only re-sort is the honest cue.
  function filtersActive() {
    if (getFormatFilter().length > 0) return true;     // Wymiar / Wersja / IMAX
    if (getFromMinutes() !== null)    return true;      // Od godziny
    if (getSubmenuFilter('country')  !== null) return true;
    if (getSubmenuFilter('genre')    !== null) return true;
    if (getSubmenuFilter('director') !== null) return true;
    if (getSubmenuFilter('cast')     !== null) return true;
    if (getSubmenuFilter('room')     !== null) return true;
    // Cinema picker (absent on pages without the Filtry UI): any cinema in THIS city switched off.
    if (document.getElementById('cinema-list') && disabledCinemasInCity().length > 0) return true;
    return false;
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
    ['country', 'genre', 'director', 'cast', 'room'].forEach(function(key) {
      var list = document.getElementById(key + '-list');
      if (list) {
        list.querySelectorAll('input[type="checkbox"]').forEach(function(checkbox) { checkbox.checked = true; });
        list.style.display = 'none';
      }
      var chevron = document.getElementById(key + '-chevron');
      if (chevron) chevron.classList.remove('open');
      updateSubmenuCount(key);
    });
    // Re-enable every cinema in this city — the picker lives in the same panel
    // and counts as a filter (the funnel icon lights for it), so "Wyczyść"
    // clears it too, matching the iOS/Android Wyczyść which resets disabledCinemas.
    if (document.getElementById('cinema-list')) toggleAllCinemas(true);
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
    var checked = boxes.filter(function(checkbox) { return checkbox.checked; });
    if (checked.length === boxes.length) return null;
    return checked.map(function(checkbox) { return checkbox.value; });
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
      list.querySelectorAll('input[type="checkbox"]:not(.submenu-all)').forEach(function(checkbox) { checkbox.checked = allCb.checked; });
      updateSubmenuCount(key); updateFormatBtn(); applyFilters();
    };
    allLabel.appendChild(allCb);
    allLabel.appendChild(document.createTextNode(' Wszystkie'));
    list.appendChild(allLabel);

    entries.forEach(function(entry) {
      var label = document.createElement('label');
      label.className = 'panel-label';
      var checkbox = document.createElement('input');
      checkbox.type = 'checkbox';
      checkbox.value = entry.value;
      checkbox.checked = true;
      checkbox.onchange = function() { updateSubmenuCount(key); updateFormatBtn(); applyFilters(); };
      label.appendChild(checkbox);
      label.appendChild(document.createTextNode(' ' + entry.label));
      var count = document.createElement('span');
      count.className = 'submenu-film-count';
      count.textContent = '(' + entry.count + ')';
      label.appendChild(count);
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
    gridScope().querySelectorAll('.cinema-group[data-cinema]').forEach(function(cinemaGroup) {
      var cinema = cinemaGroup.dataset.cinema;
      cinemaGroup.querySelectorAll('.badge-time[data-room]').forEach(function(b) {
        var room = b.dataset.room;
        if (!room) return;
        if (!byCinema[cinema]) byCinema[cinema] = {};
        byCinema[cinema][room] = (byCinema[cinema][room] || 0) + 1;
      });
    });

    var cinemas = Object.keys(byCinema).sort(function(a, b) { return a.localeCompare(b, 'pl'); });

    // Hide the Sale row entirely when no badge on the page carries `data-room`
    // (e.g. a fixture day where the scrapers returned no rooms).
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
      list.querySelectorAll('input[type="checkbox"]:not(.submenu-all)').forEach(function(checkbox) {
        checkbox.checked = allCb.checked;
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

      header.onclick = function() {
        var opening = inner.style.display === 'none';
        inner.style.display = opening ? '' : 'none';
        chevron.classList.toggle('open', opening);
      };

      rooms.forEach(function(room) {
        var label = document.createElement('label');
        label.className = 'panel-label';
        var checkbox = document.createElement('input');
        checkbox.type = 'checkbox';
        checkbox.value = cinema + '|' + room;
        checkbox.checked = true;
        checkbox.onchange = function() {
          _updateRoomCinemaCount(header);
          updateSubmenuCount('room'); updateFormatBtn(); applyFilters();
        };
        label.appendChild(checkbox);
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
  // a future "carry /plan's room picks into /" cross-page
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
  // Called from applyFilters() in the repertoire view after visibility has
  // been set on badges / groups. Walks the already-computed visibility —
  // no extra DOM measurement. The /film page has no applyFilters and
  // doesn't call this, so it renders everything.

  const _MAX_SHOWINGS_ROWS = 10;
  const _PILLS_PER_ROW     = 6;
  const _MIN_HIDDEN         = 3;

  // Locale-aware plural category. Polish has three showtime forms
  // (seans / seanse / seansów); English two (showing / showings). The rule +
  // the forms both come from KINOWO_LOCALE (server-injected per deployment).
  function _pluralCategory(n) {
    if (KINOWO_LOCALE.plural === 'pl') {
      if (n === 1) return 'one';
      const mod10 = n % 10, mod100 = n % 100;
      if (mod10 >= 2 && mod10 <= 4 && (mod100 < 12 || mod100 > 14)) return 'few';
      return 'many';
    }
    return n === 1 ? 'one' : 'other';
  }

  function _showtimeNoun(n) {
    const forms = KINOWO_LOCALE.showtime;
    return forms[_pluralCategory(n)] || forms.other || forms.many;
  }

  function undoTruncation() {
    document.querySelectorAll('.date-group, .cinema-group').forEach(element => {
      if (element._truncated) { element.style.display = ''; element._truncated = false; }
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

    for (const dateGroup of dateGroups) {
      if (dateGroup.style.display === 'none') continue;

      const cinemaGroups = dateGroup.querySelectorAll('.cinema-group');
      let dayHasVisible = false;
      const dayLabelRow = 1;
      let dayLines = dayLabelRow;

      for (const cinemaGroup of cinemaGroups) {
        if (cinemaGroup.style.display === 'none') continue;

        const visibleBadges = [...cinemaGroup.querySelectorAll('.badge-time')].filter(
          b => b.style.display !== 'none'
        ).length;
        if (visibleBadges === 0) continue;

        if (capped) {
          hidden += visibleBadges;
          cinemaGroup.style.display = 'none';
          cinemaGroup._truncated = true;
          continue;
        }

        const pillRows = Math.max(1, Math.ceil(visibleBadges / _PILLS_PER_ROW));
        const cinemaLines = (hasCinemaHeaders ? 1 : 0) + pillRows;

        if (lineCount + dayLines + cinemaLines <= _MAX_SHOWINGS_ROWS) {
          if (cinemaGroup._truncated) { cinemaGroup.style.display = ''; cinemaGroup._truncated = false; }
          dayHasVisible = true;
          dayLines += cinemaLines;
        } else {
          hidden += visibleBadges;
          cinemaGroup.style.display = 'none';
          cinemaGroup._truncated = true;
          capped = true;
        }
      }

      if (dayHasVisible) {
        lineCount += dayLines;
      } else if (capped) {
        if (dateGroup.style.display !== 'none') {
          dateGroup.style.display = 'none';
          dateGroup._truncated = true;
        }
      }
    }

    if (hidden > _MIN_HIDDEN) {
      link.textContent = '… +' + hidden + ' ' + _showtimeNoun(hidden);
      link.style.display = '';
    } else {
      if (hidden > 0) {
        for (const dateGroup of dateGroups) {
          if (dateGroup._truncated) { dateGroup.style.display = ''; dateGroup._truncated = false; }
          for (const cinemaGroup of dateGroup.querySelectorAll('.cinema-group')) {
            if (cinemaGroup._truncated) { cinemaGroup.style.display = ''; cinemaGroup._truncated = false; }
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
    const modal = document.getElementById('hidden-modal-backdrop');
    if (modal) modal.classList.add('open');
  }
  function closeHiddenModal() {
    const modal = document.getElementById('hidden-modal-backdrop');
    if (modal) modal.classList.remove('open');
    // Reset the search on close so the next open shows the full list.
    const searchInput = document.getElementById('hidden-modal-search');
    if (searchInput) { searchInput.value = ''; filterHiddenModal(); }
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
    const modal = document.getElementById('login-modal-backdrop');
    if (modal) modal.classList.add('open');
  }
  function closeLoginModal() {
    const modal = document.getElementById('login-modal-backdrop');
    if (modal) modal.classList.remove('open');
  }
  // shared.js runs inside an IIFE, so these are NOT globals by default; the
  // navbar / login-modal partials call them from inline `onclick=` handlers,
  // which resolve against `window`. Without these assignments the click throws
  // `ReferenceError: toggleAuthMenu is not defined` on every page that gets its
  // auth menu from shared.js (i.e. all but the self-contained `browse` view).
  window.toggleAuthMenu = toggleAuthMenu;
  window.closeAuthMenu  = closeAuthMenu;
  window.openLoginModal = openLoginModal;
  window.closeLoginModal = closeLoginModal;
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

  function hideFilm(button) {
    // No scrollY snapshot needed: hiding only ever removes a card, never
    // reorders the visible set, so the browser's scroll anchor stays put.
    const title = button.closest('[data-title]').dataset.title;
    const hidden = getHidden();
    if (!hidden.includes(title)) {
      hidden.push(title);
      setHidden(hidden);
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
    // Pages without the picker have no `#cinema-list` — bail instead of
    // throwing on the null `list`.
    if (!list) return;
    const disabled = getDisabledCinemas();
    list.innerHTML = '';
    ALL_CINEMAS.forEach(cinema => {
      const label = document.createElement('label');
      label.className = 'panel-label';
      const checkbox = document.createElement('input');
      checkbox.type = 'checkbox';
      checkbox.checked = !disabled.includes(cinema);
      checkbox.onchange = () => {
        const disabled = getDisabledCinemas();
        if (checkbox.checked) {
          setDisabledCinemas(disabled.filter(c => c !== cinema));
        } else {
          if (!disabled.includes(cinema)) disabled.push(cinema);
          setDisabledCinemas(disabled);
        }
        syncAllCheckbox();
        updateFormatBtn();   // cinema count is part of the Filtry label now
        applyFilters();
      };
      label.appendChild(checkbox);
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

  // ── IMDb "open in app" on Android ─────────────────────────────────────────
  // Tapping an IMDb rating badge on Android Chrome opens the IMDb app at the
  // title (imdb://) when installed, and falls back to the web page when not.
  // Android's `intent://` URL does both natively: Chrome routes the registered
  // scheme to the app and otherwise follows `browser_fallback_url` — no error
  // dialog, no JS timeout. We don't do this for the other rating sources: RT
  // and Filmweb already open their apps from the plain https link via the OS's
  // App Links, and Metacritic has no app. iOS Safari is left as the plain
  // https `target="_blank"` anchor too — there a custom scheme would raise an
  // unavoidable "Cannot Open Page" alert when the app is absent, and the https
  // link still opens the IMDb app via Universal Links when it's installed.
  // Firefox Android lacks `intent://` support, so we gate on Chrome-family UAs.
  function imdbIntentUrl(href) {
    const m = /tt\d+/.exec(href || '');
    if (!m) return null;
    return 'intent://title/' + m[0] +
      '#Intent;scheme=imdb;package=com.imdb.mobile;S.browser_fallback_url=' +
      encodeURIComponent(href) + ';end';
  }
  function isAndroidChrome(ua) {
    return /Android/.test(ua) && /Chrome\//.test(ua) && !/Firefox/.test(ua);
  }
  window.imdbIntentUrl  = imdbIntentUrl;
  window.isAndroidChrome = isAndroidChrome;

  if (isAndroidChrome(navigator.userAgent)) {
    document.addEventListener('click', e => {
      if (!(e.target instanceof Element)) return;
      const link = e.target.closest('a.rating-imdb');
      if (!link) return;
      const url = imdbIntentUrl(link.getAttribute('href'));
      if (!url) return;
      e.preventDefault();
      window.location.href = url;
    });
  }

  // ── Empty state ───────────────────────────────────────────────────────────

  function updateEmptyState(visibleCount) {
    const noFilms = document.getElementById('no-films');
    if (visibleCount === 0) {
      noFilms.textContent = KINOWO_LOCALE.emptyRepertoire;
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

  // Weekday (Sun-first, matching Date.getDay()) + month labels, injected per
  // deployment via KINOWO_LOCALE (Polish keeps the genitive month forms).
  const DAY2   = KINOWO_LOCALE.day2;
  const MONTHS = KINOWO_LOCALE.months;

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

  // Reflect the hidden `#date-filter` value onto the visible day pills: set
  // `.active` + `aria-selected` on the matching pill, clear the rest. Called on
  // boot and after every committed day change (via `onDateChange`), plus
  // eagerly on a pill tap so the highlight moves before the slide commits.
  // Set `.active` + `aria-selected` on the pill matching `value`, clear the rest.
  function highlightDayPill(value) {
    document.querySelectorAll('#day-pills .day-pill').forEach(p => {
      const on = p.dataset.day === value;
      p.classList.toggle('active', on);
      p.setAttribute('aria-selected', on ? 'true' : 'false');
    });
  }

  function syncDayPills() {
    const sel = document.getElementById('date-filter');
    if (!sel) return;
    highlightDayPill(sel.value);
  }
  window.syncDayPills = syncDayPills;

  // A day pill was tapped — move the hidden select to that day and slide there
  // exactly like the old dropdown pick (linear direction from the list order).
  function pickDay(value) {
    const sel = document.getElementById('date-filter');
    if (!sel || sel.value === value) return;
    sel.value = value;
    syncDayPills();   // highlight immediately; the slide commits the grid
    onDateSelect();
  }
  window.pickDay = pickDay;

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
  window.onDateSelect = onDateSelect;

  // "Apply the current day NOW" — no animation. Rebuilds the visible grid the
  // same way every other filter does (`applyFilters` also calls `syncDateToURL`,
  // date-only) and records the displayed day. The carousel's `commitDay`, the
  // wrap helper, and reduced-motion paths funnel through here after they've set
  // the dropdown to the target.
  function onDateChange() {
    const sel = document.getElementById('date-filter');
    if (sel) _appliedDay = sel.value;
    syncDayPills();
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
    history.replaceState(null, '', path);
    const url = window.location.origin + path;
    const done = () => {
      if (!button) return;
      const previous = button.textContent;
      button.textContent = 'Skopiowano!';
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

  function applyFiltersFromURL() {
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
          const [year, month, day] = val.split('-').map(Number);
          const dayOfWeek = new Date(year, month - 1, day).getDay();
          option.textContent = DAY2[dayOfWeek] + ' ' + day + ' ' + MONTHS[month - 1];
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

    updateFormatBtn();
  }


  // ── Server sync for logged-in users ──────────────────────────────────────
  //
  // When the user is logged in, localStorage is still the in-page truth
  // (every read goes there for zero-latency) but every write also
  // debounces a PUT to /api/me/state so the server stays in sync.
  //
  // The boot reconcile is two-phase, gated by the `serverStateSynced` flag:
  //
  //   • FIRST reconcile after a login (flag unset): union local + server and
  //     push the union, so anything the user set while anonymous is migrated
  //     up to the account ("migrate on page entry"). Then set the flag.
  //   • EVERY reconcile after that (flag set): the SERVER is the source of
  //     truth — replace localStorage with the server's sets. This is what
  //     makes a removal (un-hide a film / re-enable a cinema) STICK: a blind
  //     union on every page load could only ever add, so it resurrected
  //     anything you'd just removed on the next navigation.
  //
  // The flag is cleared whenever a page renders anonymous (logout / expired
  // session), so the next login migrates this device's current picks afresh.
  const SERVER_SYNCED_KEY = 'serverStateSynced';

  let _serverSyncTimer = 0;
  function scheduleServerSync() {
    if (!IS_LOGGED_IN) return;
    clearTimeout(_serverSyncTimer);
    // 400ms — long enough to batch a burst of toggles (clicking through
    // 5 stars rapidly produces one PUT), short enough that closing the
    // tab right after a single click still gets the write through.
    _serverSyncTimer = setTimeout(pushStateToServer, 400);
  }

  function pushStateToServer(opts) {
    _serverSyncTimer = 0;
    fetch('/api/me/state', {
      method:  'PUT',
      headers: { 'Content-Type': 'application/json' },
      // `keepalive` lets the request outlive an unloading document so the
      // pagehide flush below isn't dropped mid-navigation.
      keepalive: !!(opts && opts.keepalive),
      body:    JSON.stringify({
        hiddenFilms:     getHidden(),
        disabledCinemas: getDisabledCinemas(),
        selectedMovies:  getSelectedMovies(),
        favouriteRooms:  getFavouriteRooms()
      })
    }).catch(() => { /* offline / 401 — localStorage still has the write */ });
  }

  // Flush a still-pending debounced sync synchronously as the page goes
  // away. Without this, a toggle made <400ms before a navigation (clicking a
  // film card right after hiding one) would never reach the server, and the
  // next page — now server-authoritative — would overwrite it with the stale
  // remote state. Runs on pagehide and on tab-hide (the reliable signals;
  // beforeunload is unreliable on mobile).
  function flushServerSync() {
    if (!IS_LOGGED_IN || !_serverSyncTimer) return;
    clearTimeout(_serverSyncTimer);
    pushStateToServer({ keepalive: true });
  }
  window.addEventListener('pagehide', flushServerSync);
  document.addEventListener('visibilitychange', () => {
    if (document.visibilityState === 'hidden') flushServerSync();
  });

  async function bootMergeFromServer() {
    if (!IS_LOGGED_IN) {
      // Anonymous (incl. just-logged-out): re-arm migration so the next
      // login carries this device's current local picks up exactly once.
      try { localStorage.removeItem(SERVER_SYNCED_KEY); } catch {}
      return;
    }
    try {
      const resp = await fetch('/api/me/state', { headers: { 'Accept': 'application/json' } });
      if (!resp.ok) return;
      const remote     = await resp.json();
      const firstSync  = localStorage.getItem(SERVER_SYNCED_KEY) !== '1';

      if (firstSync) {
        const union = (local, srv) => [...new Set([...(local || []), ...(srv || [])])].sort();
        _lsSet('hiddenFilms',     union(getHidden(),          remote.hiddenFilms));
        _lsSet('disabledCinemas', union(getDisabledCinemas(), remote.disabledCinemas));
        _lsSet('selectedMovies',  union(getSelectedMovies(),  remote.selectedMovies));
        _lsSet('favouriteRooms',  union(getFavouriteRooms(),  remote.favouriteRooms));
        try { localStorage.setItem(SERVER_SYNCED_KEY, '1'); } catch {}
        pushStateToServer();   // persist the migrated union
      } else {
        // Server authoritative — mirror it locally so removals propagate.
        // `_lsSet` (not setHidden/…) avoids re-triggering a redundant push.
        const fromServer = srv => (srv || []).slice().sort();
        _lsSet('hiddenFilms',     fromServer(remote.hiddenFilms));
        _lsSet('disabledCinemas', fromServer(remote.disabledCinemas));
        _lsSet('selectedMovies',  fromServer(remote.selectedMovies));
        _lsSet('favouriteRooms',  fromServer(remote.favouriteRooms));
      }
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
    var clampedAttempt = Math.max(0, Math.min(attempt, _POSTER_RETRY_MAX));
    var delay = 2;
    for (var i = 0; i < clampedAttempt; i++) delay *= 3;
    return delay;
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
    // Record the boot day so the carousel can derive slide direction from the
    // displayed day (see `_appliedDay`), and sync the pills to the URL-applied
    // day (the eager DOMContentLoaded sync ran before `applyFiltersFromURL`).
    const dateSel = document.getElementById('date-filter');
    if (dateSel) _appliedDay = dateSel.value;
    syncDayPills();
    applyFilters();
    // Reveal the grid now that the first filter pass has set final visibility —
    // drops the anti-FOUC cloak the head script added (repertoire.scala.html /
    // the `grid-cloak` rule in _sharedStyles). No-op on views without the class.
    document.documentElement.classList.remove('grid-cloak');
  }
  window.bootView = bootView;

  // Every page is under `/{city}/…`. CURRENT_CITY is the global from
  // `_sharedJsConfig`.
  const CITY_BASE = '/' + CURRENT_CITY;
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
  // Desktop (fine pointer) takes a longer, more deliberate glide — 1.5× the base —
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
    const element = document.getElementById('swipe-hint');
    if (!element) return;                                        // not the listing page
    if (_hintGet(SWIPE_HINT_DONE)) return;                 // retired by a past swipe
    const today = new Date().toLocaleDateString('sv', { timeZone: 'Europe/Warsaw' });
    if (_hintGet(SWIPE_HINT_DAY) === today) return;        // already shown today
    _hintSet(SWIPE_HINT_DAY, today);
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
    _hintSet(SWIPE_HINT_DONE, '1');
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
    requestAnimationFrame(() => {
      track.style.transition = 'transform ' + ms + 'ms ease';
      track.style.transform  = 'translateX(' + end + 'px)';
    });
    let done = false;
    const finish = () => {
      if (done) return;
      done = true;
      track.removeEventListener('transitionend', finish);
      commitDay(targetValue);
    };
    track.addEventListener('transitionend', finish);
    setTimeout(finish, ms + 60);   // fallback if transitionend is missed
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
  window.animateToDay = animateToDay;

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
    if (matchMedia('(prefers-reduced-motion: reduce)').matches) {
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
    const ms = swipeAnimMs();
    track.style.transition = 'transform ' + ms + 'ms ease';
    setTrack(0);
    let done = false;
    const clear = () => {
      if (done) return;
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
        // Mount BOTH neighbour columns so either drag direction reveals the
        // right day from the screen edge, parked at -100vw. The day ring wraps,
        // so every direction has a destination.
        _drag.armed = armTrack(neighborDay(-1), neighborDay(1));
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
    // pill highlight previews the day a release would land on.
    if (_drag.armed) { setTrack(dx); previewDayPillForDrag(dx); }
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

  document.addEventListener('DOMContentLoaded', () => {
    // One-time shell init — navbar chrome (day pills, hidden-films badge) — then
    // the grid-dependent boot.
    syncDayPills();
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
