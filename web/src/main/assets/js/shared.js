  // Page-level flags surfaced by the server-rendered template so the JS
  // can decide where to read/write personalization state. When LOGGED_IN
  // is true the boot path merges localStorage with the server's per-country
  // hiddenFilms and every subsequent hide/unhide fires its own immediate
  // per-title request; otherwise localStorage is canonical and (Phase D) a
  // once-per-day toast nags anonymous users.

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
  // pages that render the navbar but have no real grid (the stub
  // applyFilters is harmless, but a page without one at all mustn't throw).
  function onSortChange() {
    if (typeof applyFilters === 'function') applyFilters();
  }

  // Where THIS deployment is mounted: '' for a country that owns its domain
  // (`kinowo.net/poznan/…`), '/uk' for one that shares the brand domain
  // (`showtimes.cc/uk/kent/…`). Read off the current path — every page this file
  // runs on is `{mount}/{city}/…` — rather than emitted as a server-rendered
  // constant, so one asset URL serves every country and the browser parses it
  // once. Falls back to the root if the path somehow doesn't name the city.
  function mountPrefix() {
    var marker = '/' + CURRENT_CITY + '/';
    var at = window.location.pathname.indexOf(marker);
    return at < 0 ? '' : window.location.pathname.slice(0, at);
  }

  // City picker (Filtry → Miasto) changed — remember the choice so the bare
  // `/` landing bounces here next time, then navigate to the chosen city's
  // repertoire root. A full navigation (not a view-swap) because the whole
  // corpus changes. No-op when the value is the current city.
  // Remember the city so the bare `/` landing bounces here next time.
  //
  // THE CLIENT OWNS THIS COOKIE NOW. The listing used to arrive with a
  // `Set-Cookie`, and Cloudflare bypasses the cache for any response carrying
  // one — measured: the page went DYNAMIC -> BYPASS with this as the only
  // thing left on it. Same name, path and lifetime as the header it replaces,
  // so the landing bounce behaves exactly as before.
  function rememberCity(slug) {
    var prefix = mountPrefix();
    document.cookie = 'city=' + slug + ';path=' + (prefix || '') + '/;max-age=' + (60 * 60 * 24 * 365);
  }
  window.rememberCity = rememberCity;

  function onCityChange(slug) {
    if (!slug || slug === CURRENT_CITY) return;
    rememberCity(slug);
    window.location.href = mountPrefix() + '/' + slug + '/';
  }
  window.onCityChange = onCityChange;

  // Filtry → Miasto row — navigates to the unified `/` picker
  // (`landing.scala.html`) rather than opening an in-page modal (the retired
  // `_cityPickerModal`): one picker for both "change city" and "change
  // country" instead of a separate control for each. `?pick=city` so that
  // picker asks rather than bouncing straight back to this same city.
  function goToCityPicker() {
    window.location.href = mountPrefix() + '/?pick=city';
  }
  window.goToCityPicker = goToCityPicker;

  // Client-side language switching (`t`/`applyLanguage`/`onLanguageChange`,
  // the `I18N_PACKS` pack, and the boot-time apply) lives in the standalone
  // `i18n.js`, loaded before this file — see that file's own doc comment for
  // why it isn't folded in here (landing.scala.html needs it too, and never
  // loads shared.js at all).

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
    allCb.autocomplete = 'off';   // see KEEPING BUILT CONTROLS OUT OF THE RESTORE QUEUE
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
      checkbox.autocomplete = 'off';
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
    allCb.autocomplete = 'off';   // see KEEPING BUILT CONTROLS OUT OF THE RESTORE QUEUE
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
        checkbox.autocomplete = 'off';
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
  // disabledCinemas is device-local ONLY — no server round-trip at all, ever
  // (see the "Server sync" section below, which no longer models this field
  // in either direction).
  function getDisabledCinemas()  { return _lsGet('disabledCinemas') || []; }
  function setDisabledCinemas(l) { _lsSet('disabledCinemas', l); }
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
      // `data-slug` is the server's own `Slugify` output, so card-tap lands on
      // the canonical address directly instead of bouncing through the legacy
      // query form's 301. Re-implementing the fold in JS would just give the
      // rule a second place to drift. Falls back to the query form for a title
      // that folds to nothing (the server renders that one in place).
      if (col) window.location.href = col.dataset.slug
        ? CITY_BASE + '/movie/' + col.dataset.slug
        : CITY_BASE + '/movie?title=' + encodeURIComponent(col.dataset.title);
    }
  });


  // ── Showings truncation ─────────────────────────────────────────────────
  //
  // After each filter pass, caps visible showings per card at ~10 visual
  // rows. Hides overflow at cinema-group boundaries and shows a
  // "… +N seansów" link to the /movie page. Mirrors the iOS app's collapse.
  //
  // Called from applyFilters() in the repertoire view after visibility has
  // been set on badges / groups. Walks the already-computed visibility —
  // no extra DOM measurement. The /movie page has no applyFilters and
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

  // Truncation folds rows away with the `.truncated` CLASS; the filters hide
  // them with inline `style.display` (`setVisible`). Keeping the two on separate
  // channels is what makes them safe to interleave: neither can undo the other's
  // decision by accident, and unfolding a row hands it straight back to whatever
  // the filter had decided for it. Nothing here reads or writes inline display.
  const TRUNCATED = 'truncated';

  function undoTruncation() {
    document.querySelectorAll('.' + TRUNCATED).forEach(element => element.classList.remove(TRUNCATED));
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
          cinemaGroup.classList.add(TRUNCATED);
          continue;
        }

        const pillRows = Math.max(1, Math.ceil(visibleBadges / _PILLS_PER_ROW));
        const cinemaLines = (hasCinemaHeaders ? 1 : 0) + pillRows;

        if (lineCount + dayLines + cinemaLines <= _MAX_SHOWINGS_ROWS) {
          cinemaGroup.classList.remove(TRUNCATED);
          dayHasVisible = true;
          dayLines += cinemaLines;
        } else {
          hidden += visibleBadges;
          cinemaGroup.classList.add(TRUNCATED);
          capped = true;
        }
      }

      if (dayHasVisible) {
        lineCount += dayLines;
      } else if (capped) {
        dateGroup.classList.add(TRUNCATED);
      }
    }

    if (hidden > _MIN_HIDDEN) {
      link.textContent = '… +' + hidden + ' ' + _showtimeNoun(hidden);
      link.style.display = '';
    } else {
      // Too few folded away to be worth a link — unfold them again.
      if (hidden > 0) {
        for (const dateGroup of dateGroups) {
          dateGroup.classList.remove(TRUNCATED);
          for (const cinemaGroup of dateGroup.querySelectorAll('.cinema-group'))
            cinemaGroup.classList.remove(TRUNCATED);
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

  // ── Film page: "other cities" popup ──────────────────────────────────────
  //
  // The sibling-city links themselves are server-rendered and already in the
  // DOM (`_filmDetailContent`) — this only toggles whether a visitor SEES
  // them. Same backdrop/card shape as the login modal.
  function openOtherCitiesModal() {
    const modal = document.getElementById('other-cities-modal-backdrop');
    if (modal) modal.classList.add('open');
  }
  function closeOtherCitiesModal() {
    const modal = document.getElementById('other-cities-modal-backdrop');
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
  window.openOtherCitiesModal = openOtherCitiesModal;
  window.closeOtherCitiesModal = closeOtherCitiesModal;

  // ── Who is looking at this page ──────────────────────────────────────────
  //
  // The server does not know, and deliberately does not ask. The listing page is
  // handed to Cloudflare with an `s-maxage`, and a shared cache may only ever
  // hold a response that is byte-identical for every client — so the moment the
  // HTML carried an avatar, a display name or an `IS_LOGGED_IN = true`, one
  // visitor's copy could be served to the next. `_authMenu` therefore renders the
  // signed-out slot for everybody and the answer is fetched here, per client,
  // from `/api/me` — which is `no-store` precisely so this cannot be cached back
  // into the same problem.
  //
  // Which makes the DOM the single source of truth for "am I signed in": the
  // avatar menu is present exactly when somebody is. `signedOutPageIsStale` and
  // `sessionVerifyUrl` already decided that way; the server-sync and nag paths
  // used to read a page constant instead, and now agree with them.
  function isLoggedIn() { return !!document.getElementById('auth-menu'); }

  // The avatar dropdown, built where `_authMenu` used to render it: alongside the
  // login pill, which is hidden rather than removed because the sign-out chain
  // puts it straight back (see `beginSignOut`).
  //
  // `textContent` throughout — the name and e-mail come from an OAuth provider's
  // profile, which is somebody else's text, and this is the one place it reaches
  // the page without Twirl's escaping in front of it.
  function buildAuthMenu(me) {
    const login = document.getElementById('auth-login');
    if (!login || document.getElementById('auth-menu')) return;

    const menu = document.createElement('div');
    menu.className = 'auth-menu';
    menu.id        = 'auth-menu';
    menu.addEventListener('click', toggleAuthMenu);

    if (me.avatarUrl) {
      const img = document.createElement('img');
      img.src = me.avatarUrl; img.className = 'auth-avatar'; img.alt = '';
      menu.appendChild(img);
    } else {
      const initial = document.createElement('span');
      initial.className   = 'auth-avatar-fallback';
      initial.textContent = (me.displayName || '').charAt(0).toUpperCase() || '?';
      menu.appendChild(initial);
    }

    const name = document.createElement('span');
    name.className   = 'auth-name';
    name.textContent = String(me.displayName || me.email || window.t('auth.account'))
      .split(/[ @]/)[0];
    menu.appendChild(name);

    // The logout route is `+ nocsrf`: a form post from a third-party page can
    // only log a visitor OUT, not in. A plain form post, so the sign-out redirect
    // chain runs in the browser and clears the sibling domain's cookie on the
    // way; the delegated `submit` listener below swaps the slot at once.
    const form = document.createElement('form');
    form.method    = 'post';
    form.action    = mountPrefix() + '/auth/logout';
    form.className = 'auth-logout-form';
    const out = document.createElement('button');
    out.type        = 'submit';
    out.className   = 'auth-logout-btn';
    out.setAttribute('data-i18n', 'auth.logout');
    out.textContent = window.t('auth.logout');
    form.appendChild(out);

    const dropdown = document.createElement('div');
    dropdown.className = 'auth-dropdown';
    dropdown.id        = 'auth-dropdown';
    dropdown.appendChild(form);
    menu.appendChild(dropdown);

    login.style.display = 'none';
    login.parentNode.appendChild(menu);
  }

  // Ask once per document, and hand every caller the same promise so the boot
  // order is "hydrate, then everything that depends on knowing".
  let _authHydration = null;
  // Whether `/api/me` ANSWERED — signed in (200) or signed out (401) — rather
  // than failing (offline, a 5xx). A page that could not ask renders signed
  // out, the safe way to be wrong about what to SHOW; it must not also act as
  // if the account were gone and drop what this device still owes it (see
  // `bootMergeFromServer`).
  let _authAnswered = false;
  // The last answer this device had, kept across pages: '1' while `/api/me`
  // last said signed in. A page that cannot ask goes by it — edits made
  // offline by someone who was signed in are owed to the account (see
  // `_writeHiddenFilms`), and edits by someone who was not are nobody's.
  const SIGNED_IN_KEY = 'signedIn';
  function sessionUnconfirmed() {
    if (isLoggedIn() || _authAnswered) return false;
    try { return localStorage.getItem(SIGNED_IN_KEY) === '1'; } catch { return false; }
  }
  function hydrateAuth() {
    if (_authHydration) return _authHydration;
    // Nothing to sign in to (a deployment with no OAuth secrets), or a page that
    // already carries the menu: either way there is nothing to ask.
    if (!HAS_OAUTH_PROVIDERS || isLoggedIn()) {
      _authAnswered = true;
      return (_authHydration = Promise.resolve());
    }
    _authHydration = fetch(mountPrefix() + '/api/me', { credentials: 'same-origin' })
      .then(response => {
        _authAnswered = response.ok || response.status === 401;
        try {
          if (response.ok) localStorage.setItem(SIGNED_IN_KEY, '1');
          else if (response.status === 401) localStorage.removeItem(SIGNED_IN_KEY);
        } catch {}
        return response.ok ? response.json() : null;
      })
      .then(me => { if (me) buildAuthMenu(me); })
      // Offline, or the request failed: the page stays signed out, which is the
      // safe way to be wrong — it offers the way back in rather than an avatar
      // for a session we could not confirm.
      .catch(() => {});
    return _authHydration;
  }
  window.isLoggedIn  = isLoggedIn;
  window.hydrateAuth = hydrateAuth;

  // ── Signing out ───────────────────────────────────────────────────────────
  //
  // The sign-out is a plain form POST that walks a redirect chain — this
  // domain, then the sibling one, so both cookies go — and lands the visitor on
  // a freshly rendered anonymous page. Two things still leave the avatar on
  // screen:
  //
  //   • The chain is a round trip across two domains. Until it lands, the page
  //     the visitor pressed the button on is the signed-in one they were
  //     already looking at, avatar and all.
  //   • The browser may answer that landing out of its OWN cache. The HTML is
  //     the same bytes signed in or out now, so it cannot itself be stale — but
  //     an entry stored before that shipped still carries an avatar, and a cached
  //     `/api/me` would rebuild one (which is why that endpoint is `no-store`).
  //
  // So the section empties the moment the form is submitted, and on arrival, a
  // page that still renders signed in is fetched again for real. The mark is
  // cleared BEFORE the reload, so a visitor who genuinely is still signed in
  // (the far half of the chain failed) reloads once rather than forever.
  const SIGNED_OUT_KEY = 'signedOut';

  // Private-mode Safari throws on sessionStorage rather than returning null, and
  // a sign-out that throws is worse than one that doesn't self-heal.
  function signOutMark() {
    try { return sessionStorage.getItem(SIGNED_OUT_KEY) === '1'; } catch (e) { return false; }
  }
  function setSignOutMark(on) {
    try {
      if (on) sessionStorage.setItem(SIGNED_OUT_KEY, '1');
      else    sessionStorage.removeItem(SIGNED_OUT_KEY);
    } catch (e) { /* no storage → no self-heal, but the sign-out itself still runs */ }
  }

  // Swap the avatar for the login pill, which the page renders and `hydrateAuth`
  // hid precisely so this needs no markup and no label of its own.
  //
  // Hidden rather than removed: the form being submitted is INSIDE the menu, and
  // a form detached from the document has its submission aborted outright.
  // The pill comes back with `display = ''` rather than a literal value, so the
  // slot is handed to the stylesheet and the mobile rule that keeps "Zaloguj"
  // out of the navbar still applies — what's on screen matches what an
  // anonymous render of this page would have put there.
  function beginSignOut() {
    setSignOutMark(true);
    const menu  = document.getElementById('auth-menu');
    const login = document.getElementById('auth-login');
    if (menu)  menu.style.display = 'none';
    if (login) login.style.display = '';
  }

  // Did this page load come back signed in at the far end of a sign-out — i.e.
  // did the browser answer it from cache? Split from the reload so a page test
  // can assert the decision without a navigation tearing the page down
  // mid-assertion.
  function signedOutPageIsStale() {
    return signOutMark() && !!document.getElementById('auth-menu');
  }

  function settleSignOut() {
    if (!signOutMark()) return;
    const stale = signedOutPageIsStale();
    setSignOutMark(false);
    if (stale) window.location.reload();
  }

  // ── A sign-out that happened somewhere else ──────────────────────────────
  //
  // The mark above is `sessionStorage`, which is per TAB and per ORIGIN, so it
  // only ever heals the tab the sign-out was pressed in. Sign out in one tab and
  // switch to another — or to the other domain, which cannot share that storage
  // at all — and the page sitting there still shows an avatar for a session that
  // no longer exists. Nothing the server sends can reach it: a tab that never
  // asks again hears nothing, whatever the cookie or cache headers say.
  //
  // So the page asks. Coming back to the foreground still believing it is signed
  // in, it checks whether it still is, and reloads if it was wrong. Cheap, and
  // only for signed-in pages: an anonymous one has nothing to be wrong about.
  //
  // The URL comes off the sign-out form, which `buildAuthMenu` gave this
  // deployment's mount point (`/uk/auth/logout`) — so no second path derivation,
  // and the absence of the form is itself the "not signed in" answer.
  function sessionVerifyUrl() {
    var form = document.querySelector('.auth-logout-form');
    if (!form) return null;                       // signed out: nothing to verify
    return form.action.replace(/auth\/logout\/?$/, '') + 'api/me';
  }
  window.sessionVerifyUrl = sessionVerifyUrl;

  function verifySessionOnReturn() {
    if (document.hidden) return;
    var url = sessionVerifyUrl();
    if (!url) return;
    fetch(url, { credentials: 'same-origin' })
      .then(function (response) { if (response.status === 401) window.location.reload(); })
      // Offline, or the request failed: leave the page exactly as it is. A
      // network blip is not evidence that the visitor signed out.
      .catch(function () {});
  }
  window.verifySessionOnReturn = verifySessionOnReturn;
  document.addEventListener('visibilitychange', verifySessionOnReturn);

  // Delegated, because shared.js loads after the navbar on some pages and before
  // it on others, and a `submit` listener on the document catches both.
  document.addEventListener('submit', event => {
    const form = event.target;
    if (form && form.classList && form.classList.contains('auth-logout-form')) beginSignOut();
  });

  window.beginSignOut         = beginSignOut;
  window.signedOutPageIsStale = signedOutPageIsStale;
  window.settleSignOut        = settleSignOut;
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
    label.appendChild(document.createTextNode(' ' + (CINEMA_PILLS[cinema] || cinema)));
    return label;
  }

  // A collapsible area group: a header row (area checkbox + name + chevron) over
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

    header.appendChild(areaCb);
    header.appendChild(name);
    header.appendChild(right);

    const body = document.createElement('div');
    body.className = 'submenu-list cinema-area-cinemas';
    body.style.display = 'none';                          // collapsed by default
    area.cinemas.forEach(c => body.appendChild(buildCinemaRow(c)));

    header.onclick = () => {
      const opening = body.style.display === 'none';
      body.style.display = opening ? '' : 'none';
      chevron.classList.toggle('open', opening);
    };

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
      ALL_CINEMAS.forEach(cinema => list.appendChild(buildCinemaRow(cinema)));
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
    const loc = {
      title: window.t('areaPicker.title'), subtitle: window.t('areaPicker.subtitle'),
      all: window.t('areaPicker.all'), confirm: window.t('areaPicker.confirm'),
    };

    const overlay = document.createElement('div');
    overlay.id = 'area-picker-overlay';
    overlay.style.cssText = 'position:fixed;inset:0;z-index:1000;background:rgba(5,5,20,.72);' +
      'display:flex;align-items:center;justify-content:center;padding:20px;';
    const modal = document.createElement('div');
    modal.style.cssText = 'width:100%;max-width:420px;max-height:85vh;overflow:auto;background:#14142e;' +
      'border:1px solid #2a2a5e;border-radius:14px;padding:20px;';
    const title = document.createElement('h2');
    title.style.cssText = 'font-size:1.25rem;margin:0 0 .25rem;';
    title.textContent = loc.title || 'Choose areas';
    const sub = document.createElement('p');
    sub.style.cssText = 'color:#9a9ac0;font-size:.9rem;margin:0 0 1rem;';
    sub.textContent = loc.subtitle || '';
    modal.appendChild(title);
    modal.appendChild(sub);

    const rowCss = 'display:flex;align-items:center;gap:.6rem;padding:.6rem .2rem;' +
      'font-size:1.05rem;border-bottom:1px solid #23234d;cursor:pointer;';

    const allLabel = document.createElement('label');
    allLabel.style.cssText = rowCss + 'font-weight:600;';
    const allCb = document.createElement('input');
    allCb.type = 'checkbox';
    allCb.autocomplete = 'off';
    allCb.checked = true;
    allCb.id = 'area-picker-all';
    const allName = document.createElement('span');
    allName.textContent = loc.all || 'All areas';
    allLabel.appendChild(allCb);
    allLabel.appendChild(allName);
    modal.appendChild(allLabel);

    const boxes = [];
    areas.forEach(area => {
      const label = document.createElement('label');
      label.style.cssText = rowCss;
      const cb = document.createElement('input');
      cb.type = 'checkbox';
      cb.autocomplete = 'off';
      cb.checked = true;                                       // all pre-selected
      cb.dataset.areaSlug = area.slug;
      cb.onchange = syncAreaPickerAll;
      const name = document.createElement('span');
      name.textContent = area.name;
      const count = document.createElement('span');
      count.style.cssText = 'margin-left:auto;color:#6f6f9c;font-size:.85rem;';
      count.textContent = area.cinemas.length;
      label.appendChild(cb);
      label.appendChild(name);
      label.appendChild(count);
      modal.appendChild(label);
      boxes.push({ cb, area });
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
    function syncAreaPickerAll() {
      const picked = boxes.filter(({ cb }) => cb.checked).length;
      allCb.checked = picked === boxes.length;
      allCb.indeterminate = picked > 0 && picked < boxes.length;
    }

    const btn = document.createElement('button');
    btn.style.cssText = 'margin-top:1.1rem;width:100%;padding:.8rem;font:inherit;font-size:1rem;' +
      'font-weight:600;color:#0d0d22;background:#e7b93a;border:none;border-radius:10px;cursor:pointer;';
    btn.textContent = loc.confirm || 'Show';
    btn.onclick = () => {
      // Disable the cinemas of every UNCHECKED area, leaving other cities'
      // entries untouched. Default (all checked) → nothing disabled.
      const disable = [];
      boxes.forEach(({ cb, area }) => { if (!cb.checked) disable.push(...area.cinemas); });
      setDisabledCinemas(disabledCinemasElsewhere().concat(disable));
      try { localStorage.setItem(areaPickerKey(), '1'); } catch (e) {}
      if (overlay.parentNode) overlay.parentNode.removeChild(overlay);
      buildCinemaPanel();
      updateFormatBtn();
      applyFilters();
    };
    modal.appendChild(btn);
    overlay.appendChild(modal);
    document.body.appendChild(overlay);
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
  // `/movie`) or followed whatever link sat under the cursor — so dismissing
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
      noFilms.setAttribute('data-i18n', 'empty.repertoire');
      noFilms.textContent = window.t('empty.repertoire');
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
  // References to KINOWO_LOCALE's OWN arrays — `applyLanguage` (i18n.js)
  // splices a picked language's words into these same arrays in place
  // (never reassigns `KINOWO_LOCALE.day2`), so DAY2/MONTHS stay live through
  // a language switch without this file needing its own applyLanguage hook.
  const DAY2   = KINOWO_LOCALE.day2;
  const MONTHS = KINOWO_LOCALE.months;

  // The long date-header label ("Thursday 4 June", "Czwartek 4 czerwca") —
  // mirrors `DateFormatter.format` (Scala) so the client-side re-render on a
  // language switch matches what the server would have rendered in that
  // language. `KINOWO_LOCALE.daysFull` is Monday-first, matching
  // `LocalDate.getDayOfWeek.getValue - 1`; `getDay()` below is Sunday-first,
  // hence the `+6 % 7` rebase.
  function formatDateLabel(isoDate) {
    const [year, month, day] = isoDate.split('-').map(Number);
    const dayOfWeek       = new Date(year, month - 1, day).getDay();
    const mondayFirstIdx  = (dayOfWeek + 6) % 7;
    const currentYear     = new Date().getFullYear();
    const yearSuffix      = year === currentYear ? '' : ' ' + year;
    return KINOWO_LOCALE.daysFull[mondayFirstIdx] + ' ' + day + ' ' + MONTHS[month - 1] + yearSuffix;
  }

  // The short form the custom-date `#date-filter` option uses ("Czw 21 maja") —
  // mirrors the construction below that used to be inlined at its one call site.
  function formatDateLabelShort(isoDate) {
    const [year, month, day] = isoDate.split('-').map(Number);
    const dayOfWeek = new Date(year, month - 1, day).getDay();
    return DAY2[dayOfWeek] + ' ' + day + ' ' + MONTHS[month - 1];
  }

  // Re-renders every already-drawn date label from its `data-date` ISO
  // source rather than trusting whatever text is currently on screen —
  // called after `applyLanguage` (i18n.js) splices in a new language's
  // day/month arrays above, so the listing's date headers (baked in the
  // deployment's default language at server-render time) actually follow the
  // visitor's picked language instead of staying stuck in the old one.
  function refreshDateLabels() {
    document.querySelectorAll('.date-group[data-date]').forEach(group => {
      const label = group.querySelector('.date-label');
      if (label) label.textContent = formatDateLabel(group.dataset.date);
    });
    const customOption = document.querySelector('#date-filter option[data-date]');
    if (customOption) customOption.textContent = formatDateLabelShort(customOption.dataset.date);
  }
  window.refreshDateLabels = refreshDateLabels;

  let _cachedDay = null, _cachedToday, _cachedTomorrow, _cachedIn7Days;

  // The calendar day the PAGE is on. Two things it deliberately is not:
  //
  //  - the visitor's timezone. Showtimes, their expiry stamps and the midnight
  //    rollover are all resolved in `city.zoneId` server-side, so the day filter
  //    reads the same city clock (`window.CITY_TIMEZONE`) — a Londoner at 23:30
  //    is still on today's listings, though Warsaw has already turned over.
  //  - the visitor's wall clock. `showtimeNow()` is the server's render instant
  //    carried forward by elapsed browser time, which is what the expiry prune
  //    counts on; a device clock set days out can't drag the day filter away
  //    from the listings the page actually shipped with. Only the repertoire
  //    view defines it — elsewhere (/movie, /browse) there is no expiry to
  //    stay in step with, so the browser clock is answer enough.
  //
  // `window.KINOWO_PINNED_TODAY` is set ONLY in page-test renders (fixture data
  // carries absolute past dates; a live clock would age the day filters out).
  function pageToday() {
    if (window.KINOWO_PINNED_TODAY) return window.KINOWO_PINNED_TODAY;
    const nowMs = typeof window.showtimeNow === 'function' ? window.showtimeNow() : Date.now();
    return new Date(nowMs).toLocaleDateString('sv', { timeZone: window.CITY_TIMEZONE });
  }

  function dateBounds() {
    const today = pageToday();
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
  // and the from-time selects are left exactly as the engine restored them, and
  // pressing Back keeps the search you typed, which is the behaviour every
  // engine already gets right.
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

  // ...AND THE RESTORE CAN LAND AFTER THE GRID IS ALREADY DRAWN. Measured in
  // Chromium: the search box is still EMPTY at `DOMContentLoaded`, so the filter
  // pass `bootView` runs sees nothing, and the engine fills the box afterwards.
  // A box reading "spider" above an unfiltered grid is worse than an empty one,
  // so re-filter when the controls stop agreeing with the pass that drew what is
  // on screen. Comparing, rather than re-running unconditionally, keeps an
  // ordinary load to the single pass it has always cost.
  let _appliedSignature = null;
  function filterSignature() {
    const valueOf = id => { const el = document.getElementById(id); return el ? el.value : ''; };
    return ['search-input', 'sort-by', 'from-hour', 'from-minute'].map(valueOf).join('\u0000');
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

    updateFormatBtn();
  }


  // ── Server sync for logged-in users ──────────────────────────────────────
  //
  // HIDDEN FILMS ONLY — disabledCinemas stopped being a server-synced field
  // (it's device-local now, see `setDisabledCinemas`) and this section never
  // touches the legacy `/api/me/state` endpoints at all any more (neither
  // read nor write) — those stay running, unchanged, purely for whatever
  // older app build still calls them.
  //
  // The granular per-country API (`GET`/`PUT`/`DELETE
  // /api/me/:country/hidden-films(/:title)`) replaces the old single bulk
  // PUT. Each hide/unhide is its own idempotent per-title request, fired
  // IMMEDIATELY from the call site that already knows which title changed
  // (hideFilm/restoreFilm/showAllFilms) — there is no debounce, because
  // there is nothing left to batch: the old 400ms window existed only to
  // fold a burst of toggles into one shared PUT body, and a per-title
  // request has no body to share.
  //
  // The boot/country-switch reconcile is two-phase PER COUNTRY, gated by a
  // per-country "migrated" flag:
  //
  //   • FIRST reconcile for a country (flag unset): union local + server,
  //     then PUT every LOCAL-ONLY title individually (there is no bulk
  //     write any more) so anything set while anonymous, or on another
  //     country's session, migrates up. Flag set.
  //   • EVERY reconcile after that (flag set): the SERVER is authoritative
  //     for that country — a 200 replaces localStorage with its set (what
  //     makes a removal STICK: a blind union could only ever add, so it
  //     resurrected anything just removed on the next navigation); a 304
  //     means nothing changed, so localStorage — already current — is left
  //     alone.
  //
  // A per-country ETag/Last-Modified is cached so the 304 path actually
  // fires, and a successful WRITE also refreshes them from its own response
  // headers — no need to wait for the next fetch to warm the cache. The local
  // list is per country too (`hiddenFilms:<cc>`, see `getHidden`), so a 304
  // only has to vouch for this country's own list. Validators are sent only
  // when both hold:
  //   • the country is already synced (a first reconcile must see the body to
  //     union it — replaying a validator from a PREVIOUS login used to 304
  //     straight past the migration);
  //   • no write since the last exchange failed — a failed write forgets the
  //     validators, so the next reconcile takes the server's answer.
  //
  // A write that failed is not lost with it: it is remembered as PENDING
  // (`hiddenFilmsPending:<country>`, one op per title) and replayed over the
  // server's list at the next full reconcile, then sent again — until it
  // lands, or the server refuses it for good (`_hiddenFilmsWriteRefused`).
  // While any is pending, a successful write's validators are not kept: they
  // describe a server list the local one is still ahead of.
  //
  // Every per-country flag, and every pending op, is cleared whenever a page
  // renders anonymous (logout / expired session) — as `/api/me` says, not merely
  // because it could not be reached (offline keeps them, and queues the edits
  // made meanwhile as pending too) — so the next login migrates
  // this device's current picks afresh, exactly as the old single flag did.
  //
  // `language` (the picked UI language, `kinowo_lang` — see `i18n.js`) rides
  // the LEGACY `/api/me/state` document instead — there's no granular
  // endpoint for a single scalar pick, only for the two sets it used to
  // share that document with. It skips the per-country union/authoritative
  // dance above entirely (see `reconcileLanguage`, near `bootMergeFromServer`,
  // for why) but keeps the debounced-push machinery below, narrowed to a
  // `language`-only body now that hiddenFilms/disabledCinemas both left it.
  let _serverSyncTimer = 0;
  // A language pick the account has not confirmed yet — its debounced push is
  // still waiting, it failed, or the page was offline. Persisted so the next
  // signed-in load pushes it instead of adopting the account's OLDER pick over
  // it (see `reconcileLanguage`); forgotten on a confirmed sign-out, with the
  // hidden-films writes owed. Mirrors the apps' `pendingLanguagePush`.
  const PENDING_LANGUAGE_KEY = 'kinowo_lang_pending';
  function _pendingLanguage() { try { return localStorage.getItem(PENDING_LANGUAGE_KEY); } catch { return null; } }
  function _setPendingLanguage(lang) {
    try { if (lang) localStorage.setItem(PENDING_LANGUAGE_KEY, lang); else localStorage.removeItem(PENDING_LANGUAGE_KEY); } catch {}
  }

  // Whether a hidden-films write was refused for good: only the controller's own
  // refusals — 400 (over-long title, unknown country) and 413 (full bucket).
  // Anything else may land if sent again; a 403 in particular is as likely a
  // Cloudflare challenge in front of the app as anything the app said. Same
  // rule as both apps' `HiddenFilmsWriteRefused`.
  function _hiddenFilmsWriteRefused(status) { return status === 400 || status === 413; }
  // Whether a language push was refused for good: only a 400, which is what
  // `UserStateController.put` answers for a language it does not know. Same
  // reasoning as `_hiddenFilmsWriteRefused`, and the same rule as both apps'
  // `LanguagePushRefused`.
  function _languagePickRefused(status) { return status === 400; }

  function scheduleServerSync() {
    if (!isLoggedIn() && !sessionUnconfirmed()) return;
    _setPendingLanguage((() => { try { return localStorage.getItem('kinowo_lang'); } catch { return null; } })());
    if (!isLoggedIn()) return; // could not confirm the session: the next signed-in load pushes it
    clearTimeout(_serverSyncTimer);
    // 400ms — long enough that a rapid run of picker clicks folds into one PUT.
    _serverSyncTimer = setTimeout(pushStateToServer, 400);
  }

  function currentCountryCode() {
    const city = (typeof KINOWO_CATALOG !== 'undefined' ? KINOWO_CATALOG.cities : [])
      .find(c => c.slug === CURRENT_CITY);
    return city ? city.country : 'pl';
  }

  function _hiddenFilmsSyncedKey(country)     { return 'hiddenFilmsSynced:' + country; }
  function _hiddenFilmsEtagKey(country)       { return 'hiddenFilmsEtag:' + country; }
  function _hiddenFilmsLastModifiedKey(country) { return 'hiddenFilmsLastModified:' + country; }
  function _hiddenFilmsPendingKey(country)    { return 'hiddenFilmsPending:' + country; }

  // Pending writes for `country`: `[[method, title], …]`, `title` null for a
  // clear. At most one per title, the latest; a clear supersedes them all.
  function _pendingHiddenFilms(country) {
    try { return JSON.parse(localStorage.getItem(_hiddenFilmsPendingKey(country))) || []; } catch { return []; }
  }
  function _setPendingHiddenFilms(country, ops) {
    try {
      if (ops.length) localStorage.setItem(_hiddenFilmsPendingKey(country), JSON.stringify(ops));
      else localStorage.removeItem(_hiddenFilmsPendingKey(country));
    } catch {}
  }
  function _settlePending(country, method, title, stillPending) {
    const others = title === null ? [] : _pendingHiddenFilms(country).filter(op => op[1] !== title);
    _setPendingHiddenFilms(country, stillPending ? others.concat([[method, title]]) : others);
  }
  // The server's list with the pending ops played over it, in order.
  function _withPending(list, ops) {
    return ops.reduce((acc, [method, title]) =>
      title === null ? [] : method === 'PUT' ? [...new Set([...acc, title])] : acc.filter(t => t !== title), list);
  }

  function _hiddenFilmsUrl(country, title) {
    const base = mountPrefix() + '/api/me/' + country + '/hidden-films';
    return title === undefined ? base : base + '/' + encodeURIComponent(title);
  }

  // A successful write's response is the SAME shape a fetch's 200 is (see
  // UserStateController.respondWithHiddenFilms server-side) — cache its
  // validators so the next fetch/country-switch can 304 off it.
  function _storeHiddenFilmsValidators(country, resp) {
    try {
      const etag = resp.headers.get('ETag');
      const lastModified = resp.headers.get('Last-Modified');
      if (etag) localStorage.setItem(_hiddenFilmsEtagKey(country), etag);
      if (lastModified) localStorage.setItem(_hiddenFilmsLastModifiedKey(country), lastModified);
    } catch {}
  }

  function _forgetHiddenFilmsValidators(country) {
    try {
      localStorage.removeItem(_hiddenFilmsEtagKey(country));
      localStorage.removeItem(_hiddenFilmsLastModifiedKey(country));
    } catch {}
  }

  // The tail of each country's write chain — see `_writeHiddenFilms`.
  const _hiddenFilmsWrites = {};
  // How many writes each country has been asked for on this page — how a
  // reconcile tells that an edit was made while its fetch was out.
  const _hiddenFilmsEdits = {};

  // The one request shape every hiddenFilms write shares. A write that never
  // landed (offline, 401, 5xx) leaves localStorage ahead of the server, so the
  // cached validators stop describing it — forget them, and the next
  // reconcile takes the server's answer instead of 304-ing onto the drift.
  function _writeHiddenFilms(method, country, title) {
    country = country || currentCountryCode();
    _hiddenFilmsEdits[country] = (_hiddenFilmsEdits[country] || 0) + 1;
    const key = title === undefined ? null : title;
    if (!isLoggedIn()) {
      // A page that could not confirm the session (offline) of a visitor who
      // was signed in still owes the edit to the account: the next signed-in
      // reconcile replays it, a page that is sure it is signed out drops it
      // with the rest.
      if (sessionUnconfirmed()) {
        _forgetHiddenFilmsValidators(country);
        _settlePending(country, method, key, true);
      }
      return;
    }
    const failed = retry => {
      _forgetHiddenFilmsValidators(country);
      _settlePending(country, method, key, retry);
    };
    // One write at a time per country, in the order they were made: two
    // concurrent requests may reach the server either way round, and a clear
    // landing after the hide made just behind it wipes that hide.
    const previous = _hiddenFilmsWrites[country] || Promise.resolve();
    const sent = previous.then(() => fetch(_hiddenFilmsUrl(country, title), { method: method }))
      .then(async resp => {
        if (resp.ok) {
          _settlePending(country, method, key, false);
          // Its validators vouch for the server's list as of this write: keep
          // them only when that is exactly the local list. Otherwise (another
          // device changed it, or more writes are owed) the next reconcile
          // would 304 onto a list this page does not hold. Mirrors the apps'
          // `sendPendingChanges`.
          const answer = await resp.json().then(body => body.hiddenFilms || [], () => null);
          const local  = getHidden(country);
          const mirrors = answer !== null && answer.length === local.length && answer.every(t => local.includes(t));
          if (mirrors && _pendingHiddenFilms(country).length === 0) _storeHiddenFilmsValidators(country, resp);
          else _forgetHiddenFilmsValidators(country);
        } else {
          // A refusal that will never change is not worth replaying.
          failed(!_hiddenFilmsWriteRefused(resp.status));
        }
      })
      .catch(() => failed(true));
    _hiddenFilmsWrites[country] = sent;
    return sent;
  }

  function hideFilmOnServer(title, country)   { _writeHiddenFilms('PUT', country, title); }
  function unhideFilmOnServer(title, country) { _writeHiddenFilms('DELETE', country, title); }
  function clearHiddenFilmsOnServer(country)  { _writeHiddenFilms('DELETE', country); }
  // `i18n.js` (a separate script, loaded on every page this one is) calls
  // this from `onLanguageChange` so an explicit language pick reaches the
  // server too — same cross-file hook shape as `window.refreshDateLabels`.
  window.scheduleServerSync = scheduleServerSync;

  // Called for a pick (`onLanguageChange` in i18n.js, via the debounce in
  // `scheduleServerSync` above), by `reconcileLanguage`, and by the unload flush
  // below — hiddenFilms/disabledCinemas both left this mechanism, so the body
  // carries `language` alone now. `language` rides along only
  // when THIS device has an explicit pick — never the resolved default a
  // visitor never chose, which would otherwise stamp e.g. "pl" onto the
  // account the first time a logged-in Polish visitor merely loads a page,
  // and then force Polish on them on an English deployment they sign into
  // next.
  //
  // One push on the wire at a time, each reading the pick current when it goes:
  // two concurrent PUTs can land in either order and leave the account on the
  // older pick (the apps' `sendPendingLanguage`). The unload flush alone goes
  // straight out — nothing chained behind a response runs once the page is gone.
  //
  // Each chained push gives up after LANGUAGE_PUSH_TIMEOUT_MS: one that hangs would
  // otherwise hold every later pick behind it for the life of the page. Given up is
  // "not confirmed", like offline — the pick stays pending and the next push or
  // reconcile resends it.
  const LANGUAGE_PUSH_TIMEOUT_MS = 10000;
  let _languagePush = Promise.resolve();
  function pushStateToServer(opts) {
    _serverSyncTimer = 0;
    if (opts && opts.keepalive) return _sendLanguage(true);
    _languagePush = _languagePush.then(() => _sendLanguage(false));
    return _languagePush;
  }
  function _sendLanguage(keepalive) {
    const lang = (() => { try { return localStorage.getItem('kinowo_lang'); } catch { return null; } })();
    if (!lang) return; // nothing left to push
    const abort = new AbortController();
    const giveUp = setTimeout(() => abort.abort(), LANGUAGE_PUSH_TIMEOUT_MS);
    return fetch(mountPrefix() + '/api/me/state', {
      method:  'PUT',
      headers: { 'Content-Type': 'application/json' },
      // `keepalive` lets the request outlive an unloading document so the
      // pagehide flush below isn't dropped mid-navigation.
      keepalive: keepalive,
      signal:  abort.signal,
      body:    JSON.stringify({ language: lang })
    }).then(resp => {
      // Landed: the account holds it — unless a newer pick is already owed.
      if (resp.ok && _pendingLanguage() === lang) _setPendingLanguage(null);
      // Refused for good (a language this server does not know): it can never
      // land, so stop owing it, and take the account's pick instead — without
      // pushing this one back, which is what was just refused.
      else if (_languagePickRefused(resp.status) && _pendingLanguage() === lang) {
        _setPendingLanguage(null);
        reconcileLanguage({ adoptOnly: true });
      }
    }).catch(() => { /* offline or timed out — still pending, the next reconcile resends it */ })
      .finally(() => clearTimeout(giveUp));
  }

  // Flush a still-pending debounced language push synchronously as the page
  // goes away — a pick made <400ms before a navigation must still reach the
  // server. Runs on pagehide and on tab-hide (the reliable signals;
  // beforeunload is unreliable on mobile).
  function flushServerSync() {
    if (!isLoggedIn() || !_serverSyncTimer) return;
    clearTimeout(_serverSyncTimer);
    pushStateToServer({ keepalive: true });
  }
  window.addEventListener('pagehide', flushServerSync);
  document.addEventListener('visibilitychange', () => {
    if (document.visibilityState === 'hidden') flushServerSync();
  });

  async function bootMergeFromServer(attempt) {
    const country = currentCountryCode();
    const edits   = _hiddenFilmsEdits[country] || 0;
    // Signed out as far as this page can tell, but only because `/api/me`
    // could not be asked: nothing here is known to be over, so nothing is
    // forgotten either.
    if (!isLoggedIn() && !_authAnswered) return;
    if (!isLoggedIn()) {
      // Anonymous (incl. just-logged-out): re-arm migration for EVERY country
      // so the next login carries this device's current local picks up
      // exactly once, and drop writes owed to the account just left.
      try {
        Object.keys(localStorage)
          .filter(k => k.indexOf(_hiddenFilmsSyncedKey('')) === 0 || k.indexOf(_hiddenFilmsPendingKey('')) === 0)
          .forEach(k => localStorage.removeItem(k));
        _setPendingLanguage(null);
      } catch {}
      return;
    }
    try {
      const firstSync = localStorage.getItem(_hiddenFilmsSyncedKey(country)) !== '1';
      const headers = { 'Accept': 'application/json' };
      if (!firstSync) {
        const etag = localStorage.getItem(_hiddenFilmsEtagKey(country));
        const lastModified = localStorage.getItem(_hiddenFilmsLastModifiedKey(country));
        if (etag) headers['If-None-Match'] = etag;
        else if (lastModified) headers['If-Modified-Since'] = lastModified;
      }

      const resp = await fetch(_hiddenFilmsUrl(country), { headers });
      if (resp.status === 304) return; // proven unchanged — localStorage is already current
      if (!resp.ok) return;
      const remote = await resp.json();
      // An edit made while the fetch was out is in the local list but maybe not
      // in this answer, which replacing the list with would undo on screen. Ask
      // again once the writes queued meanwhile have gone (bounded: a visitor
      // cannot keep editing faster than a round trip for long).
      if ((_hiddenFilmsEdits[country] || 0) !== edits) {
        if ((attempt || 0) >= 3) return;
        await (_hiddenFilmsWrites[country] || Promise.resolve());
        return bootMergeFromServer((attempt || 0) + 1);
      }

      _storeHiddenFilmsValidators(country, resp);

      // A first sync unions this country's local list in; after that the
      // server's list replaces it (server authoritative, so removals
      // propagate). Either way the writes this device still owes (see the
      // section comment) are played over the result and sent again — in
      // order, as every write is (see `_writeHiddenFilms`) — and only then
      // does the union migrate up what the list holds that the server does
      // not (there is no bulk write any more). A first sync with writes owed
      // is a page that could not reach the server before: without them the
      // union would bring back what they removed. `setHidden` is a pure
      // local write, so nothing is sent back out from here but the above.
      const serverList = remote.hiddenFilms || [];
      const pending    = _pendingHiddenFilms(country);
      const list       = _withPending(firstSync ? [...new Set([...getHidden(country), ...serverList])] : serverList, pending).sort();
      setHidden(list, country);
      const localOnly  = firstSync ? list.filter(t => !serverList.includes(t)) : [];
      pending.forEach(([method, title]) => _writeHiddenFilms(method, country, title === null ? undefined : title));
      localOnly.forEach(title => hideFilmOnServer(title, country));
      try { localStorage.setItem(_hiddenFilmsSyncedKey(country), '1'); } catch {}

      applyFilters();
    } catch (e) { /* network blew up — localStorage is still usable */ }
  }

  // `language` reconciles independently of `bootMergeFromServer` above —
  // deliberately a SEPARATE fetch, not a field read off that call's response:
  // the granular hidden-films endpoint's body is `{"hiddenFilms": […]}` only,
  // it never carries `language`, so reading it off `remote` there would
  // silently never fire. Also a single pick, not a set, so it skips the
  // union / migration-flag dance `bootMergeFromServer` needs entirely —
  // there is no "removed on another device" case a blind overwrite could
  // wrongly resurrect, so every reconcile (first or not) uses the same
  // rule: the ACCOUNT's explicit pick wins whenever it has one (restored on
  // login, per spec); otherwise this device's own explicit pick, if any,
  // becomes the account's. Tested separately from the two hiddenFilms
  // reconcile tests so a change to one never masks a regression in the
  // other.
  async function reconcileLanguage(opts) {
    if (!isLoggedIn()) return;
    const adoptOnly = !!(opts && opts.adoptOnly);
    // A pick the account has not confirmed is newer than anything it holds:
    // push it rather than let the account's value overwrite it.
    if (_pendingLanguage()) return pushStateToServer();
    try {
      const resp = await fetch(mountPrefix() + '/api/me/state', { headers: { 'Accept': 'application/json' } });
      if (!resp.ok) return;
      const remote = await resp.json();
      if (_pendingLanguage()) return pushStateToServer(); // picked while the fetch was out
      const localLang = localStorage.getItem('kinowo_lang');
      if (remote.language && remote.language !== localLang) {
        localStorage.setItem('kinowo_lang', remote.language);
        if (typeof window.applyLanguage === 'function') window.applyLanguage(remote.language);
      } else if (!remote.language && localLang && !adoptOnly) {
        pushStateToServer();
      }
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
    if (isLoggedIn() || !HAS_OAUTH_PROVIDERS) return;
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
    // The chain in data-fallbacks is never consumed — the inline onerror
    // walks it through the data-fallback-index cursor — so rewinding is
    // just resetting the cursor.
    delete img.dataset.fallbackIndex;
    img.style.display = '';
    img.nextElementSibling.style.display = 'none';
    img.src = _posterCacheBust(img.dataset.originalSrc, gen);
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
    noteFiltersApplied();
    // Reveal the grid now that the first filter pass has set final visibility —
    // drops the anti-FOUC cloak the head script added (repertoire.scala.html /
    // the `grid-cloak` rule in _sharedStyles). No-op on views without the class.
    document.documentElement.classList.remove('grid-cloak');
    // First visit to a split city (London): ask which areas to show. No-op on a
    // flat city or once the visitor has already chosen.
    maybeShowAreaPicker();
  }
  window.bootView = bootView;

  // Every page is under `{mount}/{city}/…`, where the mount point is empty for a
  // country that owns its domain (`kinowo.net/poznan/`) and a country segment
  // for one that shares the brand domain (`showtimes.cc/uk/kent/`). CURRENT_CITY
  // is the global from `_sharedJsConfig`; the mount point is read off the
  // CURRENT PATH rather than a second server-rendered constant, so this file
  // stays identical for every deployment (one URL, one parse, one bytecode cache
  // — the reason it was externalised at all).
  const CITY_BASE = mountPrefix() + '/' + CURRENT_CITY;
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
    const today = pageToday();                            // the city's day, see dateBounds
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
    // The live grid's folded-away rows come along in the clone. A preview is
    // untruncated (it shows one day, not ten), and `applyFiltersForDay` only
    // speaks inline display — so drop the class rather than leave rows hidden
    // by a decision that was made for a different day's budget.
    clone.querySelectorAll('.' + TRUNCATED).forEach(element => element.classList.remove(TRUNCATED));
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
    let done = false;
    const finish = () => {
      if (done) return;
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
    if (usesInstantDayChange() || matchMedia('(prefers-reduced-motion: reduce)').matches) {
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
    if (usesInstantDayChange()) { unmountNeighbors(); return; }   // nothing was armed/moved — just clear drag state
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
        // Large cities skip the neighbour-clone preview (see `usesInstantDayChange`)
        // — nothing to mount, but the gesture is still live and can still commit.
        _drag.armed = usesInstantDayChange()
          // Mount BOTH neighbour columns so either drag direction reveals the
          // right day from the screen edge, parked at -100vw. The day ring wraps,
          // so every direction has a destination.
          ? true
          : armTrack(neighborDay(-1), neighborDay(1));
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
    // pill highlight previews the day a release would land on. A large city has
    // no neighbour mounted to reveal, so it skips the track transform and just
    // keeps the pill preview — the grid itself changes only on commit.
    if (_drag.armed) {
      if (!usesInstantDayChange()) setTrack(dx);
      previewDayPillForDrag(dx);
    }
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
    // the grid-dependent boot. None of it depends on who is looking.
    //
    // The city cookie is written HERE rather than sent by the server: the
    // listing has to reach Cloudflare with no `Set-Cookie`, or the edge
    // bypasses it (see `rememberCity`). Landing directly on a city page from
    // search is the common case, so it cannot wait for a city switch.
    rememberCity(CURRENT_CITY);
    syncDayPills();
    updateNavbar();
    bootView();
    maybeShowSwipeHint();   // once-a-day phone nudge, retired on first swipe
    // AFTER the page knows who is looking, and only then: the server-state
    // reconcile is a no-op for an anonymous visitor and the sign-out self-heal
    // looks for the avatar menu, so both would read "signed out" off every page
    // if they ran before `/api/me` answered.
    hydrateAuth().then(() => {
      settleSignOut();      // re-fetch if the page came back signed in
      bootMergeFromServer();
      reconcileLanguage();
    });
  });

  // ── Image-fetch uptime tracker ────────────────────────────────────────────
  //
  // Captures browser-side img load/error outcomes and batches them to
  // /uptime/img-event so the uptime page can show per-origin reliability bars.
  // Event listeners are registered in the capture phase because `load` /
  // `error` don't bubble on <img>.
  //
  // Three things each event carries, all learned the hard way:
  //
  //   host      the ORIGIN CDN, with the `images.weserv.nl` proxy unwrapped.
  //             Posting the proxy's own host merged image.tmdb.org,
  //             m.media-amazon.com and de.web.img3.acsta.net into a single
  //             row, so a total Amazon outage sat invisible behind TMDB's
  //             healthy traffic until it was found by probing prod by hand.
  //   fallback  whether this was an attempt at a non-primary poster URL.
  //             A failed primary is a poster the visitor watched break; a
  //             failed fallback is only a dead spare behind a poster that may
  //             have rendered fine. Merged, either can mask the other.
  //   proxied   whether weserv served this image, so the proxy keeps a row of
  //             its own beside the origins' — see probeProxyFault below for
  //             how that row earns a FAILURE, which is the harder half.
  (function() {
    var pending = [];
    var FLUSH_INTERVAL_MS = 10000;
    var BATCH_SIZE_TRIGGER = 50;
    var PROXY_HOST = 'images.weserv.nl';   // tools.PosterProxy.ProxyHost

    // A weserv outage breaks EVERY poster on the page at once, and the onerror
    // chain then walks each one's spares — so an uncapped "re-fetch it direct"
    // probe would double a broken page's image traffic. A handful of probes is
    // plenty to tell a proxy fault from an origin fault.
    var MAX_PROXY_PROBES = 20;
    var proxyProbes = 0;

    // The CDN that actually holds the image: the proxy's target when weserv
    // fronted it, the URL's own host when PosterProxy skipped the proxy
    // (multikino, m.media-amazon.com, acsta.net all arrive unwrapped).
    function originHost(src) {
      try { return new URL(directUrl(src) || src, window.location.href).host || 'unknown'; }
      catch (e) { return 'unknown'; }
    }

    // A weserv URL with no inner `url=` is still a weserv request worth
    // counting against the proxy's row, so this asks the outer host alone —
    // `directUrl` is the one that needs a target to unwrap.
    function isProxied(src) {
      try { return new URL(src, window.location.href).host === PROXY_HOST; }
      catch (e) { return false; }
    }

    // The origin URL behind a proxied `src`, or '' when it wasn't proxied (or
    // carries no `url=`). PosterProxy hands weserv a scheme-less target
    // (`image.tmdb.org/p/a.jpg`) so the proxy can pick the scheme; put https://
    // back before we fetch it ourselves.
    function directUrl(src) {
      if (!isProxied(src)) return '';
      try {
        var inner = new URL(src, window.location.href).searchParams.get('url');
        if (!inner) return '';
        return /^https?:\/\//i.test(inner) ? inner : 'https://' + inner;
      } catch (e) { return ''; }
    }

    // A failed <img> carries no status code, so a broken proxied poster is as
    // likely to be the origin's fault as the proxy's — and charging it to the
    // origin is exactly how weserv's own failures have hidden before (its edge
    // refusing a domain by policy; an origin 403ing weserv's IP, which weserv
    // returns as a 404 of its own). Settle it the way the July 2026 hunt did by
    // hand: re-request the same image STRAIGHT FROM THE ORIGIN. If that loads,
    // the proxy broke a poster the origin was willing to serve, which is the
    // one outcome attributable to the proxy — post it against the proxy's host.
    // If the direct fetch fails too, the origin's row already carries the
    // failure and the proxy's row stays out of it rather than guessing.
    function probeProxyFault(src) {
      var direct = directUrl(src);
      if (!direct || proxyProbes >= MAX_PROXY_PROBES) return;
      proxyProbes++;
      // Never appended to the document, so the probe's own load/error can't
      // reach the capture-phase listeners below — it would otherwise be
      // recorded as ordinary traffic and probe itself in turn.
      var probe = new Image();
      probe.onload = function() {
        pending.push({
          host: PROXY_HOST,
          success: false,
          error: 'proxy failed, origin served it (' + direct.substring(0, 120) + ')'
        });
      };
      probe.src = direct;
    }

    // A poster carries its primary URL in data-original-src; anything else in
    // the src is a fallback the onerror chain swapped in. The retry loop
    // re-requests the SAME primary with a `_kinowo_t` cache-buster appended,
    // so strip that before comparing or every retry would look like a
    // fallback. Images with no data-original-src (site chrome, icons) have no
    // primary to differ from, so they are never fallbacks.
    function isFallback(img) {
      var primary = img.dataset ? img.dataset.originalSrc : null;
      if (!primary) return false;
      var current = img.getAttribute('src') || '';
      return stripCacheBust(current) !== stripCacheBust(primary);
    }

    function stripCacheBust(url) {
      return url.replace(/[?&]_kinowo_t=\d+/, '');
    }

    function record(target, success) {
      if (!target || target.tagName !== 'IMG') return;
      var src = target.currentSrc || target.src;
      if (!src) return;
      var ev = { host: originHost(src), success: success };
      if (isProxied(src)) ev.proxied = true;
      if (isFallback(target)) ev.fallback = true;
      if (!success) {
        ev.error = 'img load failed (' + src.substring(0, 120) + ')';
        probeProxyFault(src);
      }
      pending.push(ev);
      if (pending.length >= BATCH_SIZE_TRIGGER) flush();
    }

    function flush() {
      if (pending.length === 0) return;
      var body = JSON.stringify({ events: pending });
      pending = [];
      try {
        if (navigator.sendBeacon) {
          navigator.sendBeacon(mountPrefix() + '/uptime/img-event', new Blob([body], { type: 'application/json' }));
        } else {
          fetch(mountPrefix() + '/uptime/img-event', { method: 'POST', body: body, headers: { 'Content-Type': 'application/json' }, keepalive: true });
        }
      } catch (e) { /* tracker must never throw into page code */ }
    }

    document.addEventListener('load',  function(ev) { record(ev.target, true);  }, true);
    document.addEventListener('error', function(ev) { record(ev.target, false); }, true);
    setInterval(flush, FLUSH_INTERVAL_MS);
    window.addEventListener('pagehide', flush);

    // Test seams for PageJsBehaviourSpec, same shape as _posterDelay /
    // _posterCacheBust: assert the classification directly, and read the
    // queued batch without having to intercept sendBeacon and decode a Blob.
    window._imgOriginHost = originHost;
    window._imgIsFallback = isFallback;
    window._imgDirectUrl = directUrl;
    window._imgProbeProxyFault = probeProxyFault;
    // Probes left before the cap. Read synchronously, so a test can assert the
    // cap holds without waiting on 20 image loads — and without assuming the
    // page under test spent none of the budget on its own broken posters.
    window._imgProbeBudget = function() { return MAX_PROXY_PROBES - proxyProbes; };
    // The page's own posters spend the budget as they break, which on a fixture
    // page can exhaust it before a test provokes anything. Hand the budget back
    // so a test starts from a known one.
    window._resetImgProbeBudget = function() { proxyProbes = 0; };
    window._drainImgEvents = function() { var drained = pending; pending = []; return drained; };
    // Peek without draining, so a test can poll for the direct-origin probe's
    // asynchronous verdict and still read the batch it lands in.
    window._peekImgEvents = function() { return pending; };
  })();
