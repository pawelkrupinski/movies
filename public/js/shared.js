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
      const disabled = getDisabledCinemas();
      if (disabled.length > 0 && disabled.length < ALL_CINEMAS.length) {
        parts.push('kina ' + (ALL_CINEMAS.length - disabled.length) + '/' + ALL_CINEMAS.length);
      }
    }
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
    document.getElementById('format-panel').style.display = 'none';
    onFormatChange();
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

  // ── Favourites (per-movie + per-screening) ────────────────────────────────
  //
  // Two parallel sets:
  //   - favouriteMovies:     titles (the .col[data-title] string)
  //   - favouriteScreenings: stable per-pill ids
  //                          ("title|cinema|ISO-datetime", set on the
  //                          badge's data-screening-id attribute server-side)
  //
  // Cookies were the spec — localStorage is what we use here because the
  // hidden-films / disabled-cinemas migration above proved cookies silently
  // drop writes past the ~4 KB cap, which per-screening favourites would hit
  // quickly. /ulubione's filter runs client-side; the server never sees the
  // list. `IS_FAVOURITES_PAGE` is set by the page template (true on /ulubione,
  // false on /), and applyFilters() consults it.

  function getFavMovies()        { return _lsGet('favouriteMovies')     || []; }
  function setFavMovies(titles)  { _lsSet('favouriteMovies',     titles); scheduleServerSync(); }
  function getFavScreenings()    { return _lsGet('favouriteScreenings') || []; }
  function setFavScreenings(ids) { _lsSet('favouriteScreenings', ids);   scheduleServerSync(); }

  function toggleFavMovie(btn) {
    const title = btn.closest('[data-title]').dataset.title;
    const cur   = getFavMovies();
    const idx   = cur.indexOf(title);
    if (idx >= 0) cur.splice(idx, 1); else cur.push(title);
    setFavMovies(cur);
    btn.classList.toggle('is-fav', idx < 0);
    if (idx < 0) maybeShowAnonymousNag();   // only nag when ADDING a favourite, not removing
    // Re-filter only on the ulubione page — on the main page nothing should
    // disappear when you star a movie (it just toggles the visual state).
    if (IS_FAVOURITES_PAGE) preserveScroll(() => applyFilters());
  }

  // Reconstruct a screening's stable id (`title|cinema|datetime`) from
  // the badge's ancestor chain at click time instead of inlining the
  // id on every <a class="badge-time"> element. The structural data is
  // already there: `closest('[data-title]')` is the film card on / and
  // /kina or the `.showtimes-section` wrapper on /film; `closest('.cinema-group')`
  // is the per-cinema wrapper; `closest('.date-group')` is the per-date
  // wrapper. Server-side `_filmShowings` emits these attributes from
  // the same field values that used to feed `data-screening-id`, so
  // the reconstructed string is byte-identical to what was inlined
  // before — existing localStorage entries continue to round-trip.
  function badgeScreeningId(badge) {
    const titleEl  = badge.closest('[data-title]');
    const cinemaEl = badge.closest('.cinema-group');
    const dateEl   = badge.closest('.date-group');
    if (!titleEl || !cinemaEl || !dateEl) return '';
    return titleEl.dataset.title + '|' + cinemaEl.dataset.cinema + '|' + dateEl.dataset.date + 'T' + badge.dataset.time;
  }

  // Pill star click — runs inside an <a class="badge-time">, so we have to
  // stop the booking-link navigation from firing too.
  function toggleFavScreening(event, star) {
    event.preventDefault();
    event.stopPropagation();
    const badge = star.closest('.badge-time');
    if (!badge) return;
    const id  = badgeScreeningId(badge);
    if (!id) return;
    const cur = getFavScreenings();
    const idx = cur.indexOf(id);
    if (idx >= 0) cur.splice(idx, 1); else cur.push(id);
    setFavScreenings(cur);
    star.classList.toggle('is-fav', idx < 0);
    if (idx < 0) maybeShowAnonymousNag();   // only on ADD, not remove
    if (IS_FAVOURITES_PAGE) preserveScroll(() => applyFilters());
    // Toggle changes the pill's permanent width (is-fav stars are always
    // visible); the row may need a fresh end-of-row pre-wrap pass.
    scheduleReflow();
  }

  // One document-level delegated listener for every `.fav-star` click,
  // instead of an inline `onclick="toggleFavScreening(event, this)"` on
  // each of the ~2,600 showtime spans the / page renders. The byte
  // savings on the wire are modest (~30 KB gzipped) but the browser
  // also stops materialising ~2,600 per-element function references
  // during HTML parse — a real CPU / heap win on mobile. Uses `closest`
  // so the click target can be the ★ glyph node itself or anything
  // inside the span (currently it's just text, but future styling
  // wrappers don't break the handler).
  document.addEventListener('click', e => {
    const star = e.target.closest('.fav-star');
    if (star) { toggleFavScreening(e, star); return; }
    // Card poster ★ and ✕ buttons — one delegated listener instead of
    // a per-card inline `onclick="toggleFavMovie(this)"` and
    // `onclick="hideFilm(this)"`. ~190 cards × 2 buttons each ≈ 380
    // inline-onclick attributes that don't need to be inlined and
    // would otherwise re-materialise as function references during
    // HTML parse on every page load.
    const fav = e.target.closest('.fav-poster-btn');
    if (fav) { toggleFavMovie(fav); return; }
    const hide = e.target.closest('.hide-btn');
    if (hide) { hideFilm(hide); return; }
  });

  // On boot: paint the existing favourite state onto every star in the
  // freshly-rendered DOM. Runs once before applyFilters() so the filter on
  // /ulubione sees the right .is-fav classes.
  function paintFavourites() {
    const favMovies     = new Set(getFavMovies());
    const favScreenings = new Set(getFavScreenings());
    document.querySelectorAll('.col[data-title]').forEach(col => {
      const btn = col.querySelector('.fav-poster-btn');
      if (btn) btn.classList.toggle('is-fav', favMovies.has(col.dataset.title));
    });
    document.querySelectorAll('.badge-time').forEach(badge => {
      const star = badge.querySelector('.fav-star');
      if (star) star.classList.toggle('is-fav', favScreenings.has(badgeScreeningId(badge)));
    });
  }

  // ── End-of-row pre-wrap for fav-star hover ───────────────────────────────
  //
  // Pills are compact at rest; the star adds width on hover. Without this,
  // the last pill on a wrap-row would expand under the cursor and jump to
  // the next line. We scan each cinema-group, find the rightmost pill on
  // each row, and if its remaining row-space is less than the star width
  // we add `.needs-star-room` (CSS adds the equivalent padding-right). The
  // padding pushes the pill onto the next line BEFORE the user hovers —
  // when they hover, the star slides into the now-existing slack and the
  // pill stays put.
  //
  // Iterates up to 3 passes: adding the class on one pill can shift row
  // membership and make a previously mid-row pill the new end-of-row, so
  // a second / third pass picks up cascade cases.
  const _STAR_WIDTH_PX = 18;
  // Hover doesn't fire on touch devices — the hover-expand CSS is gated
  // by `@media (hover: hover)`, so the pre-wrap calculation does nothing
  // useful there. Skip the entire reflow on touch. Caching the
  // matchMedia result at boot saves a media-query lookup per call; the
  // value never changes for the lifetime of a tab (devices don't grow
  // hover support mid-session). On desktop where it does matter, the
  // reflow runs as before.
  const _hasHover = window.matchMedia && window.matchMedia('(hover: hover)').matches;
  function reflowFavStarSpacing() {
    if (!_hasHover) return;
    document.querySelectorAll('.cinema-group').forEach(group => {
      // Cheap ancestor-visibility short-circuit. One layout read per
      // group, vs N per-badge `offsetParent` + `getComputedStyle` reads
      // (each of which would have forced a synchronous style/layout
      // recompute). A group inside a hidden section/col has rect.width = 0.
      const groupRect = group.getBoundingClientRect();
      if (groupRect.width === 0) return;
      // `style.display` is set by `applyFilters → setVisible` and reads
      // from the cached attribute — no layout flush. The earlier
      // `b.classList.contains('hidden')` check was dead (no code writes
      // a `.hidden` class to badges anywhere in the app).
      const badges = [...group.querySelectorAll('.badge-time')].filter(b =>
        b.style.display !== 'none'
      );
      if (!badges.length) return;
      badges.forEach(b => b.classList.remove('needs-star-room'));
      for (let pass = 0; pass < 3; pass++) {
        let changed = false;
        // `.needs-star-room` only widens an end-of-row badge by 18 px of
        // padding-right — the group's outer width is unchanged across
        // passes, so the rect we already read is good for every pass.
        const containerRight = groupRect.right;
        for (let i = 0; i < badges.length; i++) {
          const b = badges[i];
          // Favourited pills already have the star permanently visible, so
          // their full width is already in the wrap calc — skip.
          const star = b.querySelector('.fav-star');
          if (star && star.classList.contains('is-fav')) continue;
          const next = badges[i + 1];
          const rect = b.getBoundingClientRect();
          const isEndOfRow = !next || next.getBoundingClientRect().top > rect.top;
          if (!isEndOfRow) continue;
          if (containerRight - rect.right >= _STAR_WIDTH_PX) continue;
          // Push the would-jump pill onto its own row by adding the margin
          // to the PREVIOUS sibling on the same row — that way the 18 px
          // hangs off the right edge of the old row (past `containerRight`,
          // so invisible) and the would-jump pill drops cleanly to the next
          // row with no leading gap. If there's no previous same-row pill
          // (this one is alone on its row), we can't prevent the jump
          // without visible side-effects — accept it for the degenerate
          // case.
          const prev = badges[i - 1];
          if (prev && prev.getBoundingClientRect().top === rect.top
              && !prev.classList.contains('needs-star-room')) {
            prev.classList.add('needs-star-room');
            changed = true;
          }
        }
        if (!changed) break;
      }
    });
  }

  // Debounce a reflow call. Used by applyFilters() and the toggle handlers
  // (every filter / star click can change which pill is end-of-row); also
  // by the resize listener. Same setTimeout id for both paths.
  let _reflowDebounce = 0;
  function scheduleReflow(delayMs) {
    clearTimeout(_reflowDebounce);
    _reflowDebounce = setTimeout(reflowFavStarSpacing, delayMs || 16);
  }
  window.addEventListener('resize', () => scheduleReflow(120));


  // ── Showings truncation ─────────────────────────────────────────────────
  //
  // After each filter pass, caps visible showings per card at ~20 visual
  // rows. Hides overflow at cinema-group boundaries and shows a
  // "… +N seansów" link to the /film page. Mirrors the iOS app's collapse.
  //
  // Called from applyFilters() in repertoire + kina after visibility has
  // been set on badges / groups. Walks the already-computed visibility —
  // no extra DOM measurement. The /film page has no applyFilters and
  // doesn't call this, so it renders everything.

  const _MAX_SHOWINGS_ROWS = 20;
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
    document.querySelectorAll('.col[data-title]').forEach(col =>
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
      label.appendChild(document.createTextNode(' ' + cinema));
      list.appendChild(label);
    });
    syncAllCheckbox();
  }

  function syncAllCheckbox() {
    const allCb = document.getElementById('cinema-all');
    if (!allCb) return;  // /kina has no Wszystkie-kina checkbox.
    const disabled = getDisabledCinemas();
    allCb.checked = disabled.length === 0;
    allCb.indeterminate = disabled.length > 0 && disabled.length < ALL_CINEMAS.length;
  }

  // Cinema picker now lives inside the Filtry dropdown; no standalone
  // open/close handler. `buildCinemaPanel` populates the same `#cinema-list`
  // element (just re-parented into `#format-panel`), so no changes needed
  // there. The cinema-count summary surfaces in `updateFormatBtn` instead.

  function toggleAllCinemas(checked) {
    setDisabledCinemas(checked ? [] : ALL_CINEMAS.slice());
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

  // Desktop hover
  document.addEventListener('mouseover', e => {
    const b = e.target.closest('.badge-time[data-room]');
    if (b) showRoomTooltip(b); else hideRoomTooltip();
  });

  // Mobile long-press
  let _roomTimer = null;

  document.addEventListener('touchstart', e => {
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

  // ── Global click: close panels ────────────────────────────────────────────
  //
  // Every dropdown trigger calls `event.stopPropagation()` on open, and
  // the panel wrappers do the same on their own click so inside-clicks
  // (radio buttons, cinema toggles, etc.) don't close them. That leaves
  // outside-clicks free to bubble to `document` — which calls
  // `closeOtherPanels(null)` to hide every `.dropdown-panel` in one go.
  //
  // Used to also reach for `#hidden-panel` directly; that element is
  // gone (the hidden-films UI is now a centred modal) and the bare
  // `getElementById(...).style` throw aborted the handler before it
  // could hide the format-panel, which is exactly the "click outside
  // Filtry doesn't close it" regression the user reported. Routing
  // through `closeOtherPanels` keeps the close path robust against
  // future additions/removals of dropdowns.
  document.addEventListener('click', () => {
    closeOtherPanels(null);
    closeAuthMenu();
  });

  // ── Counter ───────────────────────────────────────────────────────────────

  function updateCounter(visibleCount, showtimeCount) {
    const noFilms = document.getElementById('no-films');
    const counter = document.getElementById('film-counter');
    if (visibleCount === 0) {
      // On /ulubione with no favourites stored at all, point the user at
      // the action instead of saying the catalogue is empty.
      const onFavPage   = typeof IS_FAVOURITES_PAGE !== 'undefined' && IS_FAVOURITES_PAGE;
      const noFavsAtAll = onFavPage && getFavMovies().length === 0 && getFavScreenings().length === 0;
      noFilms.textContent = noFavsAtAll
        ? 'Brak ulubionych. Dodaj ulubione filmy lub seanse na innych stronach.'
        : 'Brak repertuaru.';
      noFilms.style.display = '';
      counter.style.display = 'none';
    } else {
      noFilms.style.display = 'none';
      counter.textContent = visibleCount + ' tytuł(ów) · ' + showtimeCount + ' seansów';
      counter.style.display = '';
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
    if (next >= 0 && next < sel.options.length) { sel.selectedIndex = next; applyFilters(); }
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
        favouriteMovies:     getFavMovies(),
        favouriteScreenings: getFavScreenings(),
        hiddenFilms:         getHidden(),
        disabledCinemas:     getDisabledCinemas()
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

      // Write through the underlying _lsSet, NOT via setFavMovies — the
      // wrapped setters would each fire their own scheduleServerSync(),
      // racing 4 PUTs at boot. After the merge we issue one explicit
      // pushStateToServer() at the end.
      _lsSet('favouriteMovies',     merge(getFavMovies(),        remote.favouriteMovies));
      _lsSet('favouriteScreenings', merge(getFavScreenings(),    remote.favouriteScreenings));
      _lsSet('hiddenFilms',         merge(getHidden(),           remote.hiddenFilms));
      _lsSet('disabledCinemas',     merge(getDisabledCinemas(),  remote.disabledCinemas));

      pushStateToServer();
      paintFavourites();  // repaint with the unioned state
      applyFilters();     // hidden / disabled-cinemas may have grown
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

  document.addEventListener('DOMContentLoaded', () => {
    buildIndex();
    populateDayOptions();
    squareDateNavBtns();
    updateNavbar();
    // Cinema picker lives in the Filtry dropdown now — populate the list
    // at boot so the first open of Filtry has the checkboxes ready.
    buildCinemaPanel();
    updateFormatBtn();
    paintFavourites();
    applyFilters();
    bootMergeFromServer();
  });
