// Booting the page once the document is parsed: index and filter the grid, then ask who is looking.

/* global rememberCity */  // city.js
/* global resetSubmenuPanels, updateFormatBtn */  // filters.js
/* global refreshHiddenFilmsControls */  // hidden-films.js
/* global buildCinemaPanel, maybeShowAreaPicker */  // cinemas.js
/* global hydrateAuth, settleSignOut */  // auth.js
/* global syncDayPills */  // dates.js
/* global applyFiltersFromURL, noteFiltersApplied */  // filter-url.js
/* global bootMergeFromServer */  // hidden-films-sync.js
/* global reconcileLanguage */  // language-sync.js
/* global maybeShowSwipeHint, rememberDisplayedDay */  // day-carousel.js

// Client-side language switching (`t`/`applyLanguage`/`onLanguageChange`,
// the `I18N_PACKS` pack, and the boot-time apply) lives in the standalone
// `i18n.js`, loaded before every shared script — see that file's own doc
// comment for why it is not one of them (landing.scala.html needs it too, and
// loads none of these).

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
  // displayed day, and sync the pills to the URL-applied day (the eager
  // DOMContentLoaded sync ran before `applyFiltersFromURL`).
  rememberDisplayedDay();
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
  refreshHiddenFilmsControls();
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
