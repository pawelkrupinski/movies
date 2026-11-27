// The page's one mutable state: every value the shared scripts keep between events (timers, the day in view, the
// search's origin, in-flight promises) lives in this object, built once by `createPageState()` and read and written
// as `pageState.<field>` by the module that owns the field. No module holds a `let` of its own, so what the page
// remembers is in one place a test can read and the composition root (`boot.js`) can see.

/* exported createPageState, pageState */

// What `auth.js` keeps between events.
function initialAuthState() {
  return {
    // Ask once per document, and hand every caller the same promise so the boot
    // order is "hydrate, then everything that depends on knowing".
    authHydration: null,
    // Whether `/api/me` ANSWERED — signed in (200) or signed out (401) — rather
    // than failing (offline, a 5xx). A page that could not ask renders signed
    // out, the safe way to be wrong about what to SHOW; it must not also act as
    // if the account were gone and drop what this device still owes it (see
    // `bootMergeFromServer`).
    authAnswered: false,
    nagDismissTimer: 0,
  };
}

// What `day-carousel.js` keeps between events.
function initialDayCarouselState() {
  return {
    // The day the centre grid is currently rendering. Tracked separately from the
    // `#date-filter` value because the carousel moves the dropdown to the target
    // up front (at slide start) while the centre grid keeps showing the old day
    // until the slide commits — `animateToDay` derives the slide direction from
    // THIS, not from the dropdown's already-moved index.
    appliedDay: null,
    // The day at the HEAD of the user's intent: the day the latest key-press /
    // pick is heading toward, which can sit ahead of `appliedDay` while a slide
    // is still in flight (or while follow-on steps are queued). A keyboard step
    // advances from THIS, so a second arrow press stacked on an unfinished slide
    // moves a further day instead of recomputing the same one. Resync'd to
    // `appliedDay` whenever the carousel finally settles with nothing queued.
    headDay: null,
    animating: false,   // guards re-entrancy while a commit animation runs
    queuedDay: null,   // a day-step requested mid-slide → run as a follow-on slide on commit
    swipeHintTimer: null,
  };
}

// What `day-swipe.js` keeps between events.
function initialDaySwipeState() {
  return {
    drag: null,   // the finger drag in progress: where it began, its axis once locked, its speed,
  };
}

// What `film-cards.js` keeps between events.
function initialFilmCardsState() {
  return {
    roomTimer: null,
  };
}

// What `filters.js` keeps between events.
function initialFiltersState() {
  return {
    filterDebounce: 0,
    otherNarrowings: [],
    panelBuilt: {},
  };
}

// What `filter-url.js` keeps between events.
function initialFilterUrlState() {
  return {
    rememberedElements: null,
    // The belt for an entry with no recorded state (one written before this
    // shipped): re-filter at `pageshow` when the engine's restore has moved the
    // controls away from the pass that drew what is on screen. Comparing, rather
    // than re-running unconditionally, keeps an ordinary load to its single pass.
    appliedSignature: null,
  };
}

// What `hidden-films-sync.js` keeps between events.
function initialHiddenFilmsSyncState() {
  return {
    // The tail of each country's write chain — see `_writeHiddenFilms`.
    hiddenFilmsWrites: {},
    // How many writes each country has been asked for on this page — how a
    // reconcile tells that an edit was made while its fetch was out.
    hiddenFilmsEdits: {},
  };
}

// What `language-sync.js` keeps between events.
function initialLanguageSyncState() {
  return {
    serverSyncTimer: 0,
    languagePush: Promise.resolve(),
  };
}

// What `search-day.js` keeps between events.
function initialSearchDayState() {
  return {
    searchOrigin: null,   // the day the listing was on when the search began; null: no search
    searchDayPicked: false,   // a day was picked while the search ran
    searchMovingDay: false,   // the search itself is moving the day, which is no pick,
  };
}

// What `search.js` keeps between events.
function initialSearchState() {
  return {
    searchBaseViewport: null,
    dismissSearchOnClick: false,
  };
}

// What `dates.js` keeps between events.
function initialDatesState() {
  return {
    cachedDay: null,
    cachedToday: undefined,
    cachedTomorrow: undefined,
    cachedIn7Days: undefined,
  };
}

function createPageState() {
  return {
    ...initialAuthState(),
    ...initialDayCarouselState(),
    ...initialDaySwipeState(),
    ...initialFilmCardsState(),
    ...initialFiltersState(),
    ...initialFilterUrlState(),
    ...initialHiddenFilmsSyncState(),
    ...initialLanguageSyncState(),
    ...initialSearchDayState(),
    ...initialSearchState(),
    ...initialDatesState(),
  };
}

const pageState = createPageState();
