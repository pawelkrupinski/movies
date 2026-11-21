// The films a visitor hid, as this device keeps them: one list per country, in localStorage.

/* global currentCountryCode */  // city.js
/* global readStoredJson, storeJson */  // local-store.js
/* exported getHidden, setHidden */

// Hidden films are kept PER COUNTRY (`hiddenFilms:<cc>`), as the server and
// the apps keep them: showtimes.cc serves /uk, /de, /us and /es from one
// origin, so one list there was every country's at once — a /uk hide was in
// the list the /de page filtered and counted by, and a sign-in on /de had to
// be told (by a marker saying which country the list mirrored) not to union
// /uk's titles into the /de account.
function _hiddenFilmsKey(country) { return 'hiddenFilms:' + country; }
// The single list (and its marker) earlier builds kept — and the legacy
// cookie `_migrate` (`local-store.js`) still lifts into — moved into the bucket of the
// country it belongs to: the one the marker names, else this page's. Lazy,
// on every read, so a list written under the old key at any point (a
// pre-upgrade tab, the cookie) lands in its bucket rather than being lost.
const LEGACY_HIDDEN_FILMS_KEY = 'hiddenFilms';
const LEGACY_HIDDEN_FILMS_COUNTRY_KEY = 'hiddenFilmsCountry';
function _settleLegacyHiddenFilms() {
  try {
    const legacy = readStoredJson(LEGACY_HIDDEN_FILMS_KEY);
    if (legacy === null) return;
    const marker = localStorage.getItem(LEGACY_HIDDEN_FILMS_COUNTRY_KEY);
    const owner  = marker && /^[a-z]{2}$/.test(marker) ? marker : currentCountryCode();
    const bucket = readStoredJson(_hiddenFilmsKey(owner)) || [];
    storeJson(_hiddenFilmsKey(owner), [...new Set([...bucket, ...legacy])]);
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
  return readStoredJson(_hiddenFilmsKey(country || currentCountryCode())) || [];
}
// Pure localStorage write. The server round-trip is the CALLER's job now —
// each caller already knows exactly which title it hid/unhid (or that it
// cleared everything), which is what the granular per-title API needs; see
// hideFilmOnServer/unhideFilmOnServer/clearHiddenFilmsOnServer (`hidden-films-sync.js`) and
// their call sites (hideFilm/restoreFilm/showAllFilms).
function setHidden(titles, country) {
  _settleLegacyHiddenFilms();
  storeJson(_hiddenFilmsKey(country || currentCountryCode()), titles);
}
