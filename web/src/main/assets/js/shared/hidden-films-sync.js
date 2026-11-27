// Keeping a signed-in visitor's hidden films in step with the account, per country: each write,
// the ones still owed, and the reconcile at boot.

/* global pageState */  // page-state.js
/* global currentCountryCode, mountPrefix */  // city.js
/* global updateFormatButton */  // filters.js
/* global getHidden, setHidden */  // hidden-films-store.js
/* global isLoggedIn, sessionUnconfirmed, whoIsLookingIsUnknown */  // auth.js
/* global forgetPendingLanguage */  // language-sync.js
/* exported bootMergeFromServer, clearHiddenFilmsOnServer, hideFilmOnServer, unhideFilmOnServer */

// ── Server sync for logged-in users ──────────────────────────────────────
//
// HIDDEN FILMS ONLY — disabledCinemas stopped being a server-synced field
// (it's device-local now, see `setDisabledCinemas`) and this module never
// touches `/api/me/state` at all (neither read nor write) — that endpoint
// now carries only the language pick (`language-sync.js`).
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

// Whether a hidden-films write was refused for good: only the controller's own
// refusals — 400 (over-long title, unknown country) and 413 (full bucket).
// Anything else may land if sent again; a 403 in particular is as likely a
// Cloudflare challenge in front of the app as anything the app said. Same
// rule as both apps' `HiddenFilmsWriteRefused`.
function _hiddenFilmsWriteRefused(status) { return status === 400 || status === 413; }

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

function _editCount(country) { return pageState.hiddenFilmsEdits[country] || 0; }

// The one request shape every hiddenFilms write shares. A write that never
// landed (offline, 401, 5xx) leaves localStorage ahead of the server, so the
// cached validators stop describing it — forget them, and the next
// reconcile takes the server's answer instead of 304-ing onto the drift.
function _writeHiddenFilms(method, country, title) {
  country = country || currentCountryCode();
  pageState.hiddenFilmsEdits[country] = _editCount(country) + 1;
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
  const previous = pageState.hiddenFilmsWrites[country] || Promise.resolve();
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
  pageState.hiddenFilmsWrites[country] = sent;
  return sent;
}

function hideFilmOnServer(title, country)   { _writeHiddenFilms('PUT', country, title); }
function unhideFilmOnServer(title, country) { _writeHiddenFilms('DELETE', country, title); }
function clearHiddenFilmsOnServer(country)  { _writeHiddenFilms('DELETE', country); }

async function bootMergeFromServer(attempt = 0) {
  const country = currentCountryCode();
  const edits   = _editCount(country);
  // Signed out as far as this page can tell, but only because `/api/me`
  // could not be asked: nothing here is known to be over, so nothing is
  // forgotten either.
  if (whoIsLookingIsUnknown()) return;
  if (!isLoggedIn()) { _forgetAccountSyncState(); return; }
  try {
    const firstSync = localStorage.getItem(_hiddenFilmsSyncedKey(country)) !== '1';
    const resp = await fetch(_hiddenFilmsUrl(country), { headers: _hiddenFilmsReadHeaders(country, firstSync) });
    if (resp.status === 304) return; // proven unchanged — localStorage is already current
    if (!resp.ok) return;
    const remote = await resp.json();
    // An edit made while the fetch was out is in the local list but maybe not
    // in this answer, which replacing the list with would undo on screen. Ask
    // again once the writes queued meanwhile have gone.
    if (_editCount(country) !== edits) return _mergeAgainAfterWrites(country, attempt);
    _storeHiddenFilmsValidators(country, resp);
    _mergeServerHiddenFilms(country, remote.hiddenFilms || [], firstSync);
    applyFilters();
    updateFormatButton();   // the merged hidden set may light or clear the funnel
  } catch (e) { /* network blew up — localStorage is still usable */ }
}

// At most three times: a visitor cannot keep editing faster than a round trip for long.
async function _mergeAgainAfterWrites(country, attempt) {
  if (attempt >= 3) return;
  await (pageState.hiddenFilmsWrites[country] || Promise.resolve());
  return bootMergeFromServer(attempt + 1);
}

// Anonymous (incl. just-logged-out): re-arm migration for EVERY country
// so the next login carries this device's current local picks up
// exactly once, and drop writes owed to the account just left.
function _forgetAccountSyncState() {
  try {
    Object.keys(localStorage)
      .filter(k => k.indexOf(_hiddenFilmsSyncedKey('')) === 0 || k.indexOf(_hiddenFilmsPendingKey('')) === 0)
      .forEach(k => localStorage.removeItem(k));
    forgetPendingLanguage();
  } catch {}
}

// After the first sync, the server's list is asked for only if it changed since the last answer.
function _hiddenFilmsReadHeaders(country, firstSync) {
  const headers = { 'Accept': 'application/json' };
  if (firstSync) return headers;
  try {
    const etag = localStorage.getItem(_hiddenFilmsEtagKey(country));
    const lastModified = localStorage.getItem(_hiddenFilmsLastModifiedKey(country));
    if (etag) headers['If-None-Match'] = etag;
    else if (lastModified) headers['If-Modified-Since'] = lastModified;
  } catch {}
  return headers;
}

// A first sync unions this country's local list in; after that the
// server's list replaces it (server authoritative, so removals
// propagate). Either way the writes this device still owes (see the
// comment at the top) are played over the result and sent again — in
// order, as every write is (see `_writeHiddenFilms`) — and only then
// does the union migrate up what the list holds that the server does
// not (there is no bulk write any more). A first sync with writes owed
// is a page that could not reach the server before: without them the
// union would bring back what they removed. `setHidden` is a pure
// local write, so nothing is sent back out from here but the above.
function _mergeServerHiddenFilms(country, serverList, firstSync) {
  const pending    = _pendingHiddenFilms(country);
  const list       = _withPending(firstSync ? [...new Set([...getHidden(country), ...serverList])] : serverList, pending).sort();
  setHidden(list, country);
  const localOnly  = firstSync ? list.filter(t => !serverList.includes(t)) : [];
  pending.forEach(([method, title]) => _writeHiddenFilms(method, country, title === null ? undefined : title));
  localOnly.forEach(title => hideFilmOnServer(title, country));
  try { localStorage.setItem(_hiddenFilmsSyncedKey(country), '1'); } catch {}
}
