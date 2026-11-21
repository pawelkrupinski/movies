// Reading and writing the device's own storage: JSON lists in localStorage, the legacy cookies they
// moved out of, and the plain flags the one-off hints keep.

/* exported _hintGet, _hintSet, _lsGet, _lsSet */

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

function _hintGet(k) { try { return localStorage.getItem(k); } catch (e) { return null; } }
function _hintSet(k, v) { try { localStorage.setItem(k, v); } catch (e) { /* private mode — skip */ } }
