// Keeping a signed-in visitor's language pick in step with the account: the debounced push, the
// unload flush, and the reconcile at boot.

/* global pageState */  // page-state.js
/* global mountPrefix */  // city.js
/* global isLoggedIn, sessionUnconfirmed */  // auth.js
/* exported forgetPendingLanguage, reconcileLanguage, scheduleServerSync */

// `language` (the picked UI language, `kinowo_lang` — see `i18n.js`) rides
// the LEGACY `/api/me/state` document instead — there's no granular
// endpoint for a single scalar pick, only for the two sets it used to
// share that document with. It skips the hidden films' per-country
// union/authoritative dance (`hidden-films-sync.js`) entirely (see
// `reconcileLanguage` for why) but keeps the debounced-push machinery below, narrowed to a
// `language`-only body now that hiddenFilms/disabledCinemas both left it.
// A language pick the account has not confirmed yet — its debounced push is
// still waiting, it failed, or the page was offline. Persisted so the next
// signed-in load pushes it instead of adopting the account's OLDER pick over
// it (see `reconcileLanguage`); forgotten on a confirmed sign-out, with the
// hidden-films writes owed. Mirrors the apps' `pendingLanguagePush`.
const PENDING_LANGUAGE_KEY = 'kinowo_lang_pending';
function _pendingLanguage() { try { return localStorage.getItem(PENDING_LANGUAGE_KEY); } catch { return null; } }
// The account just left (a confirmed sign-out) is owed no pick any more.
function forgetPendingLanguage() { _setPendingLanguage(null); }
function _setPendingLanguage(lang) {
  try { if (lang) localStorage.setItem(PENDING_LANGUAGE_KEY, lang); else localStorage.removeItem(PENDING_LANGUAGE_KEY); } catch {}
}

// Whether a language push was refused for good: only a 400, which is what
// `UserStateController.put` answers for a language it does not know. Same
// reasoning as `_hiddenFilmsWriteRefused`, and the same rule as both apps'
// `LanguagePushRefused`.
function _languagePickRefused(status) { return status === 400; }

function scheduleServerSync() {
  if (!isLoggedIn() && !sessionUnconfirmed()) return;
  _setPendingLanguage((() => { try { return localStorage.getItem('kinowo_lang'); } catch { return null; } })());
  if (!isLoggedIn()) return; // could not confirm the session: the next signed-in load pushes it
  clearTimeout(pageState.serverSyncTimer);
  // 400ms — long enough that a rapid run of picker clicks folds into one PUT.
  pageState.serverSyncTimer = setTimeout(pushStateToServer, 400);
}

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
function pushStateToServer(opts) {
  pageState.serverSyncTimer = 0;
  if (opts && opts.keepalive) return _sendLanguage(true);
  pageState.languagePush = pageState.languagePush.then(() => _sendLanguage(false));
  return pageState.languagePush;
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
  if (!isLoggedIn() || !pageState.serverSyncTimer) return;
  clearTimeout(pageState.serverSyncTimer);
  pushStateToServer({ keepalive: true });
}
window.addEventListener('pagehide', flushServerSync);
document.addEventListener('visibilitychange', () => {
  if (document.visibilityState === 'hidden') flushServerSync();
});

// `language` reconciles independently of `bootMergeFromServer` (`hidden-films-sync.js`) —
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
async function reconcileLanguage({ adoptOnly = false } = {}) {
  if (!isLoggedIn()) return;
  // A pick the account has not confirmed is newer than anything it holds:
  // push it rather than let the account's value overwrite it.
  if (_pendingLanguage()) return pushStateToServer();
  try {
    const resp = await fetch(mountPrefix() + '/api/me/state', { headers: { 'Accept': 'application/json' } });
    if (!resp.ok) return;
    const remote = await resp.json();
    if (_pendingLanguage()) return pushStateToServer(); // picked while the fetch was out
    _settleLanguage(remote.language, adoptOnly);
  } catch (e) { /* network blew up — localStorage is still usable */ }
}

// The account's pick wins whenever it has one; otherwise this device's own
// pick, if any, becomes the account's (unless only adopting).
function _settleLanguage(accountLanguage, adoptOnly) {
  try {
    const localLang = localStorage.getItem('kinowo_lang');
    if (accountLanguage && accountLanguage !== localLang) {
      localStorage.setItem('kinowo_lang', accountLanguage);
      if (typeof window.applyLanguage === 'function') window.applyLanguage(accountLanguage);
    } else if (!accountLanguage && localLang && !adoptOnly) {
      pushStateToServer();
    }
  } catch { /* storage blocked — nothing to settle against */ }
}
