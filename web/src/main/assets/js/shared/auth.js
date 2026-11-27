// Who is looking: the avatar menu and login modal, asking `/api/me`, signing out (here and in
// another tab), and the once-a-day nudge to sign in.

/* global mountPrefix */  // city.js
/* global wallClockNow */  // clock.js
/* global pageState */  // page-state.js
/* global readStoredFlag, storeFlag */  // local-store.js
/* global closeDialog, openDialog, toggleAuthMenu */  // overlays.js
/* exported closeAnonymousNag, closeLoginModal, hydrateAuth, isLoggedIn, maybeShowAnonymousNag,
   openLoginModal, sessionUnconfirmed, settleSignOut, whoIsLookingIsUnknown */

// ── Login modal ──────────────────────────────────────────────────────────
//
// The avatar dropdown opens and closes in `overlays.js`, with the other dropdowns.

function openLoginModal() {
  openDialog('login-dialog');
}
function closeLoginModal() {
  closeDialog('login-dialog');
}

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

// The last answer this device had, kept across pages: '1' while `/api/me`
// last said signed in. A page that cannot ask goes by it — edits made
// offline by someone who was signed in are owed to the account (see
// `_writeHiddenFilms`), and edits by someone who was not are nobody's.
const SIGNED_IN_KEY = 'signedIn';
function sessionUnconfirmed() {
  if (isLoggedIn() || pageState.authAnswered) return false;
  try { return localStorage.getItem(SIGNED_IN_KEY) === '1'; } catch { return false; }
}
// Signed out as far as this page can tell, but only because `/api/me` could not be asked.
function whoIsLookingIsUnknown() { return !isLoggedIn() && !pageState.authAnswered; }
function hydrateAuth() {
  if (pageState.authHydration) return pageState.authHydration;
  // Nothing to sign in to (a deployment with no OAuth secrets), or a page that
  // already carries the menu: either way there is nothing to ask.
  if (!HAS_OAUTH_PROVIDERS || isLoggedIn()) {
    pageState.authAnswered = true;
    return (pageState.authHydration = Promise.resolve());
  }
  pageState.authHydration = fetch(mountPrefix() + '/api/me', { credentials: 'same-origin' })
    .then(response => {
      pageState.authAnswered = response.ok || response.status === 401;
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
  return pageState.authHydration;
}

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
document.addEventListener('visibilitychange', verifySessionOnReturn);

// Delegated, because the shared scripts load after the navbar on some pages and
// before it on others, and a `submit` listener on the document catches both.
document.addEventListener('submit', event => {
  const form = event.target;
  if (form && form.classList && form.classList.contains('auth-logout-form')) beginSignOut();
});

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
  const lastAt = parseInt(readStoredFlag('lastAnonymousNagAt') || '0', 10);
  const dayMs  = 24 * 60 * 60 * 1000;
  if (wallClockNow() - lastAt < dayMs) return;
  storeFlag('lastAnonymousNagAt', wallClockNow().toString());
  showAnonymousNag();
}

function showAnonymousNag() {
  const toast = document.getElementById('anon-nag-toast');
  if (!toast) return;
  toast.classList.add('open');
  clearTimeout(pageState.nagDismissTimer);
  pageState.nagDismissTimer = setTimeout(closeAnonymousNag, 8000);
}
function closeAnonymousNag() {
  const toast = document.getElementById('anon-nag-toast');
  if (toast) toast.classList.remove('open');
}
