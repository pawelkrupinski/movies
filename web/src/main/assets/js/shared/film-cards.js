// A film card's own interactions: the card tap and its hide button, the room tooltip on a
// showtime, and the IMDb pill's app link on Android.

/* global CITY_BASE */  // city.js
/* global hideFilm */  // hidden-films.js

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
