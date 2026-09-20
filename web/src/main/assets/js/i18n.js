// ── Client-side language switching ───────────────────────────────────────
//
// The server always renders the deployment's fixed default language (no
// cookie/Accept-Language resolution any more) so every visitor gets
// byte-identical HTML for a given URL and the edge can cache it
// unconditionally — the whole class of cache/cookie-coupling bug the old
// `/lang/:code` round trip was prone to. Everything below applies the
// visitor's OWN pick client-side, against the language pack every page
// embeds (an `#i18n-packs` `<script type="application/json">` block,
// generated from `messages*` by `I18nPackGenerator` — see
// `controllers.I18nPacks`).
//
// A STANDALONE script (not folded into `shared.js`) because it has two
// independent consumers: `shared.js`-loading pages (repertoire, browse,
// film — where `_sharedJsConfig.scala.html` embeds `#i18n-packs`) AND
// `landing.scala.html`, a fully self-contained page with its own inline
// script that never loads `shared.js` at all. Load this BEFORE either.
//
// `data-i18n="key"` on an element swaps its text (or, with `data-i18n-html`,
// its innerHTML — only `login.nag`'s translator-authored `<strong>` markup
// needs that). `data-i18n-arg0`/`data-i18n-arg1` carry the already-resolved
// runtime values (e.g. a brand/city name) for `{0}`/`{1}` substitution.
// `data-i18n-attr="attr1:key1;attr2:key2"` swaps an attribute (aria-label,
// title, …) instead of the element's own text.
var I18N_PACKS = (function() {
  var el = document.getElementById('i18n-packs');
  try { return el ? JSON.parse(el.textContent) : {}; } catch (e) { return {}; }
})();
var DEFAULT_LANG = (typeof KINOWO_LOCALE !== 'undefined' && KINOWO_LOCALE.lang) || 'pl';
var currentLang  = DEFAULT_LANG;

// The client-side half of the `{0}`/`{1}` placeholders `messages.*` still
// carries — server-side rendering runs the same strings through Play's real
// `MessageFormat`; this only ever substitutes into the identical literal
// placeholders, so a plain split/join is enough.
function formatPack(value, args) {
  if (!args) return value;
  for (var i = 0; i < args.length; i++) {
    if (args[i] != null) value = value.split('{' + i + '}').join(args[i]);
  }
  return value;
}

// Looks up `key` in the current language's pack, falling back to the
// deployment default (never crash the page over a translation gap) and
// finally to the bare key.
function t(key, args) {
  var pack = I18N_PACKS[currentLang] || I18N_PACKS[DEFAULT_LANG] || {};
  var value = pack[key];
  return formatPack(value == null ? key : value, args);
}
window.t = t;

// Applies `code` to every `[data-i18n]`/`[data-i18n-attr]` element on the
// page, updates `<html lang>` and the picker's own value (where present —
// only pages with an in-page `<select id="language-select">` have one).
// Pure DOM mutation — no navigation, no request.
function applyLanguage(code) {
  if (!I18N_PACKS[code]) return;
  currentLang = code;
  var textEls = document.querySelectorAll('[data-i18n]');
  for (var i = 0; i < textEls.length; i++) {
    var el = textEls[i];
    var args = [el.getAttribute('data-i18n-arg0'), el.getAttribute('data-i18n-arg1')]
      .filter(function(a) { return a !== null; });
    var text = t(el.getAttribute('data-i18n'), args);
    if (el.hasAttribute('data-i18n-html')) el.innerHTML = text;
    else el.textContent = text;
  }
  var attrEls = document.querySelectorAll('[data-i18n-attr]');
  for (var j = 0; j < attrEls.length; j++) {
    var attrEl = attrEls[j];
    var attrArgs = [attrEl.getAttribute('data-i18n-arg0'), attrEl.getAttribute('data-i18n-arg1')]
      .filter(function(a) { return a !== null; });
    attrEl.getAttribute('data-i18n-attr').split(';').forEach(function(pair) {
      var sep = pair.indexOf(':');
      if (sep >= 0) attrEl.setAttribute(pair.slice(0, sep), t(pair.slice(sep + 1), attrArgs));
    });
  }
  // Date headers (`.date-label`) and the plural showtime noun are derived
  // from KINOWO_LOCALE, not `[data-i18n]` text — they're computed from the
  // raw date/count, not a translated string. Splice the picked language's
  // arrays IN PLACE (never reassign `KINOWO_LOCALE.day2 = …`) so `shared.js`'s
  // `DAY2`/`MONTHS` — captured once as references to these same arrays —
  // pick the change up without shared.js needing its own applyLanguage hook.
  var pageLocale = typeof KINOWO_LOCALE !== 'undefined' && KINOWO_LOCALE.locales && KINOWO_LOCALE.locales[code];
  if (pageLocale) {
    [['day2', pageLocale.day2], ['daysFull', pageLocale.daysFull], ['months', pageLocale.months]]
      .forEach(function(pair) {
        var arr = KINOWO_LOCALE[pair[0]];
        arr.length = 0;
        Array.prototype.push.apply(arr, pair[1]);
      });
    KINOWO_LOCALE.plural   = pageLocale.plural;
    KINOWO_LOCALE.showtime = pageLocale.showtime;
  }
  document.documentElement.lang = code;
  var select = document.getElementById('language-select');
  if (select) select.value = code;
  // Re-render whatever's already on screen using the arrays just updated
  // above — `shared.js` (repertoire/film pages) defines this; `landing.scala.html`
  // shows no dates, so it never defines it and this is a no-op there.
  if (typeof window.refreshDateLabels === 'function') window.refreshDateLabels();
  // Same shape, for the "… +N seansów"/"… +N showings" truncation link:
  // `_showtimeNoun` (shared.js) already reads `KINOWO_LOCALE.showtime` fresh
  // on every call, so it would render correctly on the NEXT filter pass
  // regardless — but nothing re-triggers a pass on a language switch by
  // itself, so an already-rendered link kept its old-language noun until an
  // unrelated filter change came along. `applyFilters` (repertoire's own
  // inline script) is the existing full recompute, safe to call repeatedly.
  // Only the repertoire listing defines it; `/movie` shows every showing
  // untruncated and has no such link.
  if (typeof window.applyFilters === 'function') window.applyFilters();
}
window.applyLanguage = applyLanguage;

// Language picker — an in-place swap, no navigation, no `/lang/:code` round
// trip. Persists the pick so it survives future page loads, on every page,
// not just the one it was made on (applied by the boot block below). For a
// logged-in user it also reaches the account (`shared.js`'s server sync,
// `/api/me/state`'s `language` field), so it's restored on any device they
// next sign into — anonymous visitors keep this as a device-local pick only.
function onLanguageChange(code) {
  if (!code) return;
  applyLanguage(code);
  try { localStorage.setItem('kinowo_lang', code); } catch (e) {}
  if (typeof window.scheduleServerSync === 'function') window.scheduleServerSync();
}
window.onLanguageChange = onLanguageChange;

// Boot: apply a stored explicit pick, if there is one. No browser-language
// inference — a visitor with no stored pick sees the deployment's own
// default (pl on kinowo.net, en on showtimes.cc/us, /uk, …), same as the
// server-rendered HTML they already got, regardless of their own browser's
// language settings.
//
// Applies it through `applyLanguage` TWICE — the exact same call an
// interactive picker change makes, not a parallel "boot" code path:
//
//  1. Immediately, as a top-level statement. This file loads early on every
//     page — deferred on repertoire.scala.html, but plain and early in
//     `<body>` (film.scala.html/browse.scala.html) or `<head>`
//     (landing.scala.html) elsewhere, so `t()`/`currentLang` are already
//     correct for an inline script further down the SAME page that calls
//     `t()` synchronously (e.g. film.scala.html's back-link rewrite — see
//     its own comment). On repertoire.scala.html this also lands before
//     `bootView` removes its `grid-cloak`, i.e. before the browser's next
//     paint. But the DOM may still be far from fully parsed at this point,
//     so `applyLanguage`'s `[data-i18n]` sweep can miss markup that hasn't
//     loaded yet — on film/browse/landing this used to mean a returning
//     visitor's stored pick never reached most of the page's translated
//     strings (or the date headers specifically — `refreshDateLabels`,
//     defined in `shared.js`, isn't even a function yet at this point).
//  2. Again on `DOMContentLoaded`, once the whole document — and
//     `shared.js`, wherever a page loads it — is guaranteed to have run.
//     This is what actually reaches the markup call 1 missed. It is not
//     conditional on `currentLang` having changed (call 1 already set that),
//     so on a page where call 1 already saw the full DOM
//     (repertoire.scala.html, both scripts deferred) this just repeats
//     identical, harmless work.
//
// `window.KINOWO_NO_AUTO_LANG` (set only by `landing.scala.html`'s apex
// branch) skips this entirely: the apex is brand chrome that speaks for the
// whole brand rather than for a visitor, so it never re-renders in a stored
// pick either — `t()`/`applyLanguage` stay fully usable, just never
// auto-invoked.
(function bootLanguage() {
  if (window.KINOWO_NO_AUTO_LANG) return;
  function applyStoredPick() {
    var stored = null;
    try { stored = localStorage.getItem('kinowo_lang'); } catch (e) {}
    if (stored && I18N_PACKS[stored]) applyLanguage(stored);
  }
  applyStoredPick();
  document.addEventListener('DOMContentLoaded', applyStoredPick);
})();
