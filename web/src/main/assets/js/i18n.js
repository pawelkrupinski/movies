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
  for (var i = 0; i < attrEls.length; i++) {
    var el = attrEls[i];
    var attrArgs = [el.getAttribute('data-i18n-arg0'), el.getAttribute('data-i18n-arg1')]
      .filter(function(a) { return a !== null; });
    el.getAttribute('data-i18n-attr').split(';').forEach(function(pair) {
      var sep = pair.indexOf(':');
      if (sep >= 0) el.setAttribute(pair.slice(0, sep), t(pair.slice(sep + 1), attrArgs));
    });
  }
  document.documentElement.lang = code;
  var select = document.getElementById('language-select');
  if (select) select.value = code;
}
window.applyLanguage = applyLanguage;

// Language picker — an in-place swap, no navigation, no `/lang/:code` round
// trip. Persists the pick so it survives future page loads, on every page,
// not just the one it was made on (applied by the boot block below).
function onLanguageChange(code) {
  if (!code) return;
  applyLanguage(code);
  try { localStorage.setItem('kinowo_lang', code); } catch (e) {}
}
window.onLanguageChange = onLanguageChange;

// Boot: apply a stored explicit pick, else sniff the browser's own language
// list against what this deployment ships packs for. This is a top-level
// statement, so it runs as soon as this `defer`red script executes — on
// repertoire.scala.html that's before `bootView` removes its `grid-cloak`,
// so a returning visitor's picked language is already applied before the
// grid becomes visible; other pages get it applied before the deferred
// script yields, i.e. before the browser's next paint.
//
// `window.KINOWO_NO_AUTO_LANG` (set only by `landing.scala.html`'s apex
// branch) skips this entirely: the apex is brand chrome that speaks for the
// whole brand rather than for a visitor, so it never re-renders in a stored
// pick — `t()`/`applyLanguage` stay fully usable, just never auto-invoked.
(function bootLanguage() {
  if (window.KINOWO_NO_AUTO_LANG) return;
  var stored = null;
  try { stored = localStorage.getItem('kinowo_lang'); } catch (e) {}
  if (stored) {
    if (I18N_PACKS[stored] && stored !== currentLang) applyLanguage(stored);
    return;
  }
  var languages = navigator.languages || [navigator.language || ''];
  for (var i = 0; i < languages.length; i++) {
    var code = String(languages[i]).slice(0, 2).toLowerCase();
    if (I18N_PACKS[code]) {
      if (code !== currentLang) applyLanguage(code);
      return;
    }
  }
})();
