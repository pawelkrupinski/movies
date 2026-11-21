// Where this page sits: the deployment's mount point, the city (its cookie, switching to another,
// the base its film links hang off) and the country.

/* exported CITY_BASE, currentCountryCode, goToCityPicker, mountPrefix, rememberCity */

// Where THIS deployment is mounted: '' for a country that owns its domain
// (`kinowo.net/poznan/…`), '/uk' for one that shares the brand domain
// (`showtimes.cc/uk/kent/…`). Read off the current path — every page this file
// runs on is `{mount}/{city}/…` — rather than emitted as a server-rendered
// constant, so one asset URL serves every country and the browser parses it
// once. Falls back to the root if the path somehow doesn't name the city.
function mountPrefix() {
  var marker = '/' + CURRENT_CITY + '/';
  var at = window.location.pathname.indexOf(marker);
  return at < 0 ? '' : window.location.pathname.slice(0, at);
}

// City picker (Filtry → Miasto) changed — remember the choice so the bare
// `/` landing bounces here next time, then navigate to the chosen city's
// repertoire root. A full navigation (not a view-swap) because the whole
// corpus changes. No-op when the value is the current city.
// Remember the city so the bare `/` landing bounces here next time.
//
// THE CLIENT OWNS THIS COOKIE NOW. The listing used to arrive with a
// `Set-Cookie`, and Cloudflare bypasses the cache for any response carrying
// one — measured: the page went DYNAMIC -> BYPASS with this as the only
// thing left on it. Same name, path and lifetime as the header it replaces,
// so the landing bounce behaves exactly as before.
function rememberCity(slug) {
  var prefix = mountPrefix();
  document.cookie = 'city=' + slug + ';path=' + (prefix || '') + '/;max-age=' + (60 * 60 * 24 * 365);
}
window.rememberCity = rememberCity;

function onCityChange(slug) {
  if (!slug || slug === CURRENT_CITY) return;
  rememberCity(slug);
  window.location.href = mountPrefix() + '/' + slug + '/';
}
window.onCityChange = onCityChange;

// Filtry → Miasto row — navigates to the unified `/` picker
// (`landing.scala.html`) rather than opening an in-page modal (the retired
// `_cityPickerModal`): one picker for both "change city" and "change
// country" instead of a separate control for each. `?pick=city` so that
// picker asks rather than bouncing straight back to this same city.
function goToCityPicker() {
  window.location.href = mountPrefix() + '/?pick=city';
}
window.goToCityPicker = goToCityPicker;

function currentCountryCode() {
  return typeof CURRENT_COUNTRY !== 'undefined' ? CURRENT_COUNTRY : 'pl';
}

// Every page is under `{mount}/{city}/…`, where the mount point is empty for a
// country that owns its domain (`kinowo.net/poznan/`) and a country segment
// for one that shares the brand domain (`showtimes.cc/uk/kent/`). CURRENT_CITY
// is the global from `_sharedJsConfig`; the mount point is read off the
// CURRENT PATH rather than a second server-rendered constant, so this file
// stays identical for every deployment (one URL, one parse, one bytecode cache
// — the reason it was externalised at all).
const CITY_BASE = mountPrefix() + '/' + CURRENT_CITY;
