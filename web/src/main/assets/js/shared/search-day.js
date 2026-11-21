// The day a search moves the listing to and back from: when it began, whether a day was picked
// while it ran, and what the URL and a history entry say about it.

/* exported beginSearchAtBoot, beginSearchFrom, currentQuery, endSearch, noteDayChange, restoreSearchDay,
   searchDayState, searchIsRunning, searchOriginDay, whileSearchMovesDay, urlDay */

// A search looks through EVERY day: the first letter typed moves the listing to
// "All" (no slide) and remembers the day it was on; clearing the search goes back
// there -- unless a day was picked while it ran (a tap, a swipe, a key), which then
// stays. While the search shows every day, the days it brought in are drawn lighter
// (`outside-day`, decided by the filter pass from `searchOriginDay`).
//
// The search field (`search.js`) begins and ends a search; the carousel
// (`day-carousel.js`) reports every day change here, so neither has to know the other.
let _searchOrigin = null;      // the day the listing was on when the search began; null: no search
let _searchDayPicked = false;  // a day was picked while the search ran
let _searchMovingDay = false;  // the search itself is moving the day, which is no pick

function currentQuery() {
  return (document.getElementById('search-input')?.value || '').trim();
}

function searchIsRunning() { return _searchOrigin !== null; }

/** The day a running search began from, while nothing has been picked since; else null. */
function searchOriginDay() { return _searchOrigin !== null && !_searchDayPicked ? _searchOrigin : null; }

// A search begins on `day`, nothing picked yet.
function beginSearchFrom(day) {
  _searchOrigin = day;
  _searchDayPicked = false;
}

// The search was cleared: the day to go back to, or null when a day was picked while it ran.
function endSearch() {
  const back = _searchDayPicked ? null : _searchOrigin;
  _searchOrigin = null;
  _searchDayPicked = false;
  return back;
}

// Runs `moveDay`, a day change the search itself makes, which `noteDayChange` takes for no pick.
function whileSearchMovesDay(moveDay) {
  _searchMovingDay = true;
  try { moveDay(); } finally { _searchMovingDay = false; }
}

// The listing's day moved from `previousDay` to `nextDay`. By anything but the search itself,
// mid-search, that is a pick: it stays when the search is cleared. Not the first apply at boot
// (`previousDay` null: nothing was on screen to move from).
function noteDayChange(previousDay, nextDay) {
  if (_searchOrigin !== null && !_searchMovingDay && previousDay !== null && previousDay !== nextDay)
    _searchDayPicked = true;
}

// The running search's day, for the history entry: where clearing it would go. Null: no search.
function searchDayState() {
  return _searchOrigin === null ? null : { origin: _searchOrigin, picked: _searchDayPicked };
}

// Put back a search's day recorded by `searchDayState`; the listing it was on showed "All"
// unless a day was picked since.
function restoreSearchDay(searchDay) {
  if (!searchDay || typeof searchDay.origin !== 'string') return;
  _searchOrigin = searchDay.origin;
  _searchDayPicked = !!searchDay.picked;
  // The URL carried the searched-from day (`urlDay`); the listing was on "All".
  const sel = document.getElementById('date-filter');
  if (sel && !_searchDayPicked) sel.value = 'anytime';
}

// The day the URL carries: the searched-from day while the search itself is showing
// "All", so a reload (which starts with an empty search) and Back land on the day the
// listing was really on, not on the search's "All".
function urlDay(sel) { return searchOriginDay() ?? sel.value; }

// A page opened on a query with no remembered search (a shared `?q=` link): the
// search begins from the link's day, showing every day.
function beginSearchAtBoot() {
  const sel = document.getElementById('date-filter');
  if (!sel || !currentQuery() || _searchOrigin !== null) return;
  beginSearchFrom(sel.value);
  sel.value = 'anytime';
}
