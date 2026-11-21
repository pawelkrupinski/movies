// The listing's dates: date arithmetic and labels (re-worded on a language switch), the day the
// page is on, and the day pills.

/* global onDateSelect */  // day-carousel.js
/* exported dateBounds, formatDateLabelShort, highlightDayPill, pageToday, pickDay, refreshDateLabels,
   syncDayPills */

// ── Date helpers ──────────────────────────────────────────────────────────

function isoAddDays(isoDate, n) {
  const [y, m, d] = isoDate.split('-').map(Number);
  return new Date(y, m - 1, d + n).toLocaleDateString('sv');
}

// Weekday (Sun-first, matching Date.getDay()) + month labels, injected per
// deployment via KINOWO_LOCALE (Polish keeps the genitive month forms).
// References to KINOWO_LOCALE's OWN arrays — `applyLanguage` (i18n.js)
// splices a picked language's words into these same arrays in place
// (never reassigns `KINOWO_LOCALE.day2`), so DAY2/MONTHS stay live through
// a language switch without this module needing its own applyLanguage hook.
const DAY2   = KINOWO_LOCALE.day2;
const MONTHS = KINOWO_LOCALE.months;

// The long date-header label ("Thursday 4 June", "Czwartek 4 czerwca") —
// mirrors `DateFormatter.format` (Scala) so the client-side re-render on a
// language switch matches what the server would have rendered in that
// language. `KINOWO_LOCALE.daysFull` is Monday-first, matching
// `LocalDate.getDayOfWeek.getValue - 1`; `getDay()` below is Sunday-first,
// hence the `+6 % 7` rebase. "This year" is the PAGE's day (`pageToday()`,
// the city clock the server labelled from), never the device's.
function formatDateLabel(isoDate) {
  const [year, month, day] = isoDate.split('-').map(Number);
  const dayOfWeek       = new Date(year, month - 1, day).getDay();
  const mondayFirstIdx  = (dayOfWeek + 6) % 7;
  const currentYear     = Number(pageToday().slice(0, 4));
  const yearSuffix      = year === currentYear ? '' : ' ' + year;
  return KINOWO_LOCALE.daysFull[mondayFirstIdx] + ' ' + day + ' ' + MONTHS[month - 1] + yearSuffix;
}

// The short form the custom-date `#date-filter` option uses ("Czw 21 maja") —
// mirrors the construction below that used to be inlined at its one call site.
function formatDateLabelShort(isoDate) {
  const [year, month, day] = isoDate.split('-').map(Number);
  const dayOfWeek = new Date(year, month - 1, day).getDay();
  return DAY2[dayOfWeek] + ' ' + day + ' ' + MONTHS[month - 1];
}

// Re-renders every already-drawn date label from its `data-date` ISO
// source rather than trusting whatever text is currently on screen —
// called after `applyLanguage` (i18n.js) splices in a new language's
// day/month arrays above, so the listing's date headers (baked in the
// deployment's default language at server-render time) actually follow the
// visitor's picked language instead of staying stuck in the old one.
function refreshDateLabels() {
  document.querySelectorAll('.date-group[data-date]').forEach(group => {
    const label = group.querySelector('.date-label');
    if (label) label.textContent = formatDateLabel(group.dataset.date);
  });
  const customOption = document.querySelector('#date-filter option[data-date]');
  if (customOption) customOption.textContent = formatDateLabelShort(customOption.dataset.date);
}

let _cachedDay = null, _cachedToday, _cachedTomorrow, _cachedIn7Days;

// The calendar day the PAGE is on. Two things it deliberately is not:
//
//  - the visitor's timezone. Showtimes, their expiry stamps and the midnight
//    rollover are all resolved in `city.zoneId` server-side, so the day filter
//    reads the same city clock (`window.CITY_TIMEZONE`) — a Londoner at 23:30
//    is still on today's listings, though Warsaw has already turned over.
//  - the visitor's wall clock. `showtimeNow()` is the server's render instant
//    carried forward by elapsed browser time, which is what the expiry prune
//    counts on; a device clock set days out can't drag the day filter away
//    from the listings the page actually shipped with. Only the repertoire
//    view defines it — elsewhere (/movie, /browse) there is no expiry to
//    stay in step with, so the browser clock is answer enough.
//
// `window.KINOWO_PINNED_TODAY` is set ONLY in page-test renders (fixture data
// carries absolute past dates; a live clock would age the day filters out).
function pageToday() {
  if (window.KINOWO_PINNED_TODAY) return window.KINOWO_PINNED_TODAY;
  const nowMs = typeof window.showtimeNow === 'function' ? window.showtimeNow() : Date.now();
  return new Date(nowMs).toLocaleDateString('sv', { timeZone: window.CITY_TIMEZONE });
}

function dateBounds() {
  const today = pageToday();
  if (today !== _cachedDay) {
    _cachedDay      = today;
    _cachedToday    = today;
    _cachedTomorrow = isoAddDays(today, 1);
    _cachedIn7Days  = isoAddDays(today, 6);
  }
  return { today: _cachedToday, tomorrow: _cachedTomorrow, in7Days: _cachedIn7Days };
}

// Reflect the hidden `#date-filter` value onto the visible day pills: set
// `.active` + `aria-selected` on the matching pill, clear the rest. Called on
// boot and after every committed day change (via `onDateChange`), plus
// eagerly on a pill tap so the highlight moves before the slide commits.
// Set `.active` + `aria-selected` on the pill matching `value`, clear the rest.
function highlightDayPill(value) {
  document.querySelectorAll('#day-pills .day-pill').forEach(p => {
    const on = p.dataset.day === value;
    p.classList.toggle('active', on);
    p.setAttribute('aria-selected', on ? 'true' : 'false');
  });
}

function syncDayPills() {
  const sel = document.getElementById('date-filter');
  if (!sel) return;
  highlightDayPill(sel.value);
}

// A day pill was tapped — move the hidden select to that day and slide there
// exactly like the old dropdown pick (linear direction from the list order).
function pickDay(value) {
  const sel = document.getElementById('date-filter');
  if (!sel || sel.value === value) return;
  sel.value = value;
  syncDayPills();   // highlight immediately; the slide commits the grid
  onDateSelect();
}
