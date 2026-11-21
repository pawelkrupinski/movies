// Folding a card's showings down to about ten rows after each filter pass.

/* global gridScope */  // filters.js
/* exported foldShowings, truncateAllShowings, undoTruncation */

// ── Showings truncation ─────────────────────────────────────────────────
//
// After each filter pass, caps visible showings per card at ~10 visual
// rows. Hides overflow at cinema-group boundaries and shows a
// "… +N seansów" link to the /movie page. Mirrors the iOS app's collapse.
//
// Called from applyFilters() in the repertoire view after visibility has
// been set on badges / groups. Walks the already-computed visibility —
// no extra DOM measurement. The /movie page has no applyFilters and
// doesn't call this, so it renders everything.

const _MAX_SHOWINGS_ROWS = 10;
const _PILLS_PER_ROW     = 6;
const _MIN_HIDDEN         = 3;

// Locale-aware plural category. Polish has three showtime forms
// (seans / seanse / seansów); English two (showing / showings). The rule +
// the forms both come from KINOWO_LOCALE (server-injected per deployment).
function _pluralCategory(n) {
  if (KINOWO_LOCALE.plural === 'pl') {
    if (n === 1) return 'one';
    const mod10 = n % 10, mod100 = n % 100;
    if (mod10 >= 2 && mod10 <= 4 && (mod100 < 12 || mod100 > 14)) return 'few';
    return 'many';
  }
  return n === 1 ? 'one' : 'other';
}

function _showtimeNoun(n) {
  const forms = KINOWO_LOCALE.showtime;
  return forms[_pluralCategory(n)] || forms.other || forms.many;
}

// Truncation folds rows away with the `.truncated` CLASS; the filters hide
// them with inline `style.display` (`setVisible`). Keeping the two on separate
// channels is what makes them safe to interleave: neither can undo the other's
// decision by accident, and unfolding a row hands it straight back to whatever
// the filter had decided for it. Nothing here reads or writes inline display.
const TRUNCATED = 'truncated';

// Unfold every folded row under `root` (the whole page by default).
function undoTruncation(root) {
  (root || document).querySelectorAll('.' + TRUNCATED).forEach(element => element.classList.remove(TRUNCATED));
}

// Fold one card's showings below ~10 rows, given what the last filter pass
// left visible: `days` is its `.date-group`s in order, each `{ element,
// visible, cinemaGroups: [{ element, visible, visibleCount }] }`, and `link`
// its "… +N" `.showings-more`. The listing hands in its INDEX, already holding
// every one of those facts from the pass it just ran (on New York, ~54k pills
// it would otherwise re-query); the facet view, with no index, reads them off
// the DOM (`truncateShowings`).
function foldShowings(link, days, hasCinemaHeaders) {
  if (!link) return;
  const hidden = foldPastRowLimit(days, hasCinemaHeaders);
  if (hidden > _MIN_HIDDEN) {
    link.textContent = '… +' + hidden + ' ' + _showtimeNoun(hidden);
    link.style.display = '';
  } else {
    if (hidden > 0) unfoldDays(days);   // too few folded away to be worth a link
    link.style.display = 'none';
  }
}

// Walks the card's visible rows in order, keeping each cinema group while it
// still fits in `_MAX_SHOWINGS_ROWS` and folding every one from the first that
// does not; a day left with nothing kept after that folds whole. Returns how
// many showtimes it folded away.
function foldPastRowLimit(days, hasCinemaHeaders) {
  const walk = { lineCount: 0, hidden: 0, capped: false };
  for (const day of days) {
    if (!day.visible) continue;
    const dayLines = foldDay(day, walk, hasCinemaHeaders);
    if (dayLines > 0) walk.lineCount += dayLines;
    else if (walk.capped) day.element.classList.add(TRUNCATED);
  }
  return walk.hidden;
}

// One day of `foldPastRowLimit`'s walk: the lines the day takes (its label row
// and every cinema group kept), or 0 when it kept none.
function foldDay(day, walk, hasCinemaHeaders) {
  const headerLines = hasCinemaHeaders ? 1 : 0;
  let dayLines = 1;   // the day's own label row
  let kept = false;
  for (const cinemaGroup of day.cinemaGroups) {
    const visibleBadges = cinemaGroup.visible ? cinemaGroup.visibleCount : 0;
    if (visibleBadges === 0) continue;
    const cinemaLines = headerLines + Math.max(1, Math.ceil(visibleBadges / _PILLS_PER_ROW));
    // Once one group does not fit, every later one folds too, fitting or not.
    walk.capped = walk.capped || walk.lineCount + dayLines + cinemaLines > _MAX_SHOWINGS_ROWS;
    cinemaGroup.element.classList.toggle(TRUNCATED, walk.capped);
    if (walk.capped) {
      walk.hidden += visibleBadges;
    } else {
      kept = true;
      dayLines += cinemaLines;
    }
  }
  return kept ? dayLines : 0;
}

function unfoldDays(days) {
  for (const day of days) {
    day.element.classList.remove(TRUNCATED);
    for (const cinemaGroup of day.cinemaGroups) cinemaGroup.element.classList.remove(TRUNCATED);
  }
}

// `foldShowings` for a card with no index behind it: the visibility the
// filter pass wrote is read back off the inline styles.
function truncateShowings(cardEl, hasCinemaHeaders) {
  const shown = element => element.style.display !== 'none';
  const days = [...cardEl.querySelectorAll('.date-group')].map(day => ({
    element:      day,
    visible:      shown(day),
    cinemaGroups: [...day.querySelectorAll('.cinema-group')].map(cinemaGroup => ({
      element:      cinemaGroup,
      visible:      shown(cinemaGroup),
      visibleCount: shown(cinemaGroup) ? [...cinemaGroup.querySelectorAll('.badge-time')].filter(shown).length : 0
    }))
  }));
  foldShowings(cardEl.querySelector('.showings-more'), days, hasCinemaHeaders);
}

function truncateAllShowings(hasCinemaHeaders) {
  gridScope().querySelectorAll('.col[data-title]').forEach(col =>
    truncateShowings(col, hasCinemaHeaders)
  );
}

