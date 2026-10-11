// The showings tree as the browser rebuilds it, and its showtimes lapsing, for
// `PageJsBehaviourSpec`, as `window.__expiry`. Times are on the page's own clock,
// `showtimeNow()`.
/* global showtimeNow, slotTime, pruneExpiredShowtimes, dateBounds */
(function () {
  function all(selector, root) { return Array.prototype.slice.call((root || document).querySelectorAll(selector)); }
  function cards() { return all('#film-grid .col[data-title]'); }
  function shownBadges(root) { return all('.badge-time', root).filter(function (b) { return b.offsetParent !== null; }); }
  // A badge with no expiry of its own: it lapses at its day's base plus its clock time.
  function badgeOnDayBase() { return all('.badge-time').find(function (x) { return !x.dataset.expires; }); }
  function minutesOf(time) { var hm = time.split(':').map(Number); return hm[0] * 60 + hm[1]; }

  window.__expiry = {
    // Every cinema group is named for its label, and every link in the tree is
    // whole, opening in a new tab, nofollowed and classed — `_showingsHydrate`'s work.
    hydrated: function () {
      var groups = all('.cinema-group');
      var label = function (g) { return g.querySelector('.cinema-label').textContent.replace(/\s*↗\s*$/, '').trim(); };
      return groups.length > 0 &&
        groups.every(function (g) { return g.dataset.cinema && g.dataset.cinema === label(g); }) &&
        all('.cinema-group a').every(function (a) {
          return /^https?:\/\//.test(a.getAttribute('href') || '') &&
            a.getAttribute('target') === '_blank' && a.getAttribute('rel') === 'nofollow' &&
            (a.classList.contains('badge-time') || a.classList.contains('cinema-label-link'));
        });
    },
    // Set the first badge to lapse `offsetMs` from now.
    expireFirst: function (offsetMs) {
      document.querySelector('.badge-time').dataset.expires = String(showtimeNow() + offsetMs);
    },
    // Set every badge under `root` (default: the page) to have lapsed.
    expireAll: function (root) {
      all('.badge-time', root).forEach(function (b) { b.dataset.expires = String(showtimeNow() - 1); });
    },
    // Move a day's base so exactly its earliest-lapsing day-based badge has
    // lapsed, prune, and say whether it went and every later one stayed.
    lapseAtDayBase: function () {
      var badge = badgeOnDayBase();
      var day = badge.closest('.date-group');
      day.dataset.expiresFrom = String(showtimeNow() - 1 - minutesOf(slotTime(badge)) * 60000);
      var later = all('.badge-time', day).filter(function (x) { return !x.dataset.expires && slotTime(x) > slotTime(badge); });
      pruneExpiredShowtimes();
      return !badge.isConnected && later.every(function (x) { return x.isConnected; });
    },
    // A 12-hour pill's text reads back as the 24-hour clock it spells, and
    // lapses on that clock — not a minute early, nor twelve hours.
    twelveHourPillReadsBack: function () {
      var badge = badgeOnDayBase();
      var day = badge.closest('.date-group');
      badge.firstChild.nodeValue = '7:30 PM';
      var afternoon = slotTime(badge) === '19:30';
      badge.firstChild.nodeValue = '12:05 AM';
      var midnight = slotTime(badge) === '00:05';
      badge.firstChild.nodeValue = '12:30 PM';
      var noon = slotTime(badge) === '12:30';
      badge.firstChild.nodeValue = '11:15 PM';
      day.dataset.expiresFrom = String(showtimeNow() + 60000 - (23 * 60 + 15) * 60000);
      pruneExpiredShowtimes();
      var kept = badge.isConnected;
      day.dataset.expiresFrom = String(showtimeNow() - 1 - (23 * 60 + 15) * 60000);
      pruneExpiredShowtimes();
      return afternoon && midnight && noon && kept && !badge.isConnected;
    },
    // Lapse every slot of the first card and prune; whether the card went.
    lapseFirstCard: function () {
      var col = document.querySelector('#film-grid .col[data-title]');
      var title = col.dataset.title;
      window.__expiry.expireAll(col);
      pruneExpiredShowtimes();
      return !cards().some(function (c) { return c.dataset.title === title; });
    },
    // Lapse one slot of a film other than `title` and prune; how many went.
    lapseOneNotOf: function (title) {
      var slot = all('.badge-time').find(function (b) { return b.closest('.col[data-title]').dataset.title !== title; });
      slot.dataset.expires = String(showtimeNow() - 1);
      return pruneExpiredShowtimes();
    },
    // Count, into `dayLookups`, every time the page looks a pill's day up from
    // the pill, from now on.
    watchDayLookups: function () {
      window.__expiry.dayLookups = 0;
      var original = Element.prototype.closest;
      Element.prototype.closest = function (selector) {
        if (selector === '.date-group') window.__expiry.dayLookups++;
        return original.call(this, selector);
      };
    },
    // The first card titled `title`.
    cardOf: function (title) { return cards().find(function (c) { return c.dataset.title === title; }); },
    // The title of a film with a day-row on the corpus's tomorrow, or ''.
    filmPlayingTomorrow: function () {
      var tomorrow = dateBounds().tomorrow;
      var col = cards().find(function (c) { return c.querySelector('.date-group[data-date="' + tomorrow + '"]'); });
      return col ? col.dataset.title : '';
    },
    // The card titled `title` is on screen.
    cardShown: function (title) {
      return cards().some(function (c) { return c.dataset.title === title && c.style.display !== 'none'; });
    },
    // How many showtimes show under the cards titled `title`.
    shownBadgesOf: function (title) {
      return cards().filter(function (c) { return c.dataset.title === title; })
        .flatMap(function (c) { return shownBadges(c); }).length;
    },
    // Every visible card shows at least one showtime.
    everyShownCardHasBadge: function () {
      return cards().filter(function (c) { return c.style.display !== 'none'; })
        .every(function (c) { return shownBadges(c).length > 0; });
    },
  };
})();
