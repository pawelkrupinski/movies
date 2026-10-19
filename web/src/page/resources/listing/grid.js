// The listing's film grid, read and driven for `PageJsBehaviourSpec`, as
// `window.__grid`. "Visible" is the filter's own verdict: no inline `display: none`.
/* global applyFilters, slotTime, hideFilm */
(function () {
  function all(selector, root) { return Array.prototype.slice.call((root || document).querySelectorAll(selector)); }
  function unfiltered(element) { return element.style.display !== 'none'; }
  function visibleCards() { return all('#film-grid > .col[data-title]').filter(unfiltered); }
  function visibleBadges() { return all('.badge-time').filter(unfiltered); }
  function formatTokens(badge) { return (badge.dataset.format || '').split(' '); }
  function cardButton(title) { return document.querySelector('.col[data-title=' + JSON.stringify(title) + '] .hide-btn'); }

  // Count every call the page makes to `names` on each prototype while `run`
  // runs, putting the originals back after.
  function countCalls(targets, run) {
    var count = 0;
    var wrapped = [];
    targets.forEach(function (target) {
      target[1].forEach(function (name) {
        var original = target[0][name];
        wrapped.push([target[0], name, original]);
        target[0][name] = function () { count++; return original.apply(this, arguments); };
      });
    });
    try { run(); } finally { wrapped.forEach(function (w) { w[0][w[1]] = w[2]; }); }
    return count;
  }

  window.__grid = {
    visibleCards: function () { return all('.col[data-title]').filter(unfiltered).length; },
    visibleBadges: function () { return visibleBadges().length; },
    // Pipe-joined titles of the visible grid cards, in DOM order.
    titleOrder: function () {
      return visibleCards().map(function (c) { return c.dataset.title; }).join('|');
    },
    // Every visible badge carries each of `tokens` among its format tokens.
    badgesAllCarry: function (tokens) {
      return visibleBadges().every(function (b) {
        var carried = formatTokens(b);
        return tokens.every(function (t) { return carried.includes(t); });
      });
    },
    // Every visible badge starts at `minutes` past midnight or later.
    badgesFrom: function (minutes) {
      return visibleBadges().every(function (b) {
        var hm = slotTime(b).split(':').map(Number);
        return hm[0] * 60 + hm[1] >= minutes;
      });
    },
    // Every visible card's title contains `fragment`, case-insensitively.
    titlesContain: function (fragment) {
      return all('.col[data-title]').filter(unfiltered).every(function (c) { return c.dataset.title.toLowerCase(java.util.Locale.ROOT).includes(fragment); });
    },
    // The visible cards' ratings never rise down the grid (and there are two or more).
    ratingsNonIncreasing: function () {
      var r = visibleCards().map(function (c) { return parseFloat(c.dataset.rating) || 0; });
      for (var i = 1; i < r.length; i++) if (r[i] > r[i - 1] + 1e-9) return false;
      return r.length > 1;
    },
    // The ISO dates of every unfiltered day-row of every visible card.
    shownDates: function () {
      return all('#film-grid .col[data-title]').filter(unfiltered)
        .flatMap(function (c) { return all('.date-group[data-date]', c); })
        .filter(unfiltered).map(function (g) { return g.dataset.date; });
    },
    // Every day-row of every visible card is on screen.
    everyDayRowShown: function () {
      return all('#film-grid .col[data-title]').filter(unfiltered)
        .flatMap(function (c) { return all('.date-group[data-date]', c); })
        .every(function (g) { return g.offsetParent !== null; });
    },
    // Stop the page's clock at `ms` (epoch millis), with the render's pinned
    // "today" dropped so the day bounds are measured off it.
    clockAt: function (ms) {
      delete window.KINOWO_PINNED_TODAY;
      window.showtimeNow = function () { return ms; };
    },
    // Point `#date-filter` at the ISO date `day` — adding the option the
    // URL-sync / stepDate path would have — and run a filter pass.
    selectDay: function (day) {
      var select = document.getElementById('date-filter');
      var option = document.createElement('option');
      option.value = day;
      select.appendChild(option);
      select.value = day;
      applyFilters();
    },
    // Each visible card's earliest visible showtime on `day`, pipe-joined in DOM
    // order, read off `slotTime` as `badgeFacts` does; cards with none on `day`
    // contribute nothing.
    earliestTimes: function (day) {
      var earliest = function (card) {
        var best = null;
        all('.date-group[data-date="' + day + '"]', card).filter(unfiltered).forEach(function (g) {
          all('.cinema-group[data-cinema]', g).filter(unfiltered).forEach(function (cg) {
            all('.badge-time', cg).filter(unfiltered).forEach(function (b) {
              var t = slotTime(b);
              if (t && (best === null || t < best)) best = t;
            });
          });
        });
        return best;
      };
      return visibleCards().map(earliest).filter(Boolean).join('|');
    },
    // The title of the first visible card with a shown poster, else of the first visible card.
    firstTitleWithPoster: function () {
      var cards = all('.col[data-title]');
      for (var i = 0; i < cards.length; i++) {
        var c = cards[i];
        if (!unfiltered(c)) continue;
        var img = c.querySelector('.poster-wrap > a img');
        if (img && unfiltered(img)) return c.dataset.title;
      }
      var first = cards.find(unfiltered);
      return (first && first.dataset.title) || '';
    },
    // The poster link of the first visible card.
    firstPosterLink: function () {
      var card = all('.col[data-title]').find(unfiltered);
      return card && card.querySelector('.card .poster-wrap > a');
    },
    // A real, cancelable click on the first visible card's poster link: whether
    // something prevented its default.
    clickFirstPosterPrevented: function () {
      var event = new MouseEvent('click', { bubbles: true, cancelable: true });
      window.__grid.firstPosterLink().dispatchEvent(event);
      return event.defaultPrevented;
    },
    // The visible cards directly in `#film-grid`.
    visibleGridCards: function () { return visibleCards().length; },
    // The ✕ on the card titled `title`, through the page's own handler.
    hideCard: function (title) { hideFilm(cardButton(title)); },
    // The same, on a page where reading `localStorage` throws, as blocked site
    // data makes it, and with the avatar menu gone so the page reads as anonymous.
    hideCardWithStorageBlocked: function (title) {
      document.getElementById('auth-menu').remove();
      Object.defineProperty(window, 'localStorage', { configurable: true,
        get: function () { throw new DOMException('The operation is insecure.', 'SecurityError'); } });
      hideFilm(cardButton(title));
    },
    // Hide every card in the grid through the hidden-films list, and run a pass.
    hideEveryCard: function () {
      localStorage.setItem('hiddenFilms:pl', JSON.stringify(all('#film-grid > .col[data-title]').map(function (c) { return c.dataset.title; })));
      applyFilters();
    },
    // Count, into `filterPasses`, every full `applyFilters` pass from now on.
    countFilterPasses: function () {
      var grid = window.__grid;
      grid.filterPasses = 0;
      var original = window.applyFilters;
      window.applyFilters = function () { grid.filterPasses++; return original.apply(this, arguments); };
    },
    // How many times one filter pass asks an element for its `.badge-time` pills.
    pillQueriesPerPass: function () {
      var count = 0;
      var original = Element.prototype.querySelectorAll;
      Element.prototype.querySelectorAll = function (selector) {
        if (selector === '.badge-time') count++;
        return original.call(this, selector);
      };
      try { applyFilters(); } finally { Element.prototype.querySelectorAll = original; }
      return count;
    },
    // Every DOM query one filter pass makes.
    domQueriesPerPass: function () {
      var lookups = ['querySelectorAll', 'querySelector', 'getElementsByClassName', 'getElementsByTagName'];
      return countCalls([[Element.prototype, lookups], [Document.prototype, lookups.concat(['getElementById'])]], applyFilters);
    },
  };
})();
