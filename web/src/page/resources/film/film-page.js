// The /movie page, driven and read for `PageJsBehaviourSpec`, as `window.__film`.
/* global applyFilters */
(function () {
  function all(selector) { return Array.prototype.slice.call(document.querySelectorAll(selector)); }
  function shown(element) { return element.offsetParent !== null; }

  window.__film = {
    // Switch off the page's first `count` cinemas the way the Filtry sheet does —
    // by display name into `disabledCinemas` — re-run the filter, and return them.
    disableCinemas: function (count) {
      var names = all('.cinema-group[data-cinema]').map(function (g) { return g.dataset.cinema; }).slice(0, count);
      localStorage.setItem('disabledCinemas', JSON.stringify(names));
      applyFilters();
      return names;
    },
    // The cinemas whose showings-tree group is hidden.
    hiddenGroups: function () {
      return all('.cinema-group[data-cinema]').filter(function (g) { return !shown(g); })
        .map(function (g) { return g.dataset.cinema; });
    },
    // The cinemas whose link pill the filter hid (inline display, not the fold).
    filteredLinks: function () {
      return all('.cinema-link[data-cinema]').filter(function (a) { return a.style.display === 'none'; })
        .map(function (a) { return a.dataset.cinema; });
    },
    // The cinemas whose link pill shows.
    shownLinks: function () {
      return all('.cinema-link[data-cinema]').filter(shown).map(function (a) { return a.dataset.cinema; });
    },
    // [posterTop, posterRight, posterBottom, posterLeft, titleTop, titleLeft].
    heroGeometry: function () {
      var p = document.querySelector('.poster-wrap').getBoundingClientRect();
      var t = document.querySelector('.film-title').getBoundingClientRect();
      return [p.top, p.right, p.bottom, p.left, t.top, t.left];
    },
    // The full-screen poster has a box, and fills the viewport on one axis.
    lightboxFillsScreen: function () {
      var r = document.querySelector('#poster-lightbox img').getBoundingClientRect();
      return r.width > 0 && r.height > 0 && (r.width >= innerWidth - 1 || r.height >= innerHeight - 1);
    },
  };
})();
