// Installed before any page script by `HiddenFilmsSyncModelSpec`. Counts this
// document's own fetches still in flight from the moment it starts, so "nothing
// in flight" can be asked of the page itself, and drives the page's hide button
// the way a visitor does.
/* global hideFilm, _serverSyncTimer */
(function () {
  var inflight = 0;
  var original = window.fetch.bind(window);
  var done = function () { setTimeout(function () { inflight--; }, 0); };
  window.fetch = function () {
    inflight++;
    var p;
    try { p = original.apply(null, arguments); } catch (e) { done(); throw e; }
    p.then(done, done);
    return p;
  };

  window.__syncModel = {
    // Loaded, nothing in flight, and no debounced language push armed.
    idle: function () {
      return document.readyState === 'complete' && inflight === 0 &&
        (typeof _serverSyncTimer === 'undefined' || _serverSyncTimer === 0);
    },
    // The card button's own handler, on a card carrying `title`.
    hideFilm: function (title) {
      var card = document.createElement('div');
      card.setAttribute('data-title', title);
      var button = document.createElement('button');
      card.appendChild(button);
      document.body.appendChild(card);
      hideFilm(button);
    },
  };
})();
