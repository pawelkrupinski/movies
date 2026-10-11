// Installed before any page script (`Page.addScriptToEvaluateOnNewDocument`) by
// `PageJsBehaviourSpec`'s landing tests: holds every `/api/picker/:country`
// fetch the landing makes until the test releases it, so the test sees what a
// visitor sees while a pane is in flight. Codes listed in
// `window.__paneFailing` (set just ahead of this file) are answered 500 at
// once instead, and keep failing until `fail(code, false)`.
(function () {
  var realFetch = window.fetch.bind(window);
  var held = {};
  var failing = {};
  (window.__paneFailing || []).forEach(function (code) { failing[code] = true; });

  var panes = {
    requests: [],   // every pane request, by country code, in order
    open: 0,
    maxOpen: 0,     // how many were in flight at once — all of them, if parallel

    release: function (code) {
      var queue = held[code] || [];
      delete held[code];
      queue.forEach(function (go) { go(); });
    },
    fail: function (code, on) { failing[code] = on; },
    requestsFor: function (code) {
      return panes.requests.filter(function (c) { return c === code; }).length;
    },
    arrived: function (code) {
      var all = document.getElementById('picker-panes').children;
      return Array.prototype.some.call(all, function (pane) { return pane.dataset.country === code; });
    },
    shownPanes: function () {
      return Array.prototype.filter.call(document.querySelectorAll('.picker-pane'), function (pane) {
        return getComputedStyle(pane).display !== 'none';
      }).length;
    },
    shown: function (id) { return !document.getElementById(id).hidden; },
    // "Use my location" from (lat, lon), with the pick it ends in recorded in
    // `picked` rather than navigated to.
    locateAt: function (lat, lon) {
      navigator.geolocation.getCurrentPosition = function (ok) { ok({ coords: { latitude: lat, longitude: lon } }); };
      window.pickPickerCity = function (city) { panes.picked = city; };
      window.locatePickerAnywhere();
    },
  };
  window.__panes = panes;

  window.fetch = function (url, options) {
    var match = /\/api\/picker\/([a-z]+)/.exec(String(url));
    if (!match) return realFetch(url, options);
    var code = match[1];
    panes.requests.push(code);
    if (failing[code]) return Promise.resolve(new Response('', { status: 500 }));
    panes.open++;
    panes.maxOpen = Math.max(panes.maxOpen, panes.open);
    return new Promise(function (resolve, reject) {
      (held[code] = held[code] || []).push(function () {
        panes.open--;
        realFetch(url, options).then(resolve, reject);
      });
    });
  };
})();
