// The contract between the shared scripts and the pages that load them, probed for
// `PageJsBehaviourSpec`, as `window.__sharedScripts`. Installed ahead of the page's own
// scripts (`installScript`) so the error log starts before the first of them runs.
(function () {
  var errors = [];
  // Capture phase on `window`: a `<script src>` that fails to load fires a non-bubbling
  // `error` at its element, which only a capturing listener up here sees. An image that
  // fails is a poster, never a script problem, so only scripts count.
  window.addEventListener('error', function (event) {
    var target = event.target;
    if (target && target !== window) {
      if (target.tagName === 'SCRIPT') errors.push('failed to load ' + target.src);
      return;
    }
    errors.push(String(event.message) + ' at ' + event.filename + ':' + event.lineno);
  }, true);

  window.__sharedScripts = {
    // Every uncaught script error this document raised, oldest first.
    errors: function () { return errors.slice(); },
    // The names in `names` that are not functions on this page.
    missingFunctions: function (names) {
      return names.filter(function (name) { return typeof window[name] !== 'function'; });
    },
    // A phone's tap on a film card while the search field is focused and the Filtry
    // panel is open: what the tap did. Its own `pointerdown` runs first, as a real tap's.
    tapCardWithSearchFocusedAndPanelOpen: function () {
      var search = document.getElementById('search-input');
      var panel = document.getElementById('format-panel');
      var card = document.querySelector('#film-grid .col[data-title] .card');
      search.focus();
      panel.style.display = 'block';
      card.dispatchEvent(new PointerEvent('pointerdown', { bubbles: true, cancelable: true, pointerType: 'touch' }));
      var click = new MouseEvent('click', { bubbles: true, cancelable: true });
      card.dispatchEvent(click);
      return {
        searchFocused: document.activeElement === search,
        panelOpen: panel.style.display !== 'none',
        clickPrevented: click.defaultPrevented,
      };
    },
    // Click the first element matching `selector` and say whether anything the page
    // registered prevented the click's default; a last listener prevents it after
    // reading, so a link's own navigation never runs.
    clickPrevented: function (selector) {
      var prevented = null;
      function read(event) { prevented = event.defaultPrevented; event.preventDefault(); }
      window.addEventListener('click', read);
      try {
        document.querySelector(selector).dispatchEvent(new MouseEvent('click', { bubbles: true, cancelable: true }));
      } finally {
        window.removeEventListener('click', read);
      }
      return prevented;
    },
  };
})();
