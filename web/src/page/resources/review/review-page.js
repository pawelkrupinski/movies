// The /debug/review pages, read and driven for `ReviewPageSpec`, as `window.__review`.
(function () {
  function all(selector, root) { return Array.prototype.slice.call((root || document).querySelectorAll(selector)); }

  window.__review = {
    // The card whose venue title is exactly `title`.
    card: function (title) {
      return all('.card').find(function (c) { return c.querySelector('.raw-title').textContent === title; });
    },
    // Put `text` in the card's Other-film box, firing its `input` handler.
    typeOther: function (title, text) {
      var input = window.__review.card(title).querySelector('input.other');
      input.value = text;
      input.dispatchEvent(new Event('input', { bubbles: true }));
    },
    // The colour a `color: value` declaration resolves to on this page.
    resolvedColor: function (value) {
      var probe = document.createElement('span');
      probe.style.color = value;
      document.body.appendChild(probe);
      var color = getComputedStyle(probe).color;
      probe.remove();
      return color;
    },
    // "text href target" of every link under `root` matching `selector`, pipe-joined.
    links: function (root, selector) {
      return all(selector, root).map(function (a) { return a.textContent + ' ' + a.getAttribute('href') + ' ' + a.target; }).join('|');
    },
    // Some element under `root` matching `selector` mentions `text`.
    anyMentions: function (root, selector, text) {
      return all(selector, root).some(function (el) { return el.textContent.indexOf(text) >= 0; });
    },
    // The image is there and has loaded with a size.
    loaded: function (img) { return !!img && img.complete && img.naturalWidth > 0; },
    // Every card's payload parses and names its own cluster, a title and its listings.
    payloadsWhole: function () {
      return all('.card').every(function (c) {
        var p = JSON.parse(c.getAttribute('data-card'));
        return p.clusterId === c.getAttribute('data-cluster') && p.listings.length > 0 && typeof p.title === 'string';
      });
    },
  };
})();
