// DOM moves and reads the page specs share across pages, as `window.__dom`.
(function () {
  function all(selector) { return Array.prototype.slice.call(document.querySelectorAll(selector)); }

  window.__dom = {
    // Put `value` in the input at `selector` and fire its `input` handler, as typing does.
    type: function (selector, value) {
      var input = document.querySelector(selector);
      input.value = value;
      input.dispatchEvent(new Event('input'));
    },
    // Click the first element matching each selector, in turn.
    click: function () {
      Array.prototype.forEach.call(arguments, function (selector) { document.querySelector(selector).click(); });
    },
    // Set each form control named by id in `values` to its value, firing nothing.
    fill: function (values) {
      Object.keys(values).forEach(function (id) { document.getElementById(id).value = values[id]; });
    },
    // The trimmed text of every element matching `selector`, in DOM order.
    texts: function (selector) {
      return all(selector).map(function (el) { return el.textContent.trim(); });
    },
    // Click the first element matching `selector` whose trimmed text is `text`.
    clickText: function (selector, text) {
      all(selector).filter(function (el) { return el.textContent.trim() === text; })[0].click();
    },
    // The first element matching `selector` exists and has a box of some size.
    hasBox: function (selector) {
      var element = document.querySelector(selector);
      if (!element) return false;
      var r = element.getBoundingClientRect();
      return r.width > 0 && r.height > 0;
    },
    // Fire a cancelable `submit` at the form matching `selector`: its listeners
    // run, and nothing navigates.
    submit: function (selector) {
      document.querySelector(selector).dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
    },
    // [top, left] of the first element matching `selector`.
    topLeft: function (selector) {
      var r = document.querySelector(selector).getBoundingClientRect();
      return [r.top, r.left];
    },
    // Dispatch `event` on the document itself — a target with no `.closest` —
    // and say whether every handler took it without throwing.
    dispatchesOnDocument: function (event) {
      try {
        document.dispatchEvent(event);
        return true;
      } catch { return false; }
    },
    // Pin every element to Arial / Liberation Sans, metric-compatible on macOS
    // and the Linux runners, ahead of a font-sensitive layout measurement.
    pinFont: function () {
      var style = document.createElement('style');
      style.textContent = '*, body { font-family: Arial, "Liberation Sans", sans-serif !important; }';
      document.head.appendChild(style);
    },
  };
})();
