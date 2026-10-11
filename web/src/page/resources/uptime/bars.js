// The /uptime rows' bars, read and hovered for `UptimeLiveBarsSpec`, as `window.__uptime`.
/* global barDetail */
(function () {
  function rowSelector(service) { return '.row[data-service="' + service + '"]'; }

  window.__uptime = {
    // The bar for `service`'s bucket at `ts`, or null.
    bar: function (service, ts) {
      return document.querySelector(rowSelector(service) + ' .bar[data-ts="' + ts + '"]');
    },
    bars: function (service) { return document.querySelectorAll(rowSelector(service) + ' .bar').length; },
    // The row's bucket timestamps, comma-joined in bar order.
    order: function (service) {
      return Array.prototype.map.call(document.querySelectorAll(rowSelector(service) + ' .bar'), function (b) { return b.dataset.ts; }).join(',');
    },
    hover: function (service, ts) {
      window.__uptime.bar(service, ts).dispatchEvent(new MouseEvent('mouseover', { bubbles: true }));
    },
    // Every bucket of `service`'s hover payload still has a bar on its row.
    detailOnlyForBars: function (service) {
      return Object.keys(barDetail[service]).every(function (ts) { return window.__uptime.bar(service, ts); });
    },
  };
})();
