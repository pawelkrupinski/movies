// The /debug corpus and read-model tables (`#t`), read and driven for
// `PageJsBehaviourSpec`, as `window.__debugTable`.
/* global applySse, flushSse */
(function () {
  function all(selector) { return Array.prototype.slice.call(document.querySelectorAll(selector)); }
  function dataRows() { return all('#t tbody tr.data'); }
  function shownRows() { return dataRows().filter(function (tr) { return !tr.classList.contains('hidden'); }); }
  function rowMatching(fragment) { return dataRows().find(function (tr) { return tr.dataset.haystack.includes(fragment); }); }

  window.__debugTable = {
    // The data rows' titles, pipe-joined in table order.
    titles: function () { return dataRows().map(function (r) { return r.dataset.title; }).join('|'); },
    // How many details rows sit in the table.
    attachedDetails: function () { return document.querySelectorAll('#t tbody tr.details').length; },
    // Every expanded data row is immediately followed by its own visible details row.
    expandedDetailsAdjacent: function () {
      return all('#t tbody tr.data.expanded').every(function (r) {
        var n = r.nextElementSibling;
        return !!n && n.classList.contains('details') && n.dataset.rowId === r.dataset.rowId &&
          !n.classList.contains('hidden');
      });
    },
    // Some details cell holds `marker` — its fetched body has landed.
    detailsHold: function (marker) {
      return all('#t tbody tr.details td').some(function (td) { return td.innerHTML.indexOf(marker) !== -1; });
    },
    // The data row whose haystack holds `fragment`, and that row's details row.
    row: rowMatching,
    detailsOf: function (fragment) {
      return document.querySelector('#t tbody tr.details[data-row-id="' + rowMatching(fragment).dataset.rowId + '"]');
    },
    clickRowMatching: function (fragment) { rowMatching(fragment).click(); },
    rowExpanded: function (fragment) { return rowMatching(fragment).classList.contains('expanded'); },
    shownRows: function () { return shownRows().length; },
    shownHaystacks: function () { return shownRows().map(function (tr) { return tr.dataset.haystack.trim(); }).join('|'); },
    // Count, into `detailFetches`, every `/debug/details` fetch from now on.
    countDetailFetches: function () {
      var counter = window.__debugTable;
      counter.detailFetches = 0;
      var realFetch = window.fetch;
      window.fetch = function (url) {
        if (String(url).indexOf('/debug/details') !== -1) counter.detailFetches++;
        return realFetch.apply(this, arguments);
      };
    },
    // Replay a change-stream upsert of row `id` carrying `html`.
    applyUpsert: function (id, html) {
      applySse(JSON.stringify({ type: 'upsert', id: id, html: html }));
      flushSse();
    },
    // The Cinemas cell is a plain table-cell, its slot count to the right of the
    // count and on the same line.
    slotCountBesideCount: function () {
      var td = document.querySelector('#t tbody td.cinemas');
      var slot = td.querySelector('.slots');
      var tdR = td.getBoundingClientRect();
      var sR = slot.getBoundingClientRect();
      return getComputedStyle(td).display === 'table-cell' && sR.left > tdR.left && Math.abs(sR.top - tdR.top) < sR.height;
    },
  };
})();
