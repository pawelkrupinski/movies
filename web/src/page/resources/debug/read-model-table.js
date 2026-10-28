// The /debug/readmodel table (`#t`), read for `DebugReadModelFilterSpec`, as `window.__debugTable`.
(function () {
  function all(selector) { return Array.prototype.slice.call(document.querySelectorAll(selector)); }
  function dataRows() { return all('#t tbody tr.data'); }
  function rowMatching(fragment) { return dataRows().find(function (tr) { return tr.dataset.haystack.includes(fragment); }); }

  window.__debugTable = {
    // The data row whose haystack holds `fragment`, and that row's details row.
    row: rowMatching,
    detailsOf: function (fragment) {
      return document.querySelector('#t tbody tr.details[data-row-id="' + rowMatching(fragment).dataset.rowId + '"]');
    },
    shownRows: function () { return dataRows().filter(function (tr) { return !tr.classList.contains('hidden'); }).length; },
  };
})();
