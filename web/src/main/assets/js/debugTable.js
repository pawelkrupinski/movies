// ── Shared /debug admin-table filter + expand ────────────────────────────
//
// `debug.scala.html`'s live corpus dump and `debugReadModel.scala.html`'s
// read-cache dump both render the same `#t > tbody > tr.data[data-haystack]`
// shape and both offer a `#q` search box wired to `oninput="filterRows(...)"`
// — but each page carries its own idea of what "expanding a row" actually
// does: `debug.scala.html` parks details off-DOM in a JS `Map` and fetches
// them lazily on first expand (an explicit fix for an OOM the full corpus
// once caused there), while `debugReadModel.scala.html` keeps them as plain
// DOM siblings found by a `[data-row-id]` attribute selector. This file owns
// only the shared ROW-LEVEL STATE MACHINE — click toggles `.expanded`, the
// filter toggles `.hidden` and force-collapses anything it hides — and leaves
// what "expand"/"collapse" mean to each page's own `onExpand`/`onCollapse`.
//
// `tableSelector` scopes every query to one table (`'#t'` on both pages
// today, but never assumed to be the only such table `wireExpandableTable`
// is ever asked to drive).
function wireExpandableTable(tableSelector, onExpand, onCollapse) {
  function rows() {
    return document.querySelectorAll(tableSelector + ' tbody tr.data');
  }

  // Wired per-row rather than via one delegated listener because
  // `debug.scala.html` needs to re-wire a SINGLE row a change-stream frame
  // just replaced, without touching every other row's listener.
  function wireRow(tr) {
    tr.addEventListener('click', (e) => {
      // Bail when the click landed on an `<a>` or `<button>` inside the row
      // (a rating/id/URL link, or debug.scala.html's re-enrich button) so it
      // reaches its own action instead of also toggling the row open/closed.
      if (e.target.closest('a') || e.target.closest('button')) return;
      const expanded = tr.classList.toggle('expanded');
      if (expanded) onExpand(tr); else onCollapse(tr);
    });
  }
  rows().forEach(wireRow);

  function filterRows(q) {
    const lq = q.trim().toLowerCase();
    rows().forEach(tr => {
      const hit = lq.length === 0 || tr.dataset.haystack.includes(lq);
      tr.classList.toggle('hidden', !hit);
      // A filtered-out row collapses so the layout stays tight and its
      // details follow whatever "hidden" means on this page (parked off-DOM
      // entirely, or just re-hidden in place). Rows that stay visible keep
      // whatever expanded state they had.
      if (!hit && tr.classList.contains('expanded')) {
        tr.classList.remove('expanded');
        onCollapse(tr);
      }
    });
  }

  return { wireRow, filterRows };
}
