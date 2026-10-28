// ── /debug/readmodel's table filter + expand ─────────────────────────────
//
// The ROW-LEVEL STATE MACHINE of a `#t > tbody > tr.data[data-haystack]` table
// with a `#q` search box wired to `oninput="filterRows(...)"`: a click toggles
// `.expanded`, the filter toggles `.hidden` and force-collapses anything it
// hides. What "expand"/"collapse" mean is the page's own `onExpand`/`onCollapse`.
function wireExpandableTable(tableSelector, onExpand, onCollapse) {
  function rows() {
    return document.querySelectorAll(tableSelector + ' tbody tr.data');
  }

  function wireRow(tr) {
    tr.addEventListener('click', (e) => {
      // Bail when the click landed on an `<a>` or `<button>` inside the row
      // (a rating/id/URL link) so it reaches its own action instead of also
      // toggling the row open/closed.
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
      // details are re-hidden. Rows that stay visible keep whatever expanded
      // state they had.
      if (!hit && tr.classList.contains('expanded')) {
        tr.classList.remove('expanded');
        onCollapse(tr);
      }
    });
  }

  return { filterRows };
}
