// The listing's day carousel — `#view-root` between the neighbour day columns
// inside `#day-track` — driven and read for `PageJsBehaviourSpec`, as
// `window.__carousel`.
/* global applyFiltersForDay, slotTime */
(function () {
  function all(selector, root) { return Array.prototype.slice.call((root || document).querySelectorAll(selector)); }
  function unfiltered(element) { return element.style.display !== 'none'; }

  // The nearest `.day-col` on `side` of `#view-root` ('nextElementSibling' or
  // 'previousElementSibling'), if any.
  function columnBeside(root, side) {
    var element = root[side];
    while (element && !element.classList.contains('day-col')) element = element[side];
    return element;
  }

  var heldFrames = [];
  var realRaf = null;

  window.__carousel = {
    // A finger `kind` ('pointerdown' / 'pointermove' / 'pointerup') at (x, y).
    drag: function (kind, x, y) {
      document.dispatchEvent(new PointerEvent(kind, {
        pointerType: 'touch', isPrimary: true, clientX: x, clientY: y, bubbles: true, cancelable: true,
      }));
    },
    // The visible titles of the NEXT-day preview column, pipe-joined in the
    // order releasing the drag would commit.
    neighbourTitles: function () {
      var root = document.getElementById('view-root');
      if (!root) return '';
      var column = columnBeside(root, 'nextElementSibling');
      if (!column) return '';
      return all('.col[data-title]', column).filter(unfiltered).map(function (c) { return c.dataset.title; }).join('|');
    },
    // Which day that preview column is for: the next entry in the day ring.
    neighbourDay: function () {
      var select = document.getElementById('date-filter');
      var ring = window.dayRing ? window.dayRing() : Array.prototype.map.call(select.options, function (o) { return o.value; });
      var n = ring.length;
      return ring[((select.selectedIndex + 1) % n + n) % n];
    },
    // Which flank of `#view-root` holds the armed slide's populated column —
    // 'next' or 'previous' — or '' with no slide in flight.
    armedSide: function () {
      var root = document.getElementById('view-root');
      if (!root) return '';
      var populated = function (side) {
        var column = columnBeside(root, side);
        return !!(column && column.querySelector('.col[data-title]'));
      };
      if (populated('nextElementSibling')) return 'next';
      if (populated('previousElementSibling')) return 'previous';
      return '';
    },
    // A populated neighbour column's top lines up with `#view-root`'s.
    neighbourTopAligned: function () {
      var root = document.getElementById('view-root');
      var columns = all('#day-track > .day-col').filter(function (c) { return c.querySelector('.col[data-title]'); });
      if (!root || columns.length === 0) return false;
      var rootTop = root.getBoundingClientRect().top;
      return columns.some(function (c) { return Math.abs(c.getBoundingClientRect().top - rootTop) < 1; });
    },
    // Step a day and, in the same task, report [track armed, columns mounted].
    stepAndObserve: function () {
      window.stepDate(1);
      return [document.getElementById('day-track').classList.contains('day-track--armed'),
        document.querySelectorAll('#day-track > .day-col').length];
    },
    // Hold every animation frame from now, note when, and step a day.
    stepHoldingFrames: function () {
      heldFrames = [];
      realRaf = window.requestAnimationFrame;
      window.requestAnimationFrame = function (cb) { heldFrames.push(cb); return heldFrames.length; };
      window.__carousel.steppedAt = performance.now();
      window.stepDate(1);
    },
    // Let the held frames run, late, and frames run as normal again.
    releaseFrames: function () {
      window.requestAnimationFrame = realRaf;
      heldFrames.splice(0).forEach(function (cb) { cb(performance.now()); });
    },
    msSinceStep: function () { return performance.now() - window.__carousel.steppedAt; },
    // Some card in `#view-root` is shown and inside the viewport horizontally.
    gridOnScreen: function () {
      return all('#view-root .col[data-title]').filter(function (c) { return c.offsetParent !== null; })
        .some(function (c) {
          var r = c.getBoundingClientRect();
          return r.right > 0 && r.left < window.innerWidth;
        });
    },
    // Two visible cards share their earliest visible showtime on `day`.
    hasEarliestTie: function (day) {
      var earliest = function (card) {
        var best = null;
        all('.date-group[data-date="' + day + '"] .badge-time', card).filter(unfiltered).forEach(function (b) {
          var t = slotTime(b);
          if (t && (best === null || t < best)) best = t;
        });
        return best;
      };
      var times = all('#film-grid > .col[data-title]').filter(unfiltered).map(earliest).filter(Boolean);
      return new Set(times).size < times.length;
    },
    // Leave the live grid in reverse DOM order, as a prior day's sort can.
    reverseGrid: function () {
      var grid = document.getElementById('film-grid');
      all('.col[data-title]', grid).reverse().forEach(function (c) { grid.appendChild(c); });
    },
    // The visible titles of a clone of `#film-grid` filtered to `day` as the
    // preview column is, pipe-joined.
    previewOrder: function (day) {
      var clone = document.getElementById('film-grid').cloneNode(true);
      applyFiltersForDay(clone, day);
      return all('.col[data-title]', clone).filter(unfiltered).map(function (c) { return c.dataset.title; }).join('|');
    },
    // The day `steps` options on from the selected one.
    dayAhead: function (steps) {
      var select = document.getElementById('date-filter');
      return select.options[select.selectedIndex + steps].value;
    },
  };
})();
