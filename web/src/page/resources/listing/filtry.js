// The Filtry panel's cinema, area, room and submenu controls, driven and read
// for `PageJsBehaviourSpec`, as `window.__filtry`.
/* global ALL_CINEMAS, CURRENT_CITY, applyFilters, maybeShowAreaPicker */
(function () {
  function all(selector, root) { return Array.prototype.slice.call((root || document).querySelectorAll(selector)); }
  // A submenu's own checkboxes, without its "all" row.
  function entries(list) { return all(list + ' input[type="checkbox"]:not(.submenu-all)'); }
  function pickerAreas() { return document.querySelectorAll('#area-picker-overlay input[data-area-slug]'); }

  // Check or uncheck `box` the way a click on it reads to the page: its `change` fires.
  function setChecked(box, checked) {
    box.checked = checked;
    box.dispatchEvent(new Event('change'));
  }

  window.__filtry = {
    // Split the city's cinemas into `count` synthetic areas (1: all in "Group A";
    // 2: halves, "Group A" and "Group B") as a split city's `CINEMA_AREAS`.
    areas: function (count) {
      var cs = ALL_CINEMAS;
      if (count === 1) {
        window.CINEMA_AREAS = [{ name: 'Group A', slug: 'group-a', cinemas: cs }];
      } else {
        var mid = Math.ceil(cs.length / 2);
        window.CINEMA_AREAS = [
          { name: 'Group A', slug: 'group-a', cinemas: cs.slice(0, mid) },
          { name: 'Group B', slug: 'group-b', cinemas: cs.slice(mid) },
        ];
      }
    },
    // Two areas, nothing chosen or disabled yet, and the city-entry picker up.
    openAreaPicker: function () {
      window.__filtry.areas(2);
      localStorage.removeItem('areasChosen:' + CURRENT_CITY);
      localStorage.setItem('disabledCinemas', '[]');
      maybeShowAreaPicker();
    },
    // (Un)tick the first area's checkbox in the Filtry panel.
    setArea: function (checked) { setChecked(document.querySelector('.cinema-area-toggle'), checked); },
    // (Un)tick the Filtry row of the cinema named `name`.
    setCinema: function (name, checked) {
      setChecked(document.querySelector('#cinema-list input[data-cinema="' + CSS.escape(name) + '"]'), checked);
    },
    // (Un)tick the picker's `index`th area; `notify` runs its handler, as a tap would.
    setPickerArea: function (index, checked, notify) {
      var box = pickerAreas()[index];
      box.checked = checked;
      if (notify) box.onchange();
    },
    // Flip the picker's master row, as a tap does.
    togglePickerAll: function () {
      var master = document.getElementById('area-picker-all');
      master.checked = !master.checked;
      master.onchange();
    },
    pickerAreasChecked: function () {
      return Array.prototype.filter.call(pickerAreas(), function (c) { return c.checked; }).length;
    },
    // Make the cinemas in `off` seasonal and out of season, those in `on` seasonal and in it —
    // May..September, as the server's `window.CINEMA_SEASONS` would carry them.
    seasons: function (off, on) {
      var seasons = {};
      off.forEach(function (c) { seasons[c] = { opens: 5, closes: 9, offSeason: true }; });
      (on || []).forEach(function (c) { seasons[c] = { opens: 5, closes: 9, offSeason: false }; });
      window.CINEMA_SEASONS = seasons;
    },
    // The cinema rows' names, top to bottom, in `root` (the whole panel by default).
    rowOrder: function (root) {
      return all('input[data-cinema]', root || document.getElementById('cinema-list')).map(function (c) { return c.dataset.cinema; });
    },
    // The muted second line under the row of `name`, or '' when it has none.
    seasonLine: function (name) {
      var box = document.querySelector('#cinema-list input[data-cinema="' + CSS.escape(name) + '"]');
      var line = box.parentElement.querySelector('.cinema-season');
      return line ? line.textContent : '';
    },
    // A cinema `name` with no films, out of season, and the only one the filter leaves on.
    onlyOffSeason: function (name) {
      ALL_CINEMAS.push(name);
      window.__filtry.seasons([name]);
      localStorage.setItem('disabledCinemas', JSON.stringify(ALL_CINEMAS.filter(function (c) { return c !== name; })));
      applyFilters();
    },
    // What `disabledCinemas` holds.
    disabled: function () { return JSON.parse(localStorage.getItem('disabledCinemas') || '[]'); },
    // Untick the first cinema row if it is ticked; its label, or '' if it was not.
    uncheckFirstCinema: function () {
      var box = document.querySelector('#cinema-list input[type="checkbox"]');
      if (!box.checked) return '';
      box.click();
      return box.parentElement.textContent.trim();
    },
    // The first cinema row is off and every other is on.
    onlyFirstCinemaOff: function () {
      var boxes = all('#cinema-list input[type="checkbox"]');
      return !boxes[0].checked && boxes.slice(1).every(function (c) { return c.checked; });
    },
    // The submenu `list`'s entry `value` is ticked.
    isChecked: function (list, value) {
      return entries(list).find(function (c) { return c.value === value; }).checked;
    },
    // Some entry of the submenu `list` other than `value` is ticked.
    othersChecked: function (list, value) {
      return entries(list).some(function (c) { return c.value !== value && c.checked; });
    },
    // Untick every room but `pair` (cinema|room) and run a filter pass.
    onlyRoom: function (pair) {
      entries('#room-list').forEach(function (c) { if (c.value !== pair) c.click(); });
      applyFilters();
    },
    // Every visible badge is in `room` at `cinema`.
    badgesAllIn: function (cinema, room) {
      return all('.badge-time').filter(function (b) { return b.style.display !== 'none'; }).every(function (b) {
        var group = b.closest('.cinema-group');
        return b.dataset.room === room && (group && group.dataset.cinema) === cinema;
      });
    },
    // The room names of the first cinema holding both "Sala 9" and "Sala 10",
    // pipe-joined in panel order; '' if none does.
    roomsPastNine: function () {
      var headers = all('#room-list .room-cinema-header');
      for (var i = 0; i < headers.length; i++) {
        var rooms = all('input[type="checkbox"]', headers[i].nextElementSibling)
          .map(function (c) { return c.value.split('|')[1]; });
        if (rooms.includes('Sala 9') && rooms.includes('Sala 10')) return rooms.join('|');
      }
      return '';
    },
    // Open the first cinema with two or more rooms and untick its first room;
    // how many rooms it has, or 0 if none has two.
    uncheckARoom: function () {
      var headers = all('#room-list .room-cinema-header');
      for (var i = 0; i < headers.length; i++) {
        var boxes = headers[i].nextElementSibling.querySelectorAll('input[type="checkbox"]');
        if (boxes.length >= 2) { headers[i].click(); boxes[0].click(); return boxes.length; }
      }
      return 0;
    },
    // The first cinema header's room-count badge, or '' while it is hidden.
    firstRoomBadge: function () {
      var count = document.querySelector('#room-list .room-cinema-header .room-cinema-count');
      return count.style.display === 'none' ? '' : count.textContent;
    },
  };
})();
