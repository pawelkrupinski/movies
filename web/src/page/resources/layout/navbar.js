// The listing navbar's layout, measured and staged for `PageJsBehaviourSpec`'s
// mobile and desktop sweeps, as `window.__navbar`.
/* global onFormatChange, updateFormatBtn, applySearchKeyboardInset */
(function () {
  // The navbar's children that occupy a row: no zero-size slot (an empty
  // anonymous auth), and — when `skipFixed` — not the floating search pill,
  // which portrait lifts out of the flow as a bottom overlay.
  function rowMembers(skipFixed) {
    return Array.prototype.filter.call(document.querySelector('.navbar').children, function (c) {
      var r = c.getBoundingClientRect();
      if (r.width === 0 || r.height === 0) return false;
      return !(skipFixed && getComputedStyle(c).position === 'fixed');
    });
  }

  function intoAuthSlot(element) {
    var slot = document.querySelector('.navbar-auth');
    if (slot && !slot.children.length) slot.appendChild(element);
  }

  window.__navbar = {
    // How many rows the navbar's members sit on, by distinct rounded top.
    rows: function (skipFixed) {
      var tops = new Set();
      rowMembers(skipFixed).forEach(function (c) { tops.add(Math.round(c.getBoundingClientRect().top)); });
      return tops.size;
    },
    // How far, in whole px, the furthest member reaches past the navbar's right edge.
    overflow: function () {
      var navRight = document.querySelector('.navbar').getBoundingClientRect().right;
      var maxOver = 0;
      rowMembers(true).forEach(function (c) {
        var over = Math.ceil(c.getBoundingClientRect().right - navRight);
        if (over > maxOver) maxOver = over;
      });
      return maxOver;
    },
    // The browser's evaluated `--mobile-scale`: a probe `calc(1000px * scale)`
    // wide, read back and divided. Falls back to an in-flow probe on the Chrome
    // builds that resolve the fixed, hidden one to 0.
    mobileScale: function () {
      var inProbe = function (parent, extraStyle) {
        var d = document.createElement('div');
        d.style.cssText = 'width:calc(1000px*var(--mobile-scale));' + extraStyle;
        parent.appendChild(d);
        var w = d.clientWidth;
        parent.removeChild(d);
        return w;
      };
      var w = inProbe(document.body, 'position:fixed;visibility:hidden;top:0;left:0');
      if (!w) w = inProbe(document.body, 'display:block');
      return w / 1000;
    },
    // Fill an empty auth slot with the signed-in avatar pill `buildAuthMenu` hydrates.
    addAvatarPill: function () {
      var pill = document.createElement('div');
      pill.className = 'auth-menu';
      pill.id = 'auth-menu';
      pill.innerHTML = '<span class="auth-avatar-fallback">A</span><span class="auth-name">Anonim</span>';
      intoAuthSlot(pill);
    },
    // Fill an empty auth slot with the signed-out "Zaloguj" button.
    addLoginButton: function () {
      var button = document.createElement('button');
      button.type = 'button';
      button.className = 'nav-tab nav-tab-login';
      button.textContent = 'Zaloguj';
      intoAuthSlot(button);
    },
    // A `.auth-menu` whose inline `onclick` is exactly the navbar's, clicked for
    // real: whether it opened.
    clickInlineAuthMenu: function () {
      var menu = document.createElement('div');
      menu.id = 'auth-menu';
      menu.className = 'auth-menu';
      menu.setAttribute('onclick', 'toggleAuthMenu(event)');
      document.body.appendChild(menu);
      menu.click();
      return menu.classList.contains('open');
    },
    // Set every Filtry axis the navbar can carry: 2D, napisy, IMAX, from 18:00.
    setEveryFilter: function () {
      document.querySelector('input[name="format-dim"][value="2D"]').click();
      document.querySelector('input[name="format-lang"][value="NAP"]').click();
      document.getElementById('format-imax').click();
      document.getElementById('from-hour').value = '18';
      onFormatChange();
      updateFormatBtn();
    },
    // An on-screen keyboard taking `inset` px off the visual viewport, the search
    // field focused, and the page's keyboard tracker run.
    openKeyboard: function (inset) {
      Object.defineProperty(window, 'visualViewport', { configurable: true, value: {
        height: window.innerHeight - inset, offsetTop: 0,
        addEventListener: function () {}, removeEventListener: function () {},
      } });
      document.getElementById('search-input').focus();
      applySearchKeyboardInset();
    },
    // The real visual viewport back, the inset cleared, the field blurred.
    closeKeyboard: function () {
      delete window.visualViewport;
      document.documentElement.style.removeProperty('--keyboard-inset');
      document.getElementById('search-input').blur();
    },
  };
})();
