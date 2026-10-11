// What `ReviewDesignParitySpec` compares between the local review page and the
// published one, as `window.__styles`.
(function () {
  window.__styles = {
    // The computed `properties` of the first element matching `selector`, or null if none does.
    of: function (selector, properties) {
      var element = document.querySelector(selector);
      if (!element) return null;
      var style = getComputedStyle(element);
      var picked = {};
      properties.forEach(function (p) { picked[p] = style[p]; });
      return picked;
    },
    // The font families the page has loaded a face of, once its fonts settle.
    loadedFamilies: function () {
      return document.fonts.ready.then(function () {
        return Array.from(document.fonts).filter(function (f) { return f.status === 'loaded'; })
          .map(function (f) { return f.family.replace(/"/g, ''); });
      });
    },
  };
})();
