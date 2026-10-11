// What `tools.OgCardGenerator` asks of a live city page before screenshotting it
// for its share card, as `window.__ogCard`. Only the posters intersecting the
// viewport matter — that is all the screenshot captures; the lazy ones below the
// fold never load and would hang a wait on them.
(function () {
  function posters() { return Array.prototype.slice.call(document.querySelectorAll('img[data-original-src]')); }
  function inViewport(img) {
    var r = img.getBoundingClientRect();
    return r.bottom > 0 && r.top < window.innerHeight && r.right > 0 && r.left < window.innerWidth;
  }

  window.__ogCard = {
    // Every in-viewport poster has loaded (complete, with a size) or been hidden
    // by its `onerror` fallback chain.
    postersReady: function () {
      return posters().filter(inViewport).every(function (i) { return (i.complete && i.naturalWidth > 0) || i.offsetParent === null; });
    },
    // Resolves true once every shown in-viewport poster's decode has finished
    // and two more frames have painted, or false after `timeoutMs`. A rejected
    // decode is a broken image, which the `onerror` chain hides, so it counts as done.
    postersPainted: function (timeoutMs) {
      var images = posters().filter(function (i) { return i.offsetParent !== null && inViewport(i); });
      var decoded = Promise.all(images.map(function (i) { return i.decode().catch(function () {}); }))
        .then(function () {
          return new Promise(function (resolve) {
            requestAnimationFrame(function () { requestAnimationFrame(function () { resolve(true); }); });
          });
        });
      var timeout = new Promise(function (resolve) { setTimeout(function () { resolve(false); }, timeoutMs); });
      return Promise.race([decoded, timeout]);
    },
    // Every shown poster's on-screen box, clipped to the viewport, with the
    // background its empty box shows — the nearest ancestor's non-transparent one —
    // as [x, y, width, height, r, g, b] rows. A poster with no such background is skipped.
    posterBoxes: function () {
      return posters().filter(function (i) { return i.offsetParent !== null; }).map(function (i) {
        var r = i.getBoundingClientRect();
        var x = Math.max(0, r.left);
        var y = Math.max(0, r.top);
        var w = Math.min(window.innerWidth, r.right) - x;
        var h = Math.min(window.innerHeight, r.bottom) - y;
        var element = i.parentElement;
        var rgb = null;
        while (element && !rgb) {
          var c = getComputedStyle(element).backgroundColor.match(/[0-9.]+/g);
          if (c && (c.length < 4 || +c[3] > 0)) rgb = c;
          element = element.parentElement;
        }
        return rgb && w > 0 && h > 0 ? [Math.round(x), Math.round(y), Math.round(w), Math.round(h), +rgb[0], +rgb[1], +rgb[2]] : null;
      }).filter(function (b) { return b !== null; });
    },
    // Click the area picker's button if a split city's picker is up; a no-op otherwise.
    dismissAreaPicker: function () {
      var button = document.querySelector('#area-picker-overlay button');
      if (button) button.click();
    },
  };
})();
