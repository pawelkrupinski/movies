// `CdpWaitForSpec`'s renderer-freeze probe, as `window.__freezeWatch`.
(function () {
  window.__freezeWatch = {
    // Tick every 20 ms; once a gap between ticks shows the renderer was frozen
    // for over a second, set `window.settled` 200 ms of page time later — so it
    // is false for the whole freeze and true shortly after, whatever order the
    // resumed renderer runs things in.
    settleAfterFreeze: function () {
      var last = performance.now();
      var tick = setInterval(function () {
        var now = performance.now();
        if (now - last > 1000) {
          clearInterval(tick);
          setTimeout(function () { window.settled = true; }, 200);
        }
        last = now;
      }, 20);
    },
  };
})();
