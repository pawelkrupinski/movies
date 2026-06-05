// Read-only display-geometry readout for the dev `/debug/tune*` panels. It
// leads with the *available* viewport (CSS px) that actually drives layout —
// more useful than the raw panel resolution for dialling in responsive sizing —
// then the device-pixel ratio and the physical pixel size it works out to, plus
// the screen size.
//
// Pure: the browser panel and the Playwright/node test require the SAME
// function, so the rendered readout can't drift from what's tested.
(function (root) {
  // `2` → `2`, `2.5` → `2.5` — drop a trailing `.0`, keep real fractions.
  function ratioText(dpr) {
    return Number.isInteger(dpr) ? String(dpr) : String(Math.round(dpr * 1000) / 1000);
  }

  function formatViewportInfo(d) {
    const px = Math.round(d.innerWidth * d.dpr);
    const py = Math.round(d.innerHeight * d.dpr);
    return `viewport ${d.innerWidth}×${d.innerHeight} px · ${ratioText(d.dpr)}× → ${px}×${py} px · ekran ${d.screenW}×${d.screenH}`;
  }

  const api = { formatViewportInfo, ratioText };
  if (typeof module !== 'undefined' && module.exports) module.exports = api;
  if (root) root.ViewportInfo = api;
})(typeof window !== 'undefined' ? window : null);
