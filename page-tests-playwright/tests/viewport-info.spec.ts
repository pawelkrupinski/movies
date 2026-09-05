import { test, expect } from '@playwright/test';

// The tune-panel viewport readout is a pure function shared by the browser and
// this test, so exercise it directly in node (no browser/server needed) — it
// can't drift from what `_tunePanel` renders.
const { formatViewportInfo } = require('../../web/src/main/assets/js/viewportInfo.js');

test.describe('tune-panel viewport readout', () => {
  test('reports viewport, ratio and physical px', () => {
    expect(
      formatViewportInfo({ innerWidth: 1280, innerHeight: 720, dpr: 2, screenW: 1512, screenH: 982 }),
    ).toBe('viewport 1280×720 px · 2× → 2560×1440 px · ekran 1512×982');
  });

  test('keeps a fractional device-pixel ratio', () => {
    expect(
      formatViewportInfo({ innerWidth: 390, innerHeight: 844, dpr: 2.625, screenW: 390, screenH: 844 }),
    ).toBe('viewport 390×844 px · 2.625× → 1024×2216 px · ekran 390×844');
  });
});
