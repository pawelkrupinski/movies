// Posters: retrying a broken one with backoff, and the detail page's full-screen view.

/* global closeDialog, openDialog */  // overlays.js
/* exported schedulePosterRetry */

// ── Poster retry with exponential backoff ──────────────────────────────
//
// Mirrors the iOS PosterImage retry loop (RetryBackoff.swift). When all
// fallback URLs are exhausted the img is hidden and a backoff timer
// schedules a fresh attempt of the entire chain (primary + fallbacks).
// Sequence: 2s, 6s, 18s, 54s, 162s (then 162s forever) — multiplier 3,
// same as iOS. A `_kinowo_t=<gen>` cache-buster is appended on retries
// so the browser (and any upstream CDN / weserv cache) doesn't serve a
// stale failure. Returning to the tab (visibilitychange → visible)
// resets the backoff cycle, matching iOS's scenePhase → .active reset.

const _POSTER_RETRY_MAX = 4;

function _posterDelay(attempt) {
  var clampedAttempt = Math.max(0, Math.min(attempt, _POSTER_RETRY_MAX));
  var delay = 2;
  for (var i = 0; i < clampedAttempt; i++) delay *= 3;
  return delay;
}

function _posterCacheBust(url, gen) {
  if (gen === 0) return url;
  var sep = url.indexOf('?') === -1 ? '?' : '&';
  return url + sep + '_kinowo_t=' + gen;
}

function schedulePosterRetry(img) {
  var attempt = parseInt(img.dataset.retryAttempt || '0', 10);
  var delay = _posterDelay(attempt);
  img.dataset.retryAttempt = String(attempt + 1);
  img._posterTimer = setTimeout(function() { restartPosterChain(img); }, delay * 1000);
}

function restartPosterChain(img) {
  var gen = (parseInt(img.dataset.retryGen || '0', 10)) + 1;
  img.dataset.retryGen = String(gen);
  // The chain in data-fallbacks is never consumed — the inline onerror
  // walks it through the data-fallback-index cursor — so rewinding is
  // just resetting the cursor.
  delete img.dataset.fallbackIndex;
  img.style.display = '';
  img.nextElementSibling.style.display = 'none';
  img.src = _posterCacheBust(img.dataset.originalSrc, gen);
}

function cancelPosterRetry(img) {
  if (img._posterTimer) { clearTimeout(img._posterTimer); img._posterTimer = null; }
}

document.addEventListener('visibilitychange', function() {
  if (document.visibilityState !== 'visible') return;
  document.querySelectorAll('img[data-retry-attempt]').forEach(function(img) {
    cancelPosterRetry(img);
    img.dataset.retryAttempt = '0';
    restartPosterChain(img);
  });
});

// ── Full-screen poster (detail page) ──────────────────────────────────────
//
// A click on the detail-page poster opens `#poster-lightbox`
// (`_filmDetailContent`), a modal dialog: a click anywhere on it — the ✕ or
// the backdrop or the image itself — and Escape close it. Delegated, so it
// needs no boot hook. The shown URL is the poster's `currentSrc`: the one
// that actually loaded, fallback included.

function openPosterLightbox(poster) {
  const box = document.getElementById('poster-lightbox');
  if (!box) return;
  box.querySelector('img').src = poster.currentSrc || poster.src;
  openDialog('poster-lightbox');
}

document.addEventListener('click', e => {
  if (!(e.target instanceof Element)) return;
  if (e.target.closest('#poster-lightbox')) { closeDialog('poster-lightbox'); return; }
  const poster = e.target.closest('.poster-img');
  if (poster) openPosterLightbox(poster);
});
