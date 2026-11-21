// The image-fetch uptime tracker: every poster's load or failure, batched to the uptime page.

/* global mountPrefix */  // city.js

// ── Image-fetch uptime tracker ────────────────────────────────────────────
//
// Captures browser-side img load/error outcomes and batches them to
// /uptime/img-event so the uptime page can show per-origin reliability bars.
// Event listeners are registered in the capture phase because `load` /
// `error` don't bubble on <img>.
//
// Three things each event carries, all learned the hard way:
//
//   host      the ORIGIN CDN, with the `images.weserv.nl` proxy unwrapped.
//             Posting the proxy's own host merged image.tmdb.org,
//             m.media-amazon.com and de.web.img3.acsta.net into a single
//             row, so a total Amazon outage sat invisible behind TMDB's
//             healthy traffic until it was found by probing prod by hand.
//   fallback  whether this was an attempt at a non-primary poster URL.
//             A failed primary is a poster the visitor watched break; a
//             failed fallback is only a dead spare behind a poster that may
//             have rendered fine. Merged, either can mask the other.
//   proxied   whether weserv served this image, so the proxy keeps a row of
//             its own beside the origins' — see probeProxyFault below for
//             how that row earns a FAILURE, which is the harder half.
(function() {
  var pending = [];
  var FLUSH_INTERVAL_MS = 10000;
  var BATCH_SIZE_TRIGGER = 50;
  var PROXY_HOST = 'images.weserv.nl';   // tools.PosterProxy.ProxyHost

  // A weserv outage breaks EVERY poster on the page at once, and the onerror
  // chain then walks each one's spares — so an uncapped "re-fetch it direct"
  // probe would double a broken page's image traffic. A handful of probes is
  // plenty to tell a proxy fault from an origin fault.
  var MAX_PROXY_PROBES = 20;
  var proxyProbes = 0;

  // The CDN that actually holds the image: the proxy's target when weserv
  // fronted it, the URL's own host when PosterProxy skipped the proxy
  // (multikino, m.media-amazon.com, acsta.net all arrive unwrapped).
  function originHost(src) {
    try { return new URL(directUrl(src) || src, window.location.href).host || 'unknown'; }
    catch (e) { return 'unknown'; }
  }

  // A weserv URL with no inner `url=` is still a weserv request worth
  // counting against the proxy's row, so this asks the outer host alone —
  // `directUrl` is the one that needs a target to unwrap.
  function isProxied(src) {
    try { return new URL(src, window.location.href).host === PROXY_HOST; }
    catch (e) { return false; }
  }

  // The origin URL behind a proxied `src`, or '' when it wasn't proxied (or
  // carries no `url=`). PosterProxy hands weserv an https target whole and
  // an http one scheme-less (`kinobulgarska19.pl/x.jpg`); put https:// on a
  // scheme-less one before we fetch it ourselves.
  function directUrl(src) {
    if (!isProxied(src)) return '';
    try {
      var inner = new URL(src, window.location.href).searchParams.get('url');
      if (!inner) return '';
      return /^https?:\/\//i.test(inner) ? inner : 'https://' + inner;
    } catch (e) { return ''; }
  }

  // A failed <img> carries no status code, so a broken proxied poster is as
  // likely to be the origin's fault as the proxy's — and charging it to the
  // origin is exactly how weserv's own failures have hidden before (its edge
  // refusing a domain by policy; an origin 403ing weserv's IP, which weserv
  // returns as a 404 of its own). Settle it the way the July 2026 hunt did by
  // hand: re-request the same image STRAIGHT FROM THE ORIGIN. If that loads,
  // the proxy broke a poster the origin was willing to serve, which is the
  // one outcome attributable to the proxy — post it against the proxy's host.
  // If the direct fetch fails too, the origin's row already carries the
  // failure and the proxy's row stays out of it rather than guessing.
  function probeProxyFault(src) {
    var direct = directUrl(src);
    if (!direct || proxyProbes >= MAX_PROXY_PROBES) return;
    proxyProbes++;
    // Never appended to the document, so the probe's own load/error can't
    // reach the capture-phase listeners below — it would otherwise be
    // recorded as ordinary traffic and probe itself in turn.
    var probe = new Image();
    probe.onload = function() {
      pending.push({
        host: PROXY_HOST,
        success: false,
        error: 'proxy failed, origin served it (' + direct.substring(0, 120) + ')'
      });
    };
    probe.src = direct;
  }

  // A poster carries its primary URL in data-original-src; anything else in
  // the src is a fallback the onerror chain swapped in. The retry loop
  // re-requests the SAME primary with a `_kinowo_t` cache-buster appended,
  // so strip that before comparing or every retry would look like a
  // fallback. Images with no data-original-src (site chrome, icons) have no
  // primary to differ from, so they are never fallbacks.
  function isFallback(img) {
    var primary = img.dataset ? img.dataset.originalSrc : null;
    if (!primary) return false;
    var current = img.getAttribute('src') || '';
    return stripCacheBust(current) !== stripCacheBust(primary);
  }

  function stripCacheBust(url) {
    return url.replace(/[?&]_kinowo_t=\d+/, '');
  }

  function record(target, success) {
    if (!target || target.tagName !== 'IMG') return;
    var src = target.currentSrc || target.src;
    if (!src) return;
    var ev = { host: originHost(src), success: success };
    if (isProxied(src)) ev.proxied = true;
    if (isFallback(target)) ev.fallback = true;
    if (!success) {
      ev.error = 'img load failed (' + src.substring(0, 120) + ')';
      probeProxyFault(src);
    }
    pending.push(ev);
    if (pending.length >= BATCH_SIZE_TRIGGER) flush();
  }

  function flush() {
    if (pending.length === 0) return;
    var body = JSON.stringify({ events: pending });
    pending = [];
    try {
      if (navigator.sendBeacon) {
        navigator.sendBeacon(mountPrefix() + '/uptime/img-event', new Blob([body], { type: 'application/json' }));
      } else {
        fetch(mountPrefix() + '/uptime/img-event', { method: 'POST', body: body, headers: { 'Content-Type': 'application/json' }, keepalive: true });
      }
    } catch (e) { /* tracker must never throw into page code */ }
  }

  document.addEventListener('load',  function(ev) { record(ev.target, true);  }, true);
  document.addEventListener('error', function(ev) { record(ev.target, false); }, true);
  setInterval(flush, FLUSH_INTERVAL_MS);
  window.addEventListener('pagehide', flush);

  // Test seams for PageJsBehaviourSpec, same shape as _posterDelay /
  // _posterCacheBust: assert the classification directly, and read the
  // queued batch without having to intercept sendBeacon and decode a Blob.
  window._imgOriginHost = originHost;
  window._imgIsFallback = isFallback;
  window._imgDirectUrl = directUrl;
  window._imgProbeProxyFault = probeProxyFault;
  // Probes left before the cap. Read synchronously, so a test can assert the
  // cap holds without waiting on 20 image loads — and without assuming the
  // page under test spent none of the budget on its own broken posters.
  window._imgProbeBudget = function() { return MAX_PROXY_PROBES - proxyProbes; };
  // The page's own posters spend the budget as they break, which on a fixture
  // page can exhaust it before a test provokes anything. Hand the budget back
  // so a test starts from a known one.
  window._resetImgProbeBudget = function() { proxyProbes = 0; };
  window._drainImgEvents = function() { var drained = pending; pending = []; return drained; };
  // Peek without draining, so a test can poll for the direct-origin probe's
  // asynchronous verdict and still read the batch it lands in.
  window._peekImgEvents = function() { return pending; };
})();
