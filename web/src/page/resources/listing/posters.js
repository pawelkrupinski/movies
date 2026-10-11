// Poster fallback / backoff and the image uptime tracker, driven by hand for
// `PageJsBehaviourSpec`, as `window.__posters`. Each builds its own detached or
// throwaway <img> so no real network failure runs underneath the assertion.
/* global cancelPosterRetry, schedulePosterRetry, restartPosterChain, _drainImgEvents,
   _imgIsFallback, _resetImgProbeBudget, _imgProbeProxyFault, _imgProbeBudget, _peekImgEvents */
(function () {
  // An <img> added to the page with `src` (and `originalSrc`, its primary),
  // that fires `eventType` and is removed again.
  function fire(eventType, src, originalSrc) {
    var img = document.createElement('img');
    if (originalSrc !== undefined) img.dataset.originalSrc = originalSrc;
    if (src !== undefined) img.setAttribute('src', src);
    document.body.appendChild(img);
    img.dispatchEvent(new Event(eventType, { bubbles: false }));
    img.remove();
  }

  // This test server's own favicon, behind the weserv proxy.
  function proxiedLocal(path) {
    return 'https://images.weserv.nl/?url=' + encodeURIComponent(location.origin + path);
  }

  window.__posters = {
    // Exhaust the first poster's fallback chain and hand it to the backoff
    // retry, after clearing whatever its own onerror cycle already did.
    exhaustFirst: function () {
      var img = document.querySelector('.poster-wrap img[data-original-src]');
      img.onerror = null;
      cancelPosterRetry(img);
      img.removeAttribute('data-retry-attempt');
      img.removeAttribute('data-fallbacks');
      img.style.display = 'none';
      img.nextElementSibling.style.display = 'flex';
      schedulePosterRetry(img);
    },
    cancelScheduledRetry: function () {
      cancelPosterRetry(document.querySelector('.poster-wrap img[data-retry-attempt]'));
    },
    // Walk a detached clone of a real poster through its compiled onerror, a
    // retry rewind and one more error; every step's state, space-joined.
    walkChain: function () {
      var real = document.querySelector('.poster-wrap img[data-fallbacks]');
      var img = real.cloneNode(false);
      var placeholder = document.createElement('div');
      var wrap = document.createElement('div');
      wrap.appendChild(img); wrap.appendChild(placeholder);
      var onError = img.onerror;
      img.onerror = null;
      ['data-retry-attempt', 'data-retry-gen', 'data-fallback-index'].forEach(function (a) { img.removeAttribute(a); });
      img.dataset.originalSrc = 'https://p.example/primary.jpg';
      img.dataset.fallbacks = 'https://a.example/1.jpg|https://b.example/2.jpg';
      img.setAttribute('src', img.dataset.originalSrc);
      var steps = [];
      onError.call(img); steps.push(img.getAttribute('src'));
      onError.call(img); steps.push(img.getAttribute('src'));
      onError.call(img); steps.push(img.style.display + '/' + placeholder.style.display + '/' + img.dataset.retryAttempt);
      cancelPosterRetry(img);
      restartPosterChain(img);
      steps.push(img.getAttribute('src') + '/' + (img.style.display || 'shown'));
      onError.call(img); steps.push(img.getAttribute('src'));
      steps.push(img.getAttribute('data-fallbacks'));
      return steps.join(' ');
    },
    // What the tracker queues for one image event, after discarding anything the
    // page itself queued.
    queuedFor: function (eventType, src, originalSrc) {
      _drainImgEvents();
      fire(eventType, src, originalSrc);
      return _drainImgEvents();
    },
    // What it queues for a loaded proxied poster and a loaded directly-served one.
    queuedForProxiedAndDirect: function () {
      _drainImgEvents();
      var load = function (src) { fire('load', src, src); };
      load('https://images.weserv.nl/?url=image.tmdb.org%2Fp%2Fa.jpg');
      load('https://www.multikino.pl/media/x.jpg');   // SkipHosts: never proxied
      return _drainImgEvents();
    },
    // `_imgIsFallback` for an <img> at `src` whose primary is `originalSrc`
    // (none given: an image that is not a poster at all).
    isFallback: function (src, originalSrc) {
      var img = document.createElement('img');
      img.setAttribute('src', src);
      if (originalSrc !== undefined) img.dataset.originalSrc = originalSrc;
      return _imgIsFallback(img);
    },
    // Probe the proxy's fault for this server's `path`, behind weserv, on a
    // fresh budget (the page's own broken posters spend it); `suffix` follows the URL.
    probe: function (path, suffix) {
      _resetImgProbeBudget();
      _imgProbeProxyFault(proxiedLocal(path) + (suffix || ''));
    },
    // A proxied poster of this server's favicon breaks, on a fresh budget.
    breakProxied: function () {
      _drainImgEvents();
      _resetImgProbeBudget();
      var src = proxiedLocal('/assets/img/favicon.png');
      fire('error', src, src);
    },
    // Fire five probes past the budget; what is left of it after.
    overspendProbes: function () {
      _resetImgProbeBudget();
      var budget = _imgProbeBudget();
      for (var i = 0; i < budget + 5; i++) {
        _imgProbeProxyFault(proxiedLocal('/assets/img/favicon.png') + '&n=' + i);
      }
      return _imgProbeBudget();
    },
    // The proxy verdicts queued so far whose error names `fragment`.
    proxyVerdicts: function (fragment) {
      return _peekImgEvents().filter(function (e) { return e.host === 'images.weserv.nl' && (e.error || '').includes(fragment); });
    },
    // The events queued so far for an origin host starting with `prefix`.
    queuedForHost: function (prefix) {
      return _peekImgEvents().filter(function (e) { return (e.host || '').startsWith(prefix); });
    },
  };
})();
