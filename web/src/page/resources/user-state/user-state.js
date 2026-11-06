// `PageJsBehaviourSpec`'s server-state tests (hidden films, language): what this
// document has asked the server, and fetch stubs standing in for the user-state
// endpoints where the fixture server's static answers cannot drive a case.
// Every stub keeps the real `fetch` for any other URL; `restore()` puts it back.
/* global getHidden, _writeHiddenFilms, bootMergeFromServer */
(function () {
  var realFetch = null;
  var realSetTimeout = null;

  // Route this document's fetches through `answer(url, opts)`; whatever it
  // returns nothing for goes to the real `fetch`.
  function stub(answer) {
    if (!realFetch) realFetch = window.fetch;
    window.fetch = function (url, opts) {
      return answer(String(url), opts) || realFetch(url, opts);
    };
  }

  function resources(fragment) {
    return performance.getEntriesByType('resource').filter(function (r) { return r.name.indexOf(fragment) !== -1; });
  }

  function isLanguagePush(url, opts) {
    return /\/api\/me\/state$/.test(url) && opts && opts.method === 'PUT';
  }

  var userState = {
    // Resource entries are per DOCUMENT and empty after a navigation, so these
    // answer "did the document now running ask?", which localStorage — shared
    // by every tab on the origin — cannot.
    requested: function (fragment) { return resources(fragment).length > 0; },
    requests: function (fragment) { return resources(fragment).length; },
    requestedMatching: function (pattern) {
      return performance.getEntriesByType('resource').some(function (r) { return new RegExp(pattern).test(r.name); });
    },
    requestsEndingWith: function (suffix) {
      return performance.getEntriesByType('resource')
        .filter(function (r) { return r.name.slice(-suffix.length) === suffix; }).length;
    },

    // Re-fetch this page as served; `servedNamesVisitor` is then whether its
    // bytes carry the avatar menu or a logged-in flag (null until it lands).
    fetchServedPage: function () {
      userState.servedNamesVisitor = null;
      fetch(location.href).then(function (r) { return r.text(); }).then(function (t) {
        userState.servedNamesVisitor = t.indexOf('id="auth-menu"') >= 0 || t.indexOf('IS_LOGGED_IN') >= 0;
      });
    },

    // This country's reconcile has run, and `title` came out of it hidden.
    synced: function (title) {
      return localStorage.getItem('hiddenFilmsSynced:pl') === '1' && getHidden().indexOf(title) !== -1;
    },

    // Answer every per-title hidden-films write with `status`, then write
    // `title`; `written` once its answer is handled.
    writeAnswered: function (status, title) {
      stub(function (url) {
        if (/\/api\/me\/pl\/hidden-films\//.test(url)) return Promise.resolve(new Response('answer', { status: status }));
      });
      userState.write(title);
    },

    // PUT `title` through the page's own writer; `written` once it is done.
    write: function (title) {
      userState.written = false;
      _writeHiddenFilms('PUT', 'pl', title).then(function () { userState.written = true; });
    },

    // Run the boot reconcile again; `reconciled` once it is done.
    reconcile: function () {
      userState.reconciled = false;
      bootMergeFromServer().then(function () { userState.reconciled = true; });
    },

    // A stateful `/api/me/pl/hidden-films` server: a PUT adds its title and moves
    // the ETag; a GET answers 304 to the current ETag and the list otherwise.
    // With `holdFirstGet` the first GET is held until `releaseGet()`.
    serveHiddenFilms: function (list, holdFirstGet) {
      var server = userState.server = { list: list, version: 2 };
      userState.releaseGet = null;
      function answer(status, body) {
        return new Response(status === 304 ? null : JSON.stringify(body),
          { status: status, headers: { 'ETag': '"v' + server.version + '"', 'Content-Type': 'application/json' } });
      }
      stub(function (url, opts) {
        var method = (opts && opts.method) || 'GET';
        if (!/\/api\/me\/pl\/hidden-films/.test(url)) return null;
        if (method === 'PUT') {
          var title = decodeURIComponent(url.split('/').pop());
          server.list = [...new Set(server.list.concat([title]))];
          server.version++;
          return Promise.resolve(answer(200, { hiddenFilms: server.list }));
        }
        var inm = opts && opts.headers && opts.headers['If-None-Match'];
        var response = inm === '"v' + server.version + '"' ? answer(304) : answer(200, { hiddenFilms: server.list });
        if (holdFirstGet && !userState.releaseGet) {
          return new Promise(function (resolve) { userState.releaseGet = function () { resolve(response); }; });
        }
        return Promise.resolve(response);
      });
    },

    // Answer the language push with `status`; `pushHandled` once the page's own
    // handler for that answer has run.
    answerLanguagePush: function (status) {
      userState.pushHandled = false;
      stub(function (url, opts) {
        if (!isLanguagePush(url, opts)) return null;
        // A macrotask runs only once every microtask queued before it has — the
        // push's response handler included — so this flags "handled".
        setTimeout(function () { userState.pushHandled = true; }, 0);
        return Promise.resolve(new Response('answer', { status: status }));
      });
    },

    // Record each language push into `languagePuts`, counting how many are out
    // at once; the first is held until `releaseFirstPut()`.
    holdFirstLanguagePush: function () {
      userState.languagePuts = [];
      userState.languageInFlight = 0;
      userState.languageMaxInFlight = 0;
      userState.releaseFirstPut = null;
      stub(function (url, opts) {
        if (!isLanguagePush(url, opts)) return null;
        userState.languagePuts.push(JSON.parse(opts.body).language);
        userState.languageInFlight++;
        userState.languageMaxInFlight = Math.max(userState.languageMaxInFlight, userState.languageInFlight);
        var answer = function () { userState.languageInFlight--; return new Response(null, { status: 204 }); };
        if (userState.languagePuts.length === 1) {
          return new Promise(function (resolve) { userState.releaseFirstPut = function () { resolve(answer()); }; });
        }
        return Promise.resolve(answer());
      });
    },

    // Record each language push into `languagePuts`; the first never answers
    // (only its abort), and the page's 10s push timeout fires after 50ms.
    hangFirstLanguagePush: function () {
      userState.languagePuts = [];
      if (!realSetTimeout) realSetTimeout = window.setTimeout;
      window.setTimeout = function (fn, ms) {
        return realSetTimeout.apply(window, [fn, ms === 10000 ? 50 : ms].concat([].slice.call(arguments, 2)));
      };
      stub(function (url, opts) {
        if (!isLanguagePush(url, opts)) return null;
        userState.languagePuts.push(JSON.parse(opts.body).language);
        if (userState.languagePuts.length === 1) {
          return new Promise(function (_resolve, reject) {
            if (opts.signal) opts.signal.addEventListener('abort', function () { reject(new DOMException('timed out', 'AbortError')); });
          });
        }
        return Promise.resolve(new Response(null, { status: 204 }));
      });
    },

    // Answer every fetch, whatever its URL, with a bare `{ status }` — or, with
    // no status, fail it as a network error does.
    answerEveryFetch: function (status) {
      stub(function () {
        return status === undefined ? Promise.reject(new Error('offline')) : Promise.resolve({ status: status });
      });
    },

    // Put back the real `fetch` and `setTimeout`.
    restore: function () {
      if (realFetch) { window.fetch = realFetch; realFetch = null; }
      if (realSetTimeout) { window.setTimeout = realSetTimeout; realSetTimeout = null; }
    },
  };
  window.__userState = userState;

  // The state reads THIS document has made and the page has finished acting on: the boot's language reconcile reads
  // `/api/me/state`, then pushes a pick it finds pending. A test picking while that read is still out met that push
  // as one more than it made (Main #2201). A read counts as settled one macrotask after its answer and body are in,
  // so the reconcile's own continuation, a microtask, has run by then. Installed ahead of the page's scripts, so the
  // boot's read passes through it; it answers exactly as the real `fetch` does.
  var stateReads = { made: 0, settled: 0 };
  var pageFetch = window.fetch;
  window.fetch = function (url, opts) {
    var sent = pageFetch.apply(this, arguments);
    if (!/\/api\/me\/state$/.test(String(url)) || (opts && opts.method && opts.method !== 'GET')) return sent;
    stateReads.made++;
    var settle = function () { pageSetTimeout(function () { stateReads.settled++; }, 0); };
    sent.then(function (resp) {
      var json = resp.json.bind(resp);
      resp.json = function () { var body = json(); body.then(settle, settle); return body; };
      if (!resp.ok) settle();
    }, settle);
    return sent;
  };
  var pageSetTimeout = window.setTimeout;
  // Every state read this document made has been acted on, and there was at least one.
  userState.stateReadsSettled = function () { return stateReads.made > 0 && stateReads.settled === stateReads.made; };
})();
