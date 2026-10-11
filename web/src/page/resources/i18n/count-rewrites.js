// Installed before any page script by `PageJsBehaviourSpec`'s language tests:
// counts, from the document's first byte, every time a `[data-i18n]` element's
// text is rewritten (its old child nodes removed), into `window.__i18nRewrites`.
(function () {
  window.__i18nRewrites = 0;
  new MutationObserver(function (records) {
    records.forEach(function (r) {
      if (r.type === 'childList' && r.removedNodes.length && r.target.hasAttribute && r.target.hasAttribute('data-i18n'))
        window.__i18nRewrites++;
    });
  }).observe(document, { childList: true, subtree: true });
})();
