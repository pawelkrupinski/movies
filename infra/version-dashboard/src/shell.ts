import { escapeHtml } from "./html.js";
import type { Page } from "./page.js";

/**
 * One page's document. A NAV ACROSS EVERY PAGE, so this stays discoverable as one dashboard rather
 * than unrelated pages that happen to share a port.
 */
export function shell(page: Page<unknown>, pages: readonly Page<unknown>[]): string {
  const snapshot = page.store.get();
  const nav = pages
    .map((other) => `<a href="${other.path}"${other === page ? " class=current" : ""}>${escapeHtml(other.title)}</a>`)
    .join("");
  // The snapshot rides along so the browser's first render is the server's, not a second fetch.
  const initial = JSON.stringify(snapshot).replaceAll("<", "\\u003c");
  return `<!doctype html><html lang=en><head><meta charset=utf-8>
<meta name=viewport content="width=device-width,initial-scale=1">
<title>kinowo — ${escapeHtml(page.title)}</title><link rel=stylesheet href="/assets/styles.css"></head>
<body data-page="${page.name}"><nav class=pages>${nav}<span id=live class=live></span></nav>
<main id=app>${page.renderBody(snapshot)}</main>
<script id=initial type="application/json">${initial}</script>
<script type=module src="/assets/${page.name}.js"></script></body></html>`;
}
