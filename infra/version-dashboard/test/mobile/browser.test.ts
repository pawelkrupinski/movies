// @vitest-environment jsdom
import { afterEach, beforeAll, beforeEach, describe, expect, it } from "vitest";
import { emptyMobileState } from "../../src/mobile/model.js";
import { renderMobile } from "../../src/mobile/view.js";
import { setDeps, start } from "../../web/mobile.js";
import { NOW, state, upToDate } from "./states.js";

/** A stand-in EventSource the test can push snapshots through. */
class FakeEventSource {
  static last: FakeEventSource | null = null;
  readonly listeners = new Map<string, ((event: MessageEvent<string>) => void)[]>();
  constructor(readonly url: string) {
    FakeEventSource.last = this;
  }
  addEventListener(type: string, listener: (event: MessageEvent<string>) => void): void {
    this.listeners.set(type, [...(this.listeners.get(type) ?? []), listener]);
  }
  push(snapshot: unknown): void {
    for (const listener of this.listeners.get("snapshot") ?? []) listener({ data: JSON.stringify(snapshot) } as MessageEvent<string>);
  }
}

const posts: string[] = [];

const first = () => ({ version: 1, changedAt: NOW, checkedAt: NOW, state: emptyMobileState() });
const page = () => `<span id=live></span><main id=app>${renderMobile(first().state, NOW).__raw}</main><script id=initial type="application/json">${JSON.stringify(first())}</script>`;

// Started ONCE, as the page is: its click listener lives on the document for the page's lifetime.
beforeAll(() => {
  (globalThis as unknown as { EventSource: unknown }).EventSource = FakeEventSource;
  document.body.innerHTML = page();
  setDeps({
    now: () => NOW,
    fetch: async (url, init) => {
      posts.push(`${init?.method ?? "GET"} ${url}`);
      return { json: async () => ({ ok: true }) } as Response;
    },
  });
  start();
});
beforeEach(() => {
  document.body.innerHTML = page();
  posts.length = 0;
});
afterEach(() => {
  document.body.innerHTML = "";
});
describe("the mobile page in a browser", () => {
  it("re-renders from each pushed snapshot with the server's own view", () => {
    expect(document.getElementById("app")?.textContent).toContain("for the first time");
    FakeEventSource.last?.push({ version: 2, changedAt: NOW, checkedAt: NOW, state: state([upToDate("Android", "2.0.9")]) });
    const app = document.getElementById("app");
    expect(app?.textContent).not.toContain("for the first time");
    expect(app?.querySelector("h2")?.textContent).toBe("Android");
    expect(app?.innerHTML).toContain("<b>2.0.9</b>");
    expect(FakeEventSource.last?.url).toBe("/events/mobile");
  });

  it("posts Refresh and waits for the snapshot rather than reloading", () => {
    document.querySelector<HTMLButtonElement>("button[data-action=refresh]")?.click();
    expect(posts).toEqual(["POST /mobile/refresh"]);
    expect(document.getElementById("refreshnote")?.textContent).toBe("rebuilding…");
  });
});
