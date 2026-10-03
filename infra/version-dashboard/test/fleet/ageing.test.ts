// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from "vitest";
import { renderFleet, STALE_GRACE_SECONDS } from "../../src/fleet/view.js";
import { resetForTests, setDeps, start } from "../../web/fleet.js";
import { machine, NOW, stateOf } from "./fixtures.js";

/**
 * An open tab between snapshots. The server pushes only when the state CHANGES, and a Prometheus
 * read that keeps failing changes nothing -- so the "read … ago" ages and the stale banner have to
 * age on the page itself, from the state it already holds.
 */
class SilentEventSource {
  addEventListener(): void {}
}

afterEach(() => {
  vi.useRealTimers();
  resetForTests();
  document.body.innerHTML = "";
});

describe("the fleet page between snapshots", () => {
  it("raises the stale banner once the last good read ages past the grace, with no snapshot pushed", async () => {
    vi.useFakeTimers();
    (globalThis as unknown as { EventSource: unknown }).EventSource = SilentEventSource;
    let now = NOW;
    setDeps({ now: () => now });
    const initial = { version: 1, changedAt: NOW * 1000, checkedAt: NOW * 1000, state: stateOf([machine()]) };
    document.body.innerHTML = `<span id=live></span><main id=app>${renderFleet(initial.state, NOW).__raw}</main><script id=initial type="application/json">${JSON.stringify(initial)}</script>`;
    start();
    expect(document.getElementById("app")?.textContent).not.toContain("every read since has failed");

    now = NOW + STALE_GRACE_SECONDS + 60;
    await vi.advanceTimersByTimeAsync(30_000);
    expect(document.getElementById("app")?.textContent).toContain("every read since has failed");
  });
});
