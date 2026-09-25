/**
 * The browser side of a page's live state: first render from the snapshot embedded in the page,
 * then every snapshot the server pushes over `/events/<page>`.
 *
 * THIS REPLACES THE CLOCK-BASED STALE-TAB GUARD. A tab no longer has an age to guess at: while the
 * stream is connected it shows the server's current knowledge, and when the stream drops the page
 * says so and marks `body[data-live=false]`, which disables every `.needs-live` control (deploy,
 * activate) until it reconnects. The server-side basis check on each action is the hard guard; this
 * is what keeps the page honest in between.
 */
export interface Snapshot<T> {
  readonly version: number;
  readonly changedAt: number;
  readonly checkedAt: number;
  readonly state: T;
}

export function connect<T>(page: string, onSnapshot: (snapshot: Snapshot<T>) => void): void {
  const indicator = document.getElementById("live");
  const initial = document.getElementById("initial")?.textContent;
  let current: Snapshot<T> | null = initial ? (JSON.parse(initial) as Snapshot<T>) : null;
  const setLive = (live: boolean) => {
    document.body.dataset.live = String(live);
    if (!indicator) return;
    indicator.className = `live ${live ? "on" : "off"}`;
    indicator.textContent = live ? liveLabel(current) : "disconnected — reconnecting…";
  };
  const source = new EventSource(`/events/${page}`);
  source.addEventListener("open", () => setLive(true));
  source.addEventListener("error", () => setLive(false));
  source.addEventListener("snapshot", (event) => {
    const next = JSON.parse((event as MessageEvent<string>).data) as Snapshot<T>;
    const changed = !current || next.version !== current.version;
    current = next;
    setLive(true);
    if (changed) onSnapshot(next);
  });
  setInterval(() => {
    if (document.body.dataset.live === "true" && indicator) indicator.textContent = liveLabel(current);
  }, 5_000);
}

function liveLabel(snapshot: Snapshot<unknown> | null): string {
  if (!snapshot) return "live";
  return `live · last change ${ago(Date.now() - snapshot.changedAt)} ago`;
}

export function ago(ms: number): string {
  const seconds = Math.max(0, Math.round(ms / 1000));
  if (seconds < 60) return `${seconds}s`;
  if (seconds < 3600) return `${Math.floor(seconds / 60)}m`;
  if (seconds < 86_400) return `${Math.floor(seconds / 3600)}h`;
  return `${Math.floor(seconds / 86_400)}d`;
}
