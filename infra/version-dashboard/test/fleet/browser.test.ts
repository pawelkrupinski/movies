// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import type { MachineRow } from "../../src/fleet/model.js";
import { renderFleet } from "../../src/fleet/view.js";
import { applySnapshot, bringAllToLatest, fleetCheck, LANDING_WAIT_MS, resetForTests, setDeps } from "../../web/fleet.js";
import { machine, NOW, stateOf } from "./fixtures.js";

/**
 * The bulk run and the per-machine button, driven in a DOM. Everything the view tests can reach is
 * markup; the part most likely to be WRONG is the queue in bringAllToLatest -- whether it really
 * stops taking new machines after a failure, whether the production database's typed confirmation
 * is carried into the switch it authorises, and whether a machine that has switched stops being
 * counted. What is faked is the dashboard's endpoints, never the page or the script driving it.
 */
const closureOf = (host: string) => `/nix/store/${"f".repeat(32)}-nixos-system-${host}`;
const staged = (name: string, role = "k3s-worker") =>
  machine({ name, role, public: `10.0.0.${name.length}`, actionable: true, state: "staged, not activated", stateKey: "staged", severity: "warn" });

interface Plan {
  readonly check?: { exit?: number; can_switch?: string | null; lines?: string[] };
  readonly switch?: { exit?: number; result?: string | null };
  readonly reading?: { store_hash?: string };
}
interface Post { machine: string; phase: string; closure?: string; confirm?: string }

interface Options {
  readonly confirm?: boolean;
  readonly prompt?: (message: string) => string | null;
  /** Log reads of these jobs fail, as if this dashboard had restarted under them. */
  readonly unreachableJobs?: readonly string[];
  /** Records every /fleet-apply/machine read. */
  readonly readings?: string[];
}

function fakeDashboard(plan: Record<string, Plan>, dialogs: Options = {}) {
  const posts: Post[] = [];
  const jobs = new Map<string, { lines: string[]; exit: number; can_switch: string | null; result: string | null }>();
  setDeps({
    // Polls pass at once; the wait for a landing never runs out, so it ends only by landing.
    sleep: (ms) => (ms >= LANDING_WAIT_MS ? new Promise(() => {}) : Promise.resolve()),
    now: () => NOW,
    confirm: () => dialogs.confirm ?? true,
    prompt: (message) => (dialogs.prompt ?? (() => null))(message),
    alert: () => {},
    fetch: async (url, init) => {
      let body: unknown;
      if (url === "/fleet-apply") {
        const request = JSON.parse(init?.body as string) as Post;
        posts.push(request);
        const id = `j${posts.length}`;
        const spec = request.phase === "check" ? plan[request.machine]?.check : plan[request.machine]?.switch;
        jobs.set(id, {
          lines: [`· ${request.phase} ${request.machine}`],
          exit: spec?.exit ?? 0,
          can_switch: (spec as Plan["check"])?.can_switch ?? null,
          result: (spec as Plan["switch"])?.result ?? null,
        });
        body = { job: id, phase: request.phase };
      } else if (url.startsWith("/fleet-apply/log")) {
        const id = decodeURIComponent(/job=([^&]*)/.exec(url)?.[1] ?? "");
        if (dialogs.unreachableJobs?.includes(id)) throw new TypeError("Failed to fetch");
        const job = jobs.get(id);
        const from = Number(/from=(\d+)/.exec(url)?.[1] ?? 0);
        body = job ? { ...job, lines: job.lines.slice(from), done: true } : { error: "no such job" };
      } else if (url.startsWith("/fleet-apply/machine")) {
        const name = decodeURIComponent(/machine=([^&]*)/.exec(url)?.[1] ?? "");
        dialogs.readings?.push(name);
        body = { store_hash: "ffffffffffff", ...plan[name]?.reading };
      } else {
        body = { ok: true };
      }
      return { json: async () => body } as Response;
    },
  });
  return posts;
}

function render(rows: MachineRow[]): void {
  document.getElementById("app")!.innerHTML = renderFleet(stateOf(rows), NOW).__raw;
  applySnapshot(stateOf(rows));
}

/** What the server pushes once a switched machine's new closure has been scraped. */
const landedOn = (row: MachineRow): MachineRow => ({ ...row, closure: `${"f".repeat(32)}-nixos-system-${row.name}`, actionable: false, state: "current", stateKey: "current", severity: "ok" });

const text = (selector: string) => document.querySelector(selector)?.textContent ?? "";
const bulkButton = () => document.querySelector<HTMLButtonElement>("#fleetbulkbtn")!;
const flush = () => new Promise((wake) => setTimeout(wake, 0));

beforeEach(() => {
  document.body.innerHTML = "<main id=app></main>";
  resetForTests();
});
afterEach(() => resetForTests());

describe("the bulk run", () => {
  it("checks, then switches, every staged machine, and the count falls to zero once their rows confirm", async () => {
    const rows = [staged("k3s-worker-1"), staged("monitoring-1", "monitoring")];
    render(rows);
    const posts = fakeDashboard(Object.fromEntries(rows.map((row) => [row.name, { check: { can_switch: closureOf(row.name) }, switch: { result: "DONE" } }])));
    expect(bulkButton().textContent).toContain("(2)");
    await bringAllToLatest(bulkButton());
    expect(text("#fleetbulkcons .consfoot")).toBe("done — 2 machine(s) switched.");
    expect(posts.map((post) => `${post.machine}:${post.phase}`).sort()).toEqual([
      "k3s-worker-1:check", "k3s-worker-1:switch", "monitoring-1:check", "monitoring-1:switch",
    ]);
    applySnapshot(stateOf(rows.map(landedOn)));
    await flush();
    expect(bulkButton().textContent).toContain("(0)");
    // The run's console survived the re-render that dropped both machines' buttons.
    expect(text("#fleetbulkcons .out")).toContain("=== monitoring-1");
  });

  it("carries the production database's typed confirmation into the switch it authorises", async () => {
    const rows = [staged("mongo-1", "mongo"), staged("monitoring-1", "monitoring")];
    render(rows);
    const prompts: string[] = [];
    const posts = fakeDashboard(
      Object.fromEntries(rows.map((row) => [row.name, { check: { can_switch: closureOf(row.name) }, switch: { result: "DONE" } }])),
      { prompt: (message) => (prompts.push(message), "mongo-1") },
    );
    await bringAllToLatest(bulkButton());
    expect(text("#fleetbulkcons .consfoot")).toBe("done — 2 machine(s) switched.");
    expect(prompts).toHaveLength(1);
    expect(prompts[0]).toContain("PRODUCTION DATABASE");
    expect(posts.find((post) => post.machine === "mongo-1" && post.phase === "switch")?.confirm).toBe("mongo-1");
    expect(posts.find((post) => post.machine === "monitoring-1" && post.phase === "switch")?.confirm).toBe("");
  });

  it("skips only the database when its confirmation is declined, and says so", async () => {
    render([staged("mongo-1", "mongo"), staged("monitoring-1", "monitoring")]);
    const posts = fakeDashboard({ "monitoring-1": { check: { can_switch: closureOf("monitoring-1") }, switch: { result: "DONE" } } });
    await bringAllToLatest(bulkButton());
    expect(text("#fleetbulkcons .consfoot")).toBe("done — 1 machine(s) switched.");
    expect(posts.some((post) => post.machine === "mongo-1")).toBe(false);
    expect(text("#fleetbulkcons .out")).toContain("skipping (confirmation declined): mongo-1");
  });

  it("starts nothing NEW once a check fails", async () => {
    // MORE MACHINES THAN THREADS, deliberately: with six workers and three hosts every machine starts
    // at once and there is no queue left for a stop rule to act on.
    const rows = Array.from({ length: 10 }, (_, index) => staged(`h${index + 1}`));
    render(rows);
    const posts = fakeDashboard(Object.fromEntries(rows.map((row) => [row.name, { check: { exit: row.name === "h1" ? 7 : 0, can_switch: closureOf(row.name) }, switch: { result: "DONE" } }])));
    await bringAllToLatest(bulkButton());
    const checked = new Set(posts.filter((post) => post.phase === "check").map((post) => post.machine));
    expect(checked.size).toBeLessThan(10);
    expect(text("#fleetbulkcons .consfoot")).toMatch(/^stopped after h1 — /);
    expect(posts.some((post) => post.machine === "h1" && post.phase === "switch")).toBe(false);
  });

  it("asks Prometheus before calling a switch it lost contact with a failure", async () => {
    render([staged("monitoring-1", "monitoring")]);
    const readings: string[] = [];
    const posts = fakeDashboard(
      { "monitoring-1": { check: { can_switch: closureOf("monitoring-1") }, switch: { result: "DONE" } } },
      { unreachableJobs: ["j2"], readings },
    );
    await bringAllToLatest(bulkButton());
    expect(readings).toEqual(["monitoring-1"]);
    expect(text("#fleetbulkcons .out")).toContain("confirmed from Prometheus: monitoring-1 IS running the target closure");
    expect(text("#fleetbulkcons .consfoot")).toBe("done — 1 machine(s) switched.");
    expect(posts).toHaveLength(2);
  });
});

describe("the per-machine button", () => {
  it("checks first, offers the switch only once the dry run has spoken, and retires rather than removes the button", async () => {
    const row = staged("monitoring-1", "monitoring");
    render([row]);
    const posts = fakeDashboard({ "monitoring-1": { check: { can_switch: closureOf("monitoring-1") }, switch: { result: "DONE" } } });
    await fleetCheck(document.querySelector<HTMLButtonElement>(".actioncell .actionrow .applybtn")!);
    expect(posts.map((post) => post.phase)).toEqual(["check"]);
    const go = document.querySelector<HTMLButtonElement>(".consfoot .applybtn.go");
    expect(go?.textContent).toBe("Activate this closure now");

    // A pushed state arrives mid-flow: the console, and the switch button in it, must survive.
    applySnapshot(stateOf([row]));
    const kept = document.querySelector<HTMLButtonElement>(".consfoot .applybtn.go");
    expect(kept).toBe(go);
    expect(text(".actioncell .out")).toContain("· check monitoring-1");

    kept!.click();
    await flush();
    await flush();
    expect(posts.map((post) => post.phase)).toEqual(["check", "switch"]);
    applySnapshot(stateOf([landedOn(row)]));
    await flush();
    expect(text(".actioncell .consfoot")).toContain("the row above now reflects the closure it is running");
    expect(text(".actioncell .actionrow")).toContain("reload the page");
    expect(text(".actioncell .out")).toContain("--- activating ---");
  });

  it("asks the production database for its name, and switches nothing when it is not typed", async () => {
    render([staged("mongo-1", "mongo")]);
    const posts = fakeDashboard({ "mongo-1": { check: { can_switch: closureOf("mongo-1") }, switch: { result: "DONE" } } }, { prompt: () => "mongo-2" });
    await fleetCheck(document.querySelector<HTMLButtonElement>(".actioncell .applybtn")!);
    document.querySelector<HTMLButtonElement>(".consfoot .applybtn.go")!.click();
    await flush();
    expect(posts.map((post) => post.phase)).toEqual(["check"]);
  });
});
