import { describe, expect, it } from "vitest";
import type { Checkout } from "../../src/fleet/git.js";
import { FleetLive, type FleetSources } from "../../src/fleet/live.js";
import type { Sample } from "../../src/fleet/model.js";
import type { promSeries } from "../../src/fleet/prometheus.js";
import type { RosterAnswer } from "../../src/fleet/roster.js";
import { CLOSURE } from "./fixtures.js";

const CLOCK = 1_790_000_000_000;
const NEW_CLOSURE = `${"z".repeat(32)}-nixos-system-mongo-1-26.05.20260901.abcdef0`;
const ROSTER: RosterAnswer = {
  machines: { "mongo-1": { hostName: "mongo-1", privateAddress: "10.20.0.10", publicAddress: "1.2.3.4", role: "mongo", environment: "prod" } },
  error: null,
};

const sample = (name: string, labels: Record<string, string>): Sample => ({ metric: { __name__: name, instance: "10.20.0.10:9100", ...labels }, value: [0, "1"] });
const running = (closure: string, revision = "abc") => [
  sample("nixos_closure_info", { closure }),
  sample("nixos_configuration_revision_info", { revision }),
  sample("nixos_staged_revision_info", { revision: "def" }),
  sample("nixos_auto_apply_info", { state: "blocked", reason: "units_would_change" }),
];

function world(answers: Awaited<ReturnType<typeof promSeries>>[], over: Partial<FleetSources> = {}) {
  const measured: string[] = [];
  let reads = 0;
  const sources: FleetSources = {
    roster: { read: async () => ROSTER },
    checkout: async (): Promise<Checkout> => ({ head: "head", origin: "origin", dirty: false }),
    prometheus: async () => answers[Math.min(reads++, answers.length - 1)] ?? { error: "no answer" },
    distances: {
      measure: async (a, b) => void measured.push(`${a}..${b}`),
      between: (a, b) => (a === "abc" && b === "origin" ? 7 : null),
    },
    ...over,
  };
  let now = CLOCK;
  const live = new FleetLive(sources, { now: () => (now += 100), sleep: async () => {}, cadence: { debounceMs: 0 } });
  return { live, measured, reads: () => reads };
}

const settle = () => new Promise((wake) => setTimeout(wake, 5));

describe("the fleet's live state", () => {
  it("is not ready until the first roster and the first read behind it have landed", async () => {
    const { live, measured } = world([{ series: running(CLOSURE) }]);
    expect(live.store.get().state.ready).toBe(false);
    await live.boot();
    const { state } = live.store.get();
    expect(state).toMatchObject({ ready: true, head: "head", origin: "origin", errors: [], undeclared: [], took: 0.1 });
    expect(state.rows).toHaveLength(1);
    expect(state.rows[0]).toMatchObject({ name: "mongo-1", closure: CLOSURE, behind: 7, actionable: true, state: "current" });
    // "N behind" is measured against origin/main, from the clean revision.
    expect(measured).toEqual(["abc..origin"]);
  });

  it("says why Prometheus could not be read, and shows every host as not reporting rather than as its last reading", async () => {
    const { live } = world([{ series: running(CLOSURE) }, { error: "could not reach Prometheus through root@x: refused" }]);
    await live.boot();
    await live.pollPrometheus();
    await live.recompute();
    const { state } = live.store.get();
    expect(state.errors).toEqual(["could not reach Prometheus through root@x: refused"]);
    expect(state.rows[0]).toMatchObject({ reporting: false, severity: "alarm" });
  });

  it("carries a roster error to the page", async () => {
    const { live } = world([{ series: [] }], { roster: { read: async () => ({ machines: {}, error: "nix eval failed: boom" }) } });
    await live.boot();
    expect(live.store.get().state.errors).toEqual(["nix eval failed: boom"]);
  });

  it("waits after a switch until the new closure is scraped, and pushes it the moment it is", async () => {
    const { live, reads } = world([
      { series: running(CLOSURE) },
      { series: running(CLOSURE) },
      { error: "a blip must not end the wait" },
      { series: running(NEW_CLOSURE) },
      { series: running("never-read") },
    ]);
    await live.boot();
    const machine = live.machine("mongo-1");
    expect(machine).toBeDefined();
    await live.awaitSwitchLanded(machine!, `/nix/store/${NEW_CLOSURE}`);
    expect(reads()).toBe(4);
    await settle();
    expect(live.store.get().state.rows[0]?.closure).toBe(NEW_CLOSURE);
  });

  it("answers what one machine is running now, for a bulk run that lost its job", async () => {
    const { live } = world([{ series: running(CLOSURE) }, { series: running(NEW_CLOSURE) }]);
    await live.boot();
    expect(await live.readOne("mongo-1")).toEqual({ store_hash: "zzzzzzzzzzzz", closure: NEW_CLOSURE, state: "current" });
    expect(await live.readOne("nope")).toEqual({ error: "no machine called 'nope' on this page" });
  });

  it("renders a failure to build as an alarm instead of a half-built page", async () => {
    const { live } = world([{ series: running(CLOSURE) }], {
      distances: { measure: async () => { throw new Error("git exploded"); }, between: () => null },
    });
    await live.boot();
    expect(live.store.get().state).toMatchObject({ ready: true, buildError: "git exploded" });
  });
});
