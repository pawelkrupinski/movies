import { describe, expect, it } from "vitest";
import type { Sample } from "../../src/fleet/model.js";
import { assembleFleet, indexByHost, readMachine, shortClosure } from "../../src/fleet/read.js";
import { CLOSURE } from "./fixtures.js";

const sample = (name: string, labels: Record<string, string> = {}, value = "1", instance = "10.20.0.10:9100"): Sample =>
  ({ metric: { __name__: name, instance, ...labels }, value: [0, value] });

const DECL = { hostName: "mongo-1", privateAddress: "10.20.0.10", publicAddress: "1.2.3.4", role: "mongo", environment: "prod" };
const noDistance = () => null;

function read(samples: Sample[], behind: (revision: string) => number | null = noDistance) {
  return readMachine("mongo-1", DECL, indexByHost(samples)["10.20.0.10"], behind);
}

const reporting = (...extra: Sample[]) => [
  sample("nixos_closure_info", { closure: CLOSURE }),
  sample("nixos_configuration_revision_info", { revision: "aaa92e6e2abc" }),
  sample("nixos_auto_apply_info", { state: "up_to_date" }),
  ...extra,
];

describe("indexByHost", () => {
  it("joins on the instance address, not a host label Prometheus may have relabelled", () => {
    const byHost = indexByHost([
      sample("nixos_closure_info", { host: "monitoring-1", exported_host: "mongo-1" }),
      sample("nixos_closure_info", {}, "1", "10.20.0.11:9100"),
      sample("nixos_closure_info", {}, "1", ""),
    ]);
    expect(Object.keys(byHost).sort()).toEqual(["10.20.0.10", "10.20.0.11"]);
  });

  it("keeps the first sample of a name, as pick() did", () => {
    const byHost = indexByHost([sample("nixos_closure_info", { closure: "first" }), sample("nixos_closure_info", { closure: "second" })]);
    expect(byHost["10.20.0.10"]?.nixos_closure_info?.metric.closure).toBe("first");
  });
});

describe("readMachine", () => {
  it("calls a declared host that publishes nothing an alarm with no button", () => {
    const row = readMachine("mongo-1", DECL, undefined, noDistance);
    expect(row).toMatchObject({ reporting: false, severity: "alarm", stateKey: "notreporting", actionable: false });
  });

  it("reads a current host", () => {
    const row = read(reporting(), () => 0);
    expect(row).toMatchObject({
      reporting: true, severity: "ok", state: "current", closure: CLOSURE, nixpkgs: "26.05.20260827.d57af92",
      revisionShort: "aaa92e6e2", behind: 0, applyCovered: true, actionable: false,
    });
    expect(shortClosure(row.closure)).toBe("6idh361s36gw");
  });

  it("measures distance from the committed part of a dirty revision, and says it is dirty", () => {
    const asked: string[] = [];
    const row = read([sample("nixos_configuration_revision_info", { revision: "abc123-dirty" })], (revision) => (asked.push(revision), 4));
    expect(asked).toEqual(["abc123"]);
    expect(row).toMatchObject({ dirty: true, state: "built dirty", severity: "warn", behind: 4 });
  });

  it("ranks an owed reboot over a block over a staged closure over a dirty build", () => {
    const all = [
      sample("nixos_configuration_revision_info", { revision: "abc-dirty" }),
      sample("nixos_staged_pending"),
      sample("nixos_auto_apply_blocked"),
      sample("nixos_auto_apply_info", { state: "blocked", reason: "units_would_change" }),
    ];
    expect(read([...all, sample("nixos_auto_apply_reboot_owed")]).state).toBe("reboot owed");
    expect(read(all).state).toBe("blocked: units_would_change");
    expect(read(all.filter((s) => s.metric.__name__ !== "nixos_auto_apply_blocked")).state).toBe("staged, not activated");
  });

  it("offers a button for a staged closure, from the gauge or from a differing staged revision", () => {
    expect(read(reporting(sample("nixos_staged_pending"))).actionable).toBe(true);
    expect(read(reporting(sample("nixos_staged_revision_info", { revision: "bbbbbbb" }))).actionable).toBe(true);
    expect(read(reporting(sample("nixos_staged_revision_info", { revision: "aaa92e6e2abc" }))).actionable).toBe(false);
    expect(read(reporting(sample("nixos_staged_pending", {}, "0"))).actionable).toBe(false);
  });

  it("never offers a button on a host with no public address to reach it at", () => {
    const row = readMachine("mongo-1", { ...DECL, publicAddress: "" }, indexByHost(reporting(sample("nixos_staged_pending")))["10.20.0.10"], noDistance);
    expect(row.actionable).toBe(false);
  });

  it("carries the excluded reason, and calls a host with no auto-apply metric not covered", () => {
    expect(read(reporting(sample("nixos_auto_apply_excluded", { reason: "k3s drains" }))).excludedReason).toBe("k3s drains");
    expect(read([sample("nixos_closure_info", { closure: CLOSURE })]).applyCovered).toBe(false);
  });
});

describe("assembleFleet", () => {
  const inputs = {
    machines: { "mongo-1": DECL },
    byAddress: indexByHost([...reporting(), sample("nixos_closure_info", {}, "1", "10.20.0.99:9100")]),
    promError: null, head: "h", origin: "o", dirtyCheckout: false, behind: noDistance, readAt: 1, took: 0.5,
  };

  it("names a scrape target the flake does not declare", () => {
    expect(assembleFleet({ ...inputs, rosterError: null }).undeclared).toEqual(["10.20.0.99"]);
  });

  it("reports NOTHING undeclared when the roster itself failed", () => {
    // Seen for real: a stale working directory made the page announce every host as rogue.
    const state = assembleFleet({ ...inputs, rosterError: "nix eval failed: x" });
    expect(state.undeclared).toEqual([]);
    expect(state.errors).toEqual(["nix eval failed: x"]);
  });
});
