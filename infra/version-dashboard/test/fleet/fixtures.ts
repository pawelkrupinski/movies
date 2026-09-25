import { emptyFleetState, type FleetState, type MachineRow } from "../../src/fleet/model.js";

/** Unix seconds the tests treat as "now". */
export const NOW = 1_790_000_000;
export const CLOSURE = "6idh361s36gw9zqk2p0v4x8n1m7c3jt5-nixos-system-mongo-1-26.05.20260827.d57af92";

/** A reporting, current, unremarkable host. Each test names only what it changes. */
export function machine(over: Partial<MachineRow> = {}): MachineRow {
  return {
    name: "mongo-1", hostname: "mongo-1", role: "mongo", env: "prod",
    private: "10.20.0.10", public: "1.2.3.4", reporting: true,
    closure: CLOSURE, booted: CLOSURE, nixpkgs: "26.05.20260827.d57af92",
    revision: "aaa92e6e2", revisionShort: "aaa92e6e2", stagedRevision: "", stagedShort: "",
    dirty: false, behind: 0, rebootRequired: false, stagedPending: false, blocked: false,
    autoApply: "up_to_date", blockedReason: "", detail: "", excludedReason: "",
    applyCovered: true, lastVerdict: NOW - 600,
    state: "current", stateKey: "current", severity: "ok", actionable: false,
    ...over,
  };
}

export const silent = (over: Partial<MachineRow> = {}): MachineRow => machine({
  reporting: false, closure: "", booted: "", nixpkgs: "", revision: "", revisionShort: "",
  stagedRevision: "", stagedShort: "", autoApply: "", applyCovered: false, lastVerdict: 0, behind: null,
  state: "not reporting", stateKey: "notreporting", severity: "alarm", detail: "publishes no nixos_* metrics",
  ...over,
});

export function stateOf(rows: MachineRow[], over: Partial<FleetState> = {}): FleetState {
  return {
    ...emptyFleetState(),
    ready: true,
    rows,
    head: "aaa92e6e2",
    origin: "aaa92e6e2",
    readAt: (NOW - 5) * 1000,
    took: 1.2,
    ...over,
  };
}
