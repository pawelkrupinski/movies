/**
 * Pure readings: one machine's row from its declaration and its series, and the fleet's rows from
 * the whole roster. No I/O here -- the sources feed these, the tests call them.
 */
import { byString, type Declaration, type FleetState, type MachineRow, type Sample, type Series } from "./model.js";

/**
 * Group every returned series by the host it belongs to.
 *
 * KEYED ON THE `instance` ADDRESS, not on a hostname label. The textfile publisher sets `host`,
 * but Prometheus ALSO relabels a `host` coming from a scraped target into `exported_host` when the
 * scrape config already assigns one -- which this fleet's does. So `host` may be the scrape's value
 * and `exported_host` the host's own. The instance address is set by the scrape config from
 * nodeTargets and is unambiguous, so that is what we join on.
 */
export function indexByHost(samples: readonly Sample[]): Record<string, Series> {
  const out: Record<string, Record<string, Sample>> = {};
  for (const sample of samples) {
    const address = (sample.metric.instance ?? "").split(":")[0] ?? "";
    const name = sample.metric.__name__ ?? "";
    if (!address) continue;
    const host = (out[address] ??= {});
    host[name] ??= sample;
  }
  return out;
}

const label = (sample: Sample | undefined, key: string): string => sample?.metric[key] ?? "";

function valueOf(sample: Sample | undefined, fallback: number): number {
  const parsed = Number(sample?.value[1]);
  return sample && sample.value[1]?.trim() !== "" && !Number.isNaN(parsed) ? parsed : fallback;
}

const CLOSURE_RE = /^(?<hash>[a-z0-9]{32})-nixos-system-(?<host>.+?)-(?<version>[\d.]+\..+)$/;

/** The store hash is the identity; the rest is the same on every host and only adds width. */
export function shortClosure(closure: string): string {
  return CLOSURE_RE.exec(closure)?.groups?.hash?.slice(0, 12) ?? closure.slice(0, 12);
}

export function closureVersion(closure: string): string {
  return CLOSURE_RE.exec(closure)?.groups?.version ?? "";
}

const DIRTY = "-dirty";
export const cleanRevision = (revision: string): string => revision.replace(DIRTY, "");

/**
 * Join one host's declaration with what it is publishing, and decide how alarming it is.
 * `behind` is how far the running revision is from main, measured by the caller (it is git).
 */
export function readMachine(
  name: string,
  decl: Declaration,
  series: Series | undefined,
  behind: (revision: string) => number | null,
): MachineRow {
  const base = {
    name,
    // The flake's attribute name and the host's own `networking.hostName` are the same string on
    // every machine today, so the line under the name is rendered only when they DIVERGE -- which
    // is the only time it says anything.
    hostname: decl.hostName ?? "",
    role: decl.role ?? "",
    env: decl.environment ?? "",
    private: decl.privateAddress ?? "",
    public: decl.publicAddress ?? "",
  };

  if (!series || !Object.keys(series).length) {
    // A DECLARED HOST THAT SAYS NOTHING. Not the same as "unreachable" -- we never tried to reach
    // it. Either its node_exporter is down, its textfile collector is empty, or Prometheus is not
    // scraping it. All three are worth waking up for, and none of them are "fine".
    return {
      ...base,
      reporting: false,
      state: "not reporting",
      stateKey: "notreporting",
      severity: "alarm",
      detail: "publishes no nixos_* metrics",
      closure: "", booted: "", nixpkgs: "", revision: "", revisionShort: "", stagedRevision: "", stagedShort: "",
      dirty: false, behind: null, rebootRequired: false, stagedPending: false, blocked: false,
      autoApply: "", blockedReason: "", excludedReason: "", applyCovered: false, lastVerdict: 0,
      // NO BUTTON ON A SILENT HOST. Not because acting would be unsafe -- the check phase would
      // read the host directly and find out the truth -- but because nothing here knows whether it
      // has anything staged, and an offer to "activate the staged closure" on a machine we cannot
      // say has one is an offer to find out by trying.
      actionable: false,
    };
  }

  const info = series.nixos_auto_apply_info;
  const closure = label(series.nixos_closure_info, "closure");
  const revision = label(series.nixos_configuration_revision_info, "revision");
  const stagedRevision = label(series.nixos_staged_revision_info, "revision");
  const rebootRequired = valueOf(series.nixos_reboot_required, 0) === 1;
  const rebootOwed = valueOf(series.nixos_auto_apply_reboot_owed, 0) === 1;
  const stagedPending = valueOf(series.nixos_staged_pending, 0) === 1;
  const blocked = valueOf(series.nixos_auto_apply_blocked, 0) === 1;
  const blockedReason = label(info, "reason");
  // A REVISION ENDING `-dirty` MEANS THE CLOSURE WAS BUILT FROM AN UNCOMMITTED TREE, which makes it
  // unreproducible: nothing in git describes what that machine is running. It is called out
  // separately from "behind" because the fix is different -- committing and redeploying, not
  // waiting for auto-apply.
  const dirty = revision.endsWith(DIRTY);
  const clean = cleanRevision(revision);

  let state: string;
  let stateKey: MachineRow["stateKey"];
  if (rebootRequired || rebootOwed) [state, stateKey] = ["reboot owed", "reboot"];
  else if (blocked) [state, stateKey] = [`blocked: ${blockedReason || "unknown"}`, "blocked"];
  else if (stagedPending) [state, stateKey] = ["staged, not activated", "staged"];
  else if (dirty) [state, stateKey] = ["built dirty", "dirty"];
  else [state, stateKey] = ["current", "current"];

  return {
    ...base,
    reporting: true,
    state,
    stateKey,
    severity: stateKey === "current" ? "ok" : "warn",
    detail: label(info, "detail"),
    closure,
    booted: label(series.nixos_booted_closure_info, "closure"),
    nixpkgs: label(series.node_os_info, "build_id") || closureVersion(closure),
    revision,
    revisionShort: clean.slice(0, 9),
    stagedRevision,
    stagedShort: stagedRevision.slice(0, 9),
    dirty,
    behind: behind(clean),
    rebootRequired: rebootRequired || rebootOwed,
    stagedPending,
    blocked,
    autoApply: label(info, "state"),
    blockedReason,
    // EXCLUDED-ON-PURPOSE AND NEVER-WIRED-UP ARE THE SAME ABSENCE to anything counting metrics, and
    // they want opposite responses -- which is exactly why `excludedBecause` publishes a reason.
    excludedReason: label(series.nixos_auto_apply_excluded, "reason"),
    applyCovered: info !== undefined,
    lastVerdict: valueOf(series.nixos_auto_apply_last_verdict_timestamp_seconds, 0),
    // WHETHER THIS ROW GETS A BUTTON AT ALL. The rule -- nothing staged, or staged equals running,
    // means no offer -- lives HERE, on the read side, so the table and the endpoint can never
    // disagree about it: FleetJobs resolves the machine out of this same row rather than
    // re-deciding.
    //
    // `nixos_staged_pending` is the host's own answer and is preferred, since it is computed on
    // the machine from the pin and /run/current-system. The revision comparison is the fallback
    // for a host that publishes a staged revision but not the gauge (an older auto-apply, a
    // half-written textfile): being one release behind on the metric should not hide a button the
    // operator otherwise has no way to reach.
    //
    // This is a HINT, never an authorisation. Everything here comes from a scrape that can be up
    // to a minute stale, so the check phase reads the pin off the host again and only then offers
    // the switch.
    actionable: (stagedPending || (!!stagedRevision && stagedRevision !== clean)) && !!decl.publicAddress,
  };
}

export interface FleetInputs {
  readonly machines: Readonly<Record<string, Declaration>>;
  readonly rosterError: string | null;
  readonly byAddress: Readonly<Record<string, Series>>;
  readonly promError: string | null;
  readonly head: string;
  readonly origin: string;
  readonly dirtyCheckout: boolean;
  readonly behind: (revision: string) => number | null;
  readonly readAt: number;
  readonly took: number;
}

/** The whole page's state from its inputs. */
export function assembleFleet(inputs: FleetInputs): FleetState {
  const names = Object.keys(inputs.machines).sort(byString);
  const rows = names.map((name) => {
    const decl = inputs.machines[name] ?? {};
    return readMachine(name, decl, inputs.byAddress[decl.privateAddress ?? ""], inputs.behind);
  });
  // Anything publishing metrics that the flake does not declare. On a small fleet this should
  // always be empty; if it is not, something is scraping a machine nobody owns.
  //
  // ONLY MEANINGFUL IF THE ROSTER ACTUALLY LOADED. A failed `nix eval` yields either an empty
  // roster or the one read before the failure, and "not declared" computed against either is a
  // finding about the query rather than about the fleet -- with an empty roster every healthy host
  // is reported as rogue. Seen for real when the flake directory moved out from under a running
  // process: the page announced all three machines as rogue while the only fault was a stale
  // working directory. A check whose input failed must report NOTHING, not the answer it would
  // have given with no input.
  const declared = new Set(Object.values(inputs.machines).map((decl) => decl.privateAddress));
  const undeclared = inputs.rosterError ? [] : Object.keys(inputs.byAddress).filter((address) => !declared.has(address)).sort(byString);
  return {
    ready: true,
    buildError: null,
    rows,
    undeclared,
    head: inputs.head,
    origin: inputs.origin,
    dirtyCheckout: inputs.dirtyCheckout,
    errors: [inputs.rosterError, inputs.promError].filter((error): error is string => !!error),
    readAt: inputs.readAt,
    took: inputs.took,
  };
}
