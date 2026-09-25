/**
 * The fleet page's data, as plain JSON: what the server pushes, what the browser re-renders from,
 * and what the pure functions read. Imports nothing from node: -- the browser bundles it.
 */

/** One series from an instant query: its labels (`__name__` among them) and its value. */
export interface Sample {
  readonly metric: Readonly<Record<string, string>>;
  readonly value: readonly [number, string];
}

/** One host's series, by metric name. The FIRST sample of a name wins, as `pick` always did. */
export type Series = Readonly<Record<string, Sample>>;

export type Severity = "ok" | "warn" | "alarm";

/** What the flake declares about one host (the shape `nix eval` returns, per configuration). */
export interface Declaration {
  readonly hostName?: string;
  readonly privateAddress?: string;
  readonly publicAddress?: string;
  readonly role?: string;
  readonly environment?: string;
}

export type StateKey = "current" | "staged" | "reboot" | "blocked" | "dirty" | "notreporting";

/**
 * One machine's row. The flags are kept SEPARATE rather than collapsed into the one `state`,
 * because each is a different repair: a staged closure needs activating, an owed reboot a window,
 * a blocked auto-apply a person to read the reason, a dirty build a commit and a redeploy.
 */
export interface MachineRow {
  readonly name: string;
  /** The host's own `networking.hostName`; rendered only where it differs from `name`. */
  readonly hostname: string;
  readonly role: string;
  readonly env: string;
  /** Where Prometheus reports it -- the join key. */
  readonly private: string;
  /** Where ssh reaches it: this fleet has no VPN and no jump host. */
  readonly public: string;
  readonly reporting: boolean;
  readonly state: string;
  readonly stateKey: StateKey;
  readonly severity: Severity;
  readonly detail: string;
  readonly closure: string;
  readonly booted: string;
  readonly nixpkgs: string;
  readonly revision: string;
  readonly revisionShort: string;
  readonly stagedRevision: string;
  readonly stagedShort: string;
  readonly dirty: boolean;
  /** Commits origin/main (or HEAD) is ahead of the running revision; null when unmeasurable. */
  readonly behind: number | null;
  readonly rebootRequired: boolean;
  readonly stagedPending: boolean;
  readonly blocked: boolean;
  readonly autoApply: string;
  readonly blockedReason: string;
  readonly excludedReason: string;
  readonly applyCovered: boolean;
  /** Unix seconds; 0 when never. */
  readonly lastVerdict: number;
  /** Whether this row gets a button at all -- see `readMachine`. */
  readonly actionable: boolean;
}

export interface FleetState {
  /** False until the first roster and the first Prometheus read behind it have finished. */
  readonly ready: boolean;
  /** The last recompute threw: the page says it knows NOTHING rather than showing old rows. */
  readonly buildError: string | null;
  readonly rows: readonly MachineRow[];
  /** Addresses publishing metrics that the flake does not declare. */
  readonly undeclared: readonly string[];
  readonly head: string;
  readonly origin: string;
  readonly dirtyCheckout: boolean;
  readonly errors: readonly string[];
  /** Epoch ms of the newest SUCCESSFUL Prometheus read; 0 when there has been none. */
  readonly readAt: number;
  /** Seconds that read took (it is an ssh round trip, so it is worth seeing). */
  readonly took: number;
}

export const emptyFleetState = (): FleetState => ({
  ready: false,
  buildError: null,
  rows: [],
  undeclared: [],
  head: "",
  origin: "",
  dirtyCheckout: false,
  errors: [],
  readAt: 0,
  took: 0,
});

/**
 * A store path this tool will consent to activate. The switch phase is handed a path, and this
 * pattern plus the "a check must have offered it" rule in FleetJobs are the two things that keep
 * that endpoint from being an activate-any-path endpoint. Neither alone is enough -- the pattern
 * would still admit any well-formed store path that happens to exist on the host.
 */
export const CLOSURE_PATH_RE = /^\/nix\/store\/[a-z0-9]{32}-nixos-system-[A-Za-z0-9._+-]+$/;

/**
 * ROLES THAT DEMAND A SECOND, TYPED CONFIRMATION rather than one click.
 *
 * Keyed on `fleet.role`, NOT on the hostname, and that is the point: a second database host added
 * tomorrow inherits the guard by declaring `role = "mongo"`, whereas a hostname list would let it
 * through silently. mongo-1 is the production database. Activating a closure there can restart
 * mongod, and the web tier holds CHANGE STREAMS against it (see the replSetName comment in
 * nix/hosts/mongo-1/default.nix -- a change stream that drops does not come back by itself), so a
 * restart here is not "some units bounce", it is the read path going quiet until the app
 * reconnects. That is a risk, not an incident that happened; it is stated as a risk.
 */
export const CONFIRM_ROLES: readonly string[] = ["mongo"];

/** Whether this machine's switch demands a typed confirmation rather than one click. */
export const needsConfirmation = (row: Pick<MachineRow, "role">): boolean => CONFIRM_ROLES.includes(row.role);

export const byString = (a: string, b: string): number => (a < b ? -1 : a > b ? 1 : 0);
