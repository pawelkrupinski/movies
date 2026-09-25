/**
 * The fleet page's live state: every source polled in the background on its own cadence, the state
 * recomputed (debounced) when any of them answers, and pushed to every open tab. A request never
 * builds anything -- the Python page's request-time builds, its serve-stale-while-rebuilding cache
 * and its 30-second `location.reload()` are gone with it.
 *
 *   checkout + roster  every 10s: git HEAD/origin, and the flake's fingerprint; `nix eval` only
 *                      when that fingerprint moves (see RosterKeeper)
 *   Prometheus         every 30s, one ssh round trip to monitoring-1 (every 5s for up to 240s
 *                      after a switch, until the new closure shows)
 */
import { Store } from "../store.js";
import { Distances, inspectCheckout, type Checkout } from "./git.js";
import { emptyFleetState, type FleetState, type MachineRow, type Series } from "./model.js";
import { promSeries } from "./prometheus.js";
import { assembleFleet, cleanRevision, indexByHost, shortClosure } from "./read.js";
import { RosterKeeper, type RosterAnswer } from "./roster.js";

export interface Cadence {
  readonly checkoutMs: number;
  readonly prometheusMs: number;
  readonly debounceMs: number;
  readonly scrapeWaitStepMs: number;
  readonly scrapeWaitBudgetMs: number;
}

export const DEFAULT_CADENCE: Cadence = {
  checkoutMs: 10_000,
  prometheusMs: 30_000,
  debounceMs: 150,
  scrapeWaitStepMs: 5_000,
  scrapeWaitBudgetMs: 240_000,
};

export interface FleetSources {
  readonly roster: Pick<RosterKeeper, "read">;
  readonly checkout: () => Promise<Checkout>;
  readonly prometheus: typeof promSeries;
  readonly distances: Pick<Distances, "measure" | "between">;
}

export function realSources(infraDir: string, onRosterChange: () => void): FleetSources {
  return {
    roster: new RosterKeeper({ infraDir, onChange: onRosterChange }),
    checkout: () => inspectCheckout(infraDir),
    prometheus: promSeries,
    distances: new Distances(infraDir),
  };
}

/**
 * Runs `work` unless a previous call is still in flight (then that one's promise is returned).
 * Never rejects: a poller that threw is logged, and its in-flight marker is cleared in the same
 * `finally` either way -- a marker that outlives a failure wedges every later poll.
 */
function singleFlight(label: string, work: () => Promise<void>): () => Promise<void> {
  let current: Promise<void> | null = null;
  return () => {
    current ??= (async () => {
      try {
        await work();
      } catch (error) {
        console.error(`fleet: ${label} failed: ${error instanceof Error ? error.message : String(error)}`);
      } finally {
        current = null;
      }
    })();
    return current;
  };
}

export class FleetLive {
  readonly store: Store<FleetState>;
  private readonly sources: FleetSources;
  private roster: RosterAnswer | null = null;
  private checkout: Checkout = { head: "", origin: "", dirty: false };
  private byAddress: Record<string, Series> = {};
  private promError: string | null = null;
  private readAt = 0;
  private took = 0;
  private ready = false;
  /** The newest state that was built without error: what a job resolves its machine against. */
  private lastGood: FleetState | null = null;
  private timers: ReturnType<typeof setInterval>[] = [];
  private debounce: ReturnType<typeof setTimeout> | null = null;
  private recomputing: Promise<void> | null = null;
  private recomputeAgain = false;
  private stopped = false;
  private readonly cadence: Cadence;
  private readonly now: () => number;
  private readonly sleep: (ms: number) => Promise<void>;

  constructor(
    sources: FleetSources | ((onRosterChange: () => void) => FleetSources),
    options: { cadence?: Partial<Cadence>; now?: () => number; sleep?: (ms: number) => Promise<void> } = {},
  ) {
    this.sources = typeof sources === "function" ? sources(() => void this.pollCheckout()) : sources;
    this.cadence = { ...DEFAULT_CADENCE, ...options.cadence };
    this.now = options.now ?? Date.now;
    this.sleep = options.sleep ?? ((ms) => new Promise((wake) => setTimeout(wake, ms)));
    this.store = new Store(emptyFleetState(), this.now);
  }

  start(): void {
    this.stopped = false;
    void this.boot().then(() => {
      if (this.stopped) return;
      this.timers.push(
        setInterval(() => void this.pollCheckout(), this.cadence.checkoutMs),
        setInterval(() => void this.pollPrometheus(), this.cadence.prometheusMs),
      );
    });
  }

  stop(): void {
    this.stopped = true;
    this.timers.forEach(clearInterval);
    this.timers = [];
    if (this.debounce) clearTimeout(this.debounce);
  }

  /**
   * First read of everything. Roster and metrics CONCURRENTLY: one may have to run a local `nix
   * eval` and the other is an ssh round trip, and there is no reason to pay for both in turn.
   */
  async boot(): Promise<void> {
    await Promise.all([this.pollCheckout(), this.pollPrometheus()]);
    this.ready = true;
    await this.recompute();
  }

  /** Re-poll everything now (the Refresh button, and a Mac that has just woken: timers do not
   * catch up and a poll in flight across the sleep may have died silently). */
  async refreshNow(): Promise<void> {
    await Promise.all([this.pollCheckout(), this.pollPrometheus()]);
  }

  /** The machine as the page last showed it. */
  machine(name: string): MachineRow | undefined {
    return this.lastGood?.rows.find((row) => row.name === name);
  }

  readonly pollCheckout = singleFlight("checkout and roster", async () => {
    const [checkout, roster] = await Promise.all([this.sources.checkout(), this.sources.roster.read()]);
    this.checkout = checkout;
    this.roster = roster;
    this.schedule();
  });

  readonly pollPrometheus = singleFlight("prometheus", async () => {
    await this.readPrometheus();
    this.schedule();
  });

  /** One read; answers whether it succeeded. A failed read is no series at all, never the last
   * one's -- every row then says it is not reporting, under a banner saying why. */
  private async readPrometheus(): Promise<boolean> {
    const started = this.now();
    const answer = await this.sources.prometheus();
    if ("error" in answer) {
      this.byAddress = {};
      this.promError = answer.error;
      return false;
    }
    this.byAddress = indexByHost(answer.series);
    this.promError = null;
    this.readAt = this.now();
    this.took = (this.readAt - started) / 1000;
    return true;
  }

  /**
   * After a switch: the closure metric is written by the host's activation script, but Prometheus
   * scrapes on its own cadence, so the moment a switch completes is precisely when the metrics
   * still describe the closure it replaced -- a reload issued then shows the OLD closure and looks
   * like the switch did nothing. So wait for the exact closure to show, pushing the state the
   * moment it does, and push either way when the budget runs out.
   */
  async awaitSwitchLanded(machine: MachineRow, closure: string): Promise<void> {
    const want = closure.slice(closure.lastIndexOf("/") + 1);
    const deadline = this.now() + this.cadence.scrapeWaitBudgetMs;
    while (machine.private && want && this.now() < deadline && !this.stopped) {
      await this.sleep(this.cadence.scrapeWaitStepMs);
      if (await this.readPrometheus()) {
        this.schedule();
        if (this.byAddress[machine.private]?.nixos_closure_info?.metric.closure === want) return;
      }
      // A blip in the poll must not end the wait.
    }
    await this.pollPrometheus();
  }

  /**
   * GET /fleet-apply/machine -- re-read Prometheus and say what ONE machine is running now. The
   * bulk run asks this when it lost contact with its own job mid-switch: the consumer of what got
   * activated is a better witness than the process that ran it. The ROSTER is not re-read: nothing
   * a switch does can change what the flake declares.
   */
  async readOne(name: string): Promise<{ store_hash: string; closure: string; state: string } | { error: string }> {
    const known = this.machine(name);
    if (!known) return { error: `no machine called '${name}' on this page` };
    if (!known.private) return { error: `${name} has no private address, so Prometheus reports nothing for it` };
    if (!(await this.readPrometheus())) return { error: this.promError ?? "Prometheus could not be read" };
    this.schedule();
    await this.recompute();
    const row = this.machine(name);
    return { store_hash: shortClosure(row?.closure ?? ""), closure: row?.closure ?? "", state: row?.state ?? "" };
  }

  private schedule(): void {
    if (!this.ready || this.stopped) return;
    if (this.debounce) clearTimeout(this.debounce);
    this.debounce = setTimeout(() => {
      this.debounce = null;
      void this.recompute();
    }, this.cadence.debounceMs);
  }

  /** Single-flight, rerun once more if asked again meanwhile. A failure is rendered as an alarm and
   * never leaves the state half-built. */
  recompute(): Promise<void> {
    if (this.recomputing) {
      this.recomputeAgain = true;
      return this.recomputing;
    }
    this.recomputing = (async () => {
      do {
        this.recomputeAgain = false;
        try {
          const target = this.checkout.origin || this.checkout.head;
          const revisions = new Set<string>();
          for (const series of Object.values(this.byAddress)) {
            const revision = series.nixos_configuration_revision_info?.metric.revision;
            if (revision) revisions.add(cleanRevision(revision));
          }
          await Promise.all([...revisions].map((revision) => this.sources.distances.measure(revision, target)));
          const state = assembleFleet({
            machines: this.roster?.machines ?? {},
            rosterError: this.roster?.error ?? null,
            byAddress: this.byAddress,
            promError: this.promError,
            head: this.checkout.head,
            origin: this.checkout.origin,
            dirtyCheckout: this.checkout.dirty,
            behind: (revision) => this.sources.distances.between(revision, target),
            readAt: this.readAt,
            took: this.took,
          });
          this.lastGood = state;
          this.store.set(state);
        } catch (error) {
          this.store.set({
            ...(this.lastGood ?? emptyFleetState()),
            ready: true,
            buildError: error instanceof Error ? error.message : String(error),
          });
        }
      } while (this.recomputeAgain);
    })().finally(() => (this.recomputing = null));
    return this.recomputing;
  }
}
