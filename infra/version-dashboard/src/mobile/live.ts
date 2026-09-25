/**
 * The mobile page's live state: rebuilt in the background on its own cadence and pushed to every
 * open tab. A request never builds anything.
 */
import { Store } from "../store.js";
import { buildMobile, nextBuildDelayMs, type MobileSources } from "./build.js";
import { allPlatformsNetworkFailed, emptyMobileState, type MobileState } from "./model.js";
import { describeError } from "./network.js";

/**
 * Consecutive builds that failed for a network reason on EVERY platform, after which the process
 * restarts itself. The backstop for 2026-09-22: both ASC and Play failed with the identical
 * "nodename nor servname provided" for as long as anyone looked, and only a restart cleared it -- a
 * stuck resolver that in-process retries do not fix, since each retry re-hits the same stale state.
 * An HTTP-level failure (bad credentials, a real Apple/Google outage) never counts, so this cannot
 * restart-loop over something a fresh process would not fix either.
 */
export const SELF_RESTART_AFTER = 5;
/** Between deciding to restart and doing it, so whatever is mid-response finishes first. */
export const SELF_RESTART_GRACE_MS = 2_000;

export interface MobileLiveOptions {
  readonly build?: () => Promise<MobileState>;
  readonly now?: () => number;
  /** How the process restarts: SIGTERM, so the server drains running work and exits, and launchd's
   * KeepAlive relaunches it. The process holds no state; a restart costs exactly one build. */
  readonly restart?: () => void;
}

export class MobileLive {
  readonly store: Store<MobileState>;
  consecutiveNetworkFailures = 0;
  private readonly build: () => Promise<MobileState>;
  private readonly restart: () => void;
  private building: Promise<void> | null = null;
  private timer: ReturnType<typeof setTimeout> | null = null;
  private restartScheduled = false;
  private stopped = true;

  constructor(sources: MobileSources | null, options: MobileLiveOptions = {}) {
    const now = options.now ?? Date.now;
    this.store = new Store(emptyMobileState(), now);
    this.build = options.build ?? (() => {
      if (!sources) throw new Error("no sources to build from");
      return buildMobile(sources);
    });
    this.restart = options.restart ?? (() => process.kill(process.pid, "SIGTERM"));
  }

  start(): void {
    this.stopped = false;
    void this.rebuild();
  }

  stop(): void {
    this.stopped = true;
    if (this.timer) clearTimeout(this.timer);
    this.timer = null;
  }

  /**
   * Build now (a Refresh press, a wake, the timer). Single-flight: a press while a build runs gets
   * that build rather than a second one racing it. Never rejects.
   */
  rebuild(): Promise<void> {
    this.building ??= this.runBuild().finally(() => (this.building = null));
    return this.building;
  }

  private async runBuild(): Promise<void> {
    let built: MobileState | null = null;
    try {
      built = await this.build();
      this.store.set(built);
      this.consecutiveNetworkFailures = allPlatformsNetworkFailed(built) ? this.consecutiveNetworkFailures + 1 : 0;
      this.maybeRestart();
    } catch (error) {
      // A build that threw leaves the last good platforms up, and says so on the page.
      console.error(`mobile dashboard: build failed: ${describeError(error)}`);
      this.store.set({ ...this.store.get().state, ready: true, buildError: describeError(error) });
    }
    if (this.stopped) return;
    if (this.timer) clearTimeout(this.timer);
    this.timer = setTimeout(() => void this.rebuild(), nextBuildDelayMs(built));
    this.timer.unref?.();
  }

  private maybeRestart(): void {
    if (this.consecutiveNetworkFailures < SELF_RESTART_AFTER || this.restartScheduled) return;
    this.restartScheduled = true;
    console.error(`mobile dashboard: ${this.consecutiveNetworkFailures} consecutive network-level failures on every platform, restarting in ${SELF_RESTART_GRACE_MS / 1000}s for launchd to relaunch`);
    setTimeout(() => this.restart(), SELF_RESTART_GRACE_MS).unref?.();
  }
}
