/**
 * The page's release button: runs `scripts/mobile-ship.sh` -- the release lane itself, never a copy
 * of its logic -- and streams its output to every open tab.
 *
 * TWO PRESSES, AS ON THE FLEET PAGE. The first runs `--dry-run`, which reads both stores and prints
 * the version it would ship, changing nothing; only a plan that finished with a version to ship,
 * and is fresh, unlocks the second press, which ships. One run at a time, and the real one is
 * protected work: a restart (an autodeploy) waits for it rather than killing a build mid-upload.
 */
import type { Executor } from "../exec.js";
import { isDraining, protect, ShuttingDown } from "../lifecycle.js";
import { plannedVersion, type ReleaseRun } from "./model.js";

/** A plan older than this is not acted on: the stores it read may have moved since. */
export const PLAN_MAX_AGE_MS = 30 * 60_000;
/** `--dry-run`: a git fetch and a few store reads. */
export const PLAN_TIMEOUT_MS = 10 * 60_000;
/** Both builds (each bounded at 90 minutes by the lane), the submission and the promotion. */
export const RELEASE_TIMEOUT_MS = 4 * 60 * 60_000;
/** The lane logs milestones, not build output, but a runaway must not grow the snapshot without bound. */
export const LINE_CAP = 1000;

export interface ReleaseRunsOptions {
  /** scripts/mobile-ship.sh */
  readonly script: string;
  readonly cwd: string;
  /** Added to the child's environment: launchd's PATH has no JDK or Android SDK. */
  readonly env: Readonly<Record<string, string>>;
  readonly run: Executor;
  readonly now: () => number;
  /** Shows a run on the page; called on every line and once it ends. */
  readonly publish: (run: ReleaseRun) => void;
}

export interface ReleaseReply {
  readonly status: number;
  readonly payload: { ok: true } | { error: string };
}

export class ReleaseRuns {
  private current: ReleaseRun | undefined;

  constructor(private readonly options: ReleaseRunsOptions) {}

  start(mode: ReleaseRun["mode"]): ReleaseReply {
    if (this.current && !this.current.done) return refuse(409, `a ${this.current.mode === "plan" ? "dry run" : "release"} is already running`);
    if (mode === "release") {
      const plan = this.current;
      if (!plannedVersion(plan)) return refuse(409, "nothing planned to ship -- run the dry run first");
      if (plan && this.options.now() - plan.startedAt > PLAN_MAX_AGE_MS) return refuse(409, "the dry run is over 30 minutes old -- plan again");
    }
    // Both start the run before answering (an async body runs up to its first await), so a second
    // press finds this one running.
    if (mode === "plan") void this.execute(mode);
    else if (isDraining()) return refuse(503, new ShuttingDown().message);
    else void protect("mobile release", () => this.execute(mode));
    return { status: 202, payload: { ok: true } };
  }

  private async execute(mode: ReleaseRun["mode"]): Promise<void> {
    const lines: string[] = [];
    this.show({ mode, startedAt: this.options.now(), lines: [], done: false, exit: null });
    const result = await this.options.run(mode === "plan" ? [this.options.script, "--dry-run"] : [this.options.script], {
      cwd: this.options.cwd,
      env: this.options.env,
      mergeStderr: true,
      timeoutMs: mode === "plan" ? PLAN_TIMEOUT_MS : RELEASE_TIMEOUT_MS,
      onLine: (line) => {
        lines.push(line);
        if (lines.length > LINE_CAP) lines.splice(0, lines.length - LINE_CAP);
        this.show({ ...this.started(), lines: [...lines] });
      },
    });
    if (result.timedOut) lines.push(`✗ timed out after ${(mode === "plan" ? PLAN_TIMEOUT_MS : RELEASE_TIMEOUT_MS) / 60_000} minutes`);
    this.show({ ...this.started(), lines: [...lines], done: true, exit: result.code });
  }

  private started(): ReleaseRun {
    if (!this.current) throw new Error("no release run started");
    return this.current;
  }

  private show(run: ReleaseRun): void {
    this.current = run;
    this.options.publish(run);
  }
}

const refuse = (status: number, error: string): ReleaseReply => ({ status, payload: { error } });
