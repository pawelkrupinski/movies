/**
 * THE ROSTER -- who is supposed to exist.
 *
 * THE FLAKE IS THE ROSTER, NOT PROMETHEUS, and the direction matters. Deriving the list from what
 * is currently reporting would make a host that has died disappear from the page entirely -- the
 * single most important thing it could tell you, rendered as an absence nobody notices. Taking the
 * roster from the flake means a declared host that stops publishing shows up as a row that says so.
 */
import { createHash } from "node:crypto";
import { mkdirSync, readdirSync, readFileSync, renameSync, statSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { binary, CACHE_DIR } from "../config.js";
import { runCommand } from "../exec.js";
import { ago } from "../age.js";
import type { Declaration } from "./model.js";

export type Machines = Readonly<Record<string, Declaration>>;

/** `nix` is not on launchd's PATH; the multi-user install puts it here. */
export const NIX_FALLBACK = "/nix/var/nix/profiles/default/bin/nix";

/**
 * GENEROUS BECAUSE IT IS PAID RARELY, and because the alternative to waiting is worse. The
 * evaluation runs off the page's critical path and, once it lands, not again until the flake
 * changes -- so the only thing a tight cap buys is a failed read. It was 240s, and this laptop
 * routinely runs test suites that push its load average into the hundreds; every evaluation that
 * started during one of those was killed at 240s, and the page then had nothing to show.
 */
export const NIX_TIMEOUT_MS = 900_000;
/** After a FAILED evaluation, the floor before another is tried. */
export const ROSTER_RETRY_MS = 300_000;

/**
 * WHAT AN EVALUATION READS, and therefore what has to change before one is worth repeating: the
 * flake, its lock, and the host/module tree they import. NOT the repository's HEAD -- `nix eval .`
 * in infra/ resolves to the ENCLOSING git checkout, which is this repository's Scala application
 * (~18k tracked files), so keying on its commit would re-evaluate the fleet roster on every
 * application commit, which is most commits.
 */
export const ROSTER_INPUTS = ["flake.nix", "flake.lock", "nix"] as const;

export const ROSTER_EXPRESSION =
  "cfgs: builtins.mapAttrs (n: c: {"
  + " hostName = c.config.networking.hostName;"
  + " privateAddress = c.config.fleet.privateAddress;"
  + " publicAddress = c.config.fleet.publicAddress;"
  + " role = c.config.fleet.role;"
  + " environment = c.config.fleet.environment;"
  + "}) cfgs";

/** Every host the flake declares, with the addresses, role and environment it declares. */
export async function flakeMachines(infraDir: string): Promise<{ machines: Machines } | { error: string }> {
  const nix = binary("nix", [NIX_FALLBACK]);
  if (!nix) return { error: `nix eval failed: no \`nix\` on PATH or at ${NIX_FALLBACK}` };
  const result = await runCommand(
    [nix, "--extra-experimental-features", "nix-command flakes", "eval", "--json", ".#nixosConfigurations", "--apply", ROSTER_EXPRESSION],
    { cwd: infraDir, timeoutMs: NIX_TIMEOUT_MS },
  );
  if (result.code !== 0) {
    const why = result.timedOut ? `timed out after ${NIX_TIMEOUT_MS / 1000}s` : result.stderr.trim().split("\n").pop() || "unknown";
    return { error: `nix eval failed: ${why}` };
  }
  try {
    return { machines: JSON.parse(result.stdout) as Machines };
  } catch (error) {
    return { error: `nix eval returned unparseable JSON: ${error instanceof Error ? error.message : String(error)}` };
  }
}

/** Every file under `path`, in os.walk's order -- a directory's own files, sorted, then each
 * subdirectory in turn -- so the stamp equals the one the Python page wrote and a restart onto this
 * code reuses its cached roster instead of paying for an evaluation. */
function inputFiles(path: string): string[] {
  let isDirectory = false;
  try {
    isDirectory = statSync(path).isDirectory();
  } catch {
    return [path];
  }
  if (!isDirectory) return [path];
  const entries = readdirSync(path, { withFileTypes: true }).sort((a, b) => (a.name < b.name ? -1 : a.name > b.name ? 1 : 0));
  return [
    ...entries.filter((entry) => !entry.isDirectory()).map((entry) => join(path, entry.name)),
    ...entries.filter((entry) => entry.isDirectory()).flatMap((entry) => inputFiles(join(path, entry.name))),
  ];
}

/**
 * A cheap stamp over the files a roster evaluation reads, used to decide whether to repeat it.
 *
 * SIZE AND MTIME RATHER THAN THE BYTES, because this runs every few seconds and its whole job is to
 * be orders of magnitude cheaper than the evaluation it is deciding to skip. It cannot miss an edit
 * made by an editor, a checkout or a rebase -- all of them rewrite the file -- only one that
 * restores a file to its exact previous size and modification time, which is not something that
 * happens to a flake by accident.
 */
export function flakeFingerprint(infraDir: string): string {
  const stamp = createHash("sha256");
  for (const entry of ROSTER_INPUTS) {
    for (const path of inputFiles(join(infraDir, entry))) {
      try {
        const stat = statSync(path, { bigint: true });
        stamp.update(`${path}:${stat.size}:${stat.mtimeNs}\n`);
      } catch {
        stamp.update(`${path}:gone\n`);
      }
    }
  }
  return stamp.digest("hex");
}

interface Remembered {
  readonly fingerprint: string | null;
  readonly machines: Machines;
  /** Unix seconds, as the Python page wrote it -- the file survives the port. */
  readonly evaluatedAt: number;
}

export interface RosterOptions {
  readonly infraDir: string;
  /** WHERE THE LAST ROSTER SURVIVES A RESTART. */
  readonly cacheFile?: string;
  readonly evaluate?: (infraDir: string) => Promise<{ machines: Machines } | { error: string }>;
  readonly fingerprint?: (infraDir: string) => string;
  readonly now?: () => number;
  /** A background re-read landed: the page should recompute. */
  readonly onChange?: () => void;
}

export interface RosterAnswer {
  readonly machines: Machines;
  readonly error: string | null;
}

/**
 * The declared fleet, evaluated only when the flake declaring it has actually changed.
 *
 * WHY IT IS CACHED AT ALL: the evaluation is `nix eval` over every NixOS configuration, reached
 * through a flakeref that resolves to a 1.1GB git checkout. Measured on an idle laptop it takes 86
 * seconds, almost none of it CPU -- it is nix copying the enclosing repository's tree into the
 * store. Run on every read, an evaluation was in flight essentially all the time, each one queueing
 * behind the previous one's git-fetch and eval-cache locks; the page said "roster unavailable" on
 * every load while every machine was healthy, and the laptop paid for it continuously.
 *
 * THE ROSTER IS A DECLARATION, NOT A READING. It changes when somebody edits infra/nix, not when a
 * machine does something -- so it is read then, and the running state keeps its own cadence. A
 * failure is not retried on every read either: continuous retries were the fault, not the
 * diagnosis. And the answer outlives the process that read it, so a restart is not a fleet with no
 * machines in it.
 *
 * AN EVALUATION THAT DOES HAVE TO RUN RUNS BEHIND THE PAGE, not in front of it -- once there is any
 * roster to show, a re-read happens in the background and the page carries on with the one it has.
 * The single exception is the first read of all, which is waited for, because the alternative to
 * waiting is a page with no fleet on it.
 */
export class RosterKeeper {
  private fingerprint: string | null = null;
  private machines: Machines | null = null;
  private evaluatedAt = 0;
  private error: string | null = null;
  private failedAt = 0;
  private recalled = false;
  private evaluating: Promise<void> | null = null;
  private readonly cacheFile: string;
  private readonly now: () => number;

  constructor(private readonly options: RosterOptions) {
    this.cacheFile = options.cacheFile ?? process.env.KINOWO_ROSTER_CACHE ?? join(CACHE_DIR, "roster.json");
    this.now = options.now ?? Date.now;
  }

  async read(): Promise<RosterAnswer> {
    const fingerprint = (this.options.fingerprint ?? flakeFingerprint)(this.options.infraDir);
    if (!this.recalled) {
      this.recalled = true;
      const remembered = this.recall();
      if (remembered) {
        this.fingerprint = remembered.fingerprint;
        this.machines = remembered.machines;
        this.evaluatedAt = remembered.evaluatedAt * 1000;
      }
    }
    if (this.machines && this.fingerprint === fingerprint) return { machines: this.machines, error: null };
    const due = !this.evaluating && (!this.error || this.now() - this.failedAt >= ROSTER_RETRY_MS);
    if (!due) return this.last();
    const evaluation = this.evaluate(fingerprint);
    if (this.machines) {
      void evaluation.then(() => this.options.onChange?.());
      return this.last();
    }
    // NOTHING TO SHOW, so this one is worth waiting for: the alternative is a page with no fleet on
    // it. Every later evaluation has last time's answer to fall back on and runs behind the page.
    await evaluation;
    return { machines: this.machines ?? {}, error: this.error };
  }

  /** Tests: whether a re-read is in flight, and the promise to wait for it. */
  pending(): Promise<void> | null {
    return this.evaluating;
  }

  /**
   * What to answer with when this read has no answer of its own.
   *
   * A ROSTER THAT WAS READ AN HOUR AGO IS STILL THE ROSTER. The page's subject is what the machines
   * are doing now, and it reads that from Prometheus regardless; dropping every row because `nix`
   * could not be run is throwing away the healthy half of the page along with the broken half. The
   * error is still shown, and it says how old the rows are.
   */
  private last(): RosterAnswer {
    if (!this.machines) return { machines: {}, error: this.error };
    return {
      machines: this.machines,
      error: `${this.error ?? "roster evaluation is pending"} — the machines below are the roster read ${ago(this.evaluatedAt / 1000, this.now() / 1000)}, which is the last one that evaluated`,
    };
  }

  private evaluate(fingerprint: string): Promise<void> {
    this.evaluating = (async () => {
      try {
        const answer = await (this.options.evaluate ?? flakeMachines)(this.options.infraDir);
        if ("error" in answer) {
          this.error = answer.error;
          this.failedAt = this.now();
          return;
        }
        this.error = null;
        this.fingerprint = fingerprint;
        this.machines = answer.machines;
        this.evaluatedAt = this.now();
        this.remember();
      } finally {
        this.evaluating = null;
      }
    })();
    return this.evaluating;
  }

  /**
   * THE PROCESS RESTARTS MORE OFTEN THAN THE FLEET CHANGES -- launchd's KeepAlive, a laptop
   * rebooting, `npm run restart`. Without this, every one of those is a page with no machines on
   * it until an 86-second evaluation finishes, and if that one times out (which is what happens
   * when the laptop is busy, which is when it is restarted) the page has nothing to show for as
   * long as the retry floor lasts.
   */
  private remember(): void {
    try {
      mkdirSync(dirname(this.cacheFile), { recursive: true });
      const body: Remembered = { fingerprint: this.fingerprint, machines: this.machines ?? {}, evaluatedAt: this.evaluatedAt / 1000 };
      writeFileSync(`${this.cacheFile}.tmp`, JSON.stringify({ fingerprint: body.fingerprint, machines: body.machines, evaluated_at: body.evaluatedAt }));
      renameSync(`${this.cacheFile}.tmp`, this.cacheFile);
    } catch {
      // a dashboard that cannot write its cache is still a dashboard
    }
  }

  /** The roster this process was left with, or nothing at all if that cannot be read. */
  private recall(): Remembered | null {
    try {
      const saved = JSON.parse(readFileSync(this.cacheFile, "utf8")) as { fingerprint?: unknown; machines?: unknown; evaluated_at?: unknown };
      const machines = saved.machines;
      if (machines && typeof machines === "object" && !Array.isArray(machines) && Object.keys(machines).length) {
        return {
          fingerprint: typeof saved.fingerprint === "string" ? saved.fingerprint : null,
          machines: machines as Machines,
          evaluatedAt: Number(saved.evaluated_at) || 0,
        };
      }
    } catch {
      // absent, truncated, or written by a version that meant something else by it
    }
    return null;
  }
}
