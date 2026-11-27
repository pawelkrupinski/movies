/**
 * The page's release button: runs `scripts/mobile-ship.sh` -- the release lane itself, never a copy
 * of its logic -- and shows its output on every open tab.
 *
 * TWO PRESSES, AS ON THE FLEET PAGE. The first runs `--dry-run`, which reads both stores and prints
 * the version it would ship, changing nothing; only a plan that finished with a version to ship,
 * and is fresh, unlocks the second press, which ships. One run at a time.
 *
 * DETACHED, AND READ BACK FROM A FILE. A release runs for an hour or two, far past launchd's
 * ExitTimeOut, and the dashboard restarts on every merge (autodeploy): a child of this process would
 * hold every restart for hours or be killed mid-upload. So the run is its own session writing to a
 * log in `stateDir`, with its exit code in a file beside it; this process only reads them, and a
 * restarted one picks the console back up (`resume`).
 *
 * UNDER `secrets run`. The vault pins an unlock to the process that asked, so the lane's own
 * `secrets get` would be locked; `secrets` started by this (unlocked) process hands it the keys.
 */
import { spawn } from "node:child_process";
import { closeSync, mkdirSync, openSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { systemEnvironment } from "../host/system.js";
import { NOTES_LANGUAGES, PLAY_NOTES_LIMIT, plannedVersion, type ReleaseNotes, type ReleaseRun } from "./model.js";

/** A plan older than this is not acted on: the stores it read may have moved since. */
export const PLAN_MAX_AGE_MS = 30 * 60_000;
/** The lane logs milestones, not build output, but a runaway must not grow the snapshot without bound. */
export const LINE_CAP = 1000;
const POLL_MS = 1_000;
/** Records the script's exit code once it ends: `$1` is where, the rest is the command. */
const RECORD_EXIT = 'exit_file="$1"; shift; "$@"; echo $? > "$exit_file"';

/** Processes that outlive this one. */
export interface DetachedProcesses {
  /** Starts `argv` in a session of its own, stdout and stderr appended to `log`; returns its pid. */
  spawn(argv: readonly string[], options: { readonly cwd: string; readonly env: Readonly<Record<string, string>>; readonly log: string }): number;
  alive(pid: number): boolean;
}

export const realDetachedProcesses: DetachedProcesses = {
  spawn(argv, { cwd, env, log }) {
    const [command, ...args] = argv;
    if (!command) throw new Error("empty command");
    const out = openSync(log, "a");
    try {
      const child = spawn(command, args, { cwd, env: { ...systemEnvironment(), ...env }, detached: true, stdio: ["ignore", out, out] });
      child.unref();
      if (!child.pid) throw new Error(`could not start ${command}`);
      return child.pid;
    } finally {
      closeSync(out);
    }
  },
  alive(pid) {
    try {
      process.kill(pid, 0);
      return true;
    } catch (error) {
      return (error as NodeJS.ErrnoException).code === "EPERM";
    }
  },
};

export interface ReleaseRunsOptions {
  /** scripts/mobile-ship.sh */
  readonly script: string;
  readonly cwd: string;
  /** Added to the run's environment: launchd's PATH has no JDK or Android SDK. */
  readonly env: Readonly<Record<string, string>>;
  /** The vault CLI and the keys it hands the lane. */
  readonly secrets: { readonly bin: string; readonly namespace: string; readonly names: readonly string[] };
  /** Where the run's state, log and exit code live; outside the repository. */
  readonly stateDir: string;
  readonly processes: DetachedProcesses;
  readonly now: () => number;
  /** Shows a run on the page. */
  readonly publish: (run: ReleaseRun) => void;
  readonly pollMs?: number;
}

export interface ReleaseReply {
  readonly status: number;
  readonly payload: { ok: true } | { error: string };
}

/** What `run.json` holds: enough to find the run again after a restart. */
interface Started {
  readonly mode: ReleaseRun["mode"];
  readonly startedAt: number;
  readonly pid: number;
}

export class ReleaseRuns {
  private started: Started | null = null;
  private current: ReleaseRun | undefined;
  private timer: ReturnType<typeof setInterval> | null = null;

  constructor(private readonly options: ReleaseRunsOptions) {}

  private file(name: "run.json" | "output.log" | "exit"): string {
    return join(this.options.stateDir, name);
  }

  /** `notes`: the release's "What's new" by language (ignored for a plan); blank ones are dropped. */
  start(mode: ReleaseRun["mode"], notes: Readonly<Record<string, unknown>> = {}): ReleaseReply {
    this.poll();
    const refusal = this.refusal(mode);
    if (refusal) return refuse(409, refusal);
    const typed = mode === "release" ? releaseNotes(notes) : {};
    if (typeof typed === "string") return refuse(400, typed);
    const notesDir = this.freshState(typed);
    const flags = mode === "plan" ? ["--dry-run"] : Object.keys(typed).length ? ["--notes-dir", notesDir] : [];
    const { script, cwd, env, secrets, processes } = this.options;
    const argv = [secrets.bin, "run", secrets.namespace, ...secrets.names, "--", "/bin/sh", "-c", RECORD_EXIT, "mobile-ship", this.file("exit"), script, ...flags];
    let pid: number;
    try {
      pid = processes.spawn(argv, { cwd, env, log: this.file("output.log") });
    } catch (error) {
      return refuse(500, error instanceof Error ? error.message : String(error));
    }
    this.started = { mode, startedAt: this.options.now(), pid };
    writeFileSync(this.file("run.json"), JSON.stringify(this.started));
    this.poll();
    return { status: 202, payload: { ok: true } };
  }

  /** Why a `mode` run cannot start now, or null: one run at a time, and a release only under a fresh
   * plan that found a version to ship. */
  private refusal(mode: ReleaseRun["mode"]): string | null {
    const current = this.current;
    if (current && !current.done) return `a ${current.mode === "plan" ? "dry run" : "release"} is already running`;
    if (mode === "plan") return null;
    if (!current || !plannedVersion(current)) return "nothing planned to ship -- run the dry run first";
    if (this.options.now() - current.startedAt > PLAN_MAX_AGE_MS) return "the dry run is over 30 minutes old -- plan again";
    return null;
  }

  /** Clears the last run's exit code and log and writes this run's notes; returns the notes' directory.
   * The notes are written afresh every run, so one release's text never rides along on the next. */
  private freshState(notes: ReleaseNotes): string {
    const { stateDir } = this.options;
    mkdirSync(stateDir, { recursive: true });
    rmSync(this.file("exit"), { force: true });
    writeFileSync(this.file("output.log"), "");
    const notesDir = join(stateDir, "notes");
    rmSync(notesDir, { recursive: true, force: true });
    mkdirSync(notesDir);
    for (const [language, text] of Object.entries(notes)) writeFileSync(join(notesDir, `${language}.txt`), text);
    return notesDir;
  }

  /** After a restart: show the newest run again, and follow it if it is still going. */
  resume(): void {
    try {
      this.started = JSON.parse(readFileSync(this.file("run.json"), "utf8")) as Started;
    } catch {
      return; // never started, or unreadable: nothing to show
    }
    this.poll();
  }

  /** Reads the run's log and exit code into the page; follows it while it runs. */
  poll(): void {
    const started = this.started;
    if (!started) return;
    const exitText = readOr(this.file("exit"), null);
    const done = exitText !== null || !this.options.processes.alive(started.pid);
    const lines = (readOr(this.file("output.log"), "") ?? "").split("\n");
    if (lines.at(-1) === "") lines.pop();
    const run: ReleaseRun = {
      mode: started.mode,
      startedAt: started.startedAt,
      lines: lines.slice(-LINE_CAP),
      done,
      exit: recordedExit(exitText),
    };
    this.current = run;
    this.options.publish(run);
    if (done) this.stop();
    else this.timer ??= unref(setInterval(() => this.poll(), this.options.pollMs ?? POLL_MS));
  }

  stop(): void {
    if (this.timer) clearInterval(this.timer);
    this.timer = null;
  }
}

/** The non-blank notes, trimmed; or why they cannot be shipped. */
function releaseNotes(notes: Readonly<Record<string, unknown>>): ReleaseNotes | string {
  const typed: ReleaseNotes = {};
  for (const [language, value] of Object.entries(notes)) {
    if (!(NOTES_LANGUAGES as readonly string[]).includes(language)) return `no store language ${language}`;
    if (typeof value !== "string") return `the ${language} notes are not text`;
    const text = value.trim();
    if (text.length > PLAY_NOTES_LIMIT) return `the ${language} notes are ${text.length} characters; Play takes ${PLAY_NOTES_LIMIT}`;
    if (text) typed[language as keyof ReleaseNotes] = text;
  }
  return typed;
}

/** The exit code `RECORD_EXIT` wrote; null before it wrote one, or for text that is no number. */
function recordedExit(text: string | null): number | null {
  if (text === null) return null;
  const code = Number.parseInt(text.trim(), 10);
  return Number.isNaN(code) ? null : code;
}

function readOr<T>(path: string, fallback: T): string | T {
  try {
    return readFileSync(path, "utf8");
  } catch {
    return fallback;
  }
}

function unref<T extends { unref?: () => unknown }>(timer: T): T {
  timer.unref?.();
  return timer;
}

const refuse = (status: number, error: string): ReleaseReply => ({ status, payload: { error } });
