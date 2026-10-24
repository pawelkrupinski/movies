import { mkdtempSync, readdirSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import type { ReleaseRun } from "../../src/mobile/model.js";
import { PLAN_MAX_AGE_MS, ReleaseRuns, type DetachedProcesses } from "../../src/mobile/release-run.js";
import { NOW } from "./states.js";

const SCRIPT = "/repo/scripts/mobile-ship.sh";
const PLAN_LINES = [
  "10:00:00 main        abc123456  mobile-version.txt 2.0.13",
  "10:00:01 → 2.0.14: main is newer than both stores",
  "10:00:01   iOS      archive + upload → new version → submit for review",
];

let stateDir: string;
beforeEach(() => {
  stateDir = mkdtempSync(join(tmpdir(), "mobile-release-"));
});
afterEach(() => rmSync(stateDir, { recursive: true, force: true }));

/** Processes the test plays: it writes their output and exit code where the real ones would. */
function processes() {
  const started: { argv: readonly string[]; cwd: string; env: Readonly<Record<string, string>>; log: string; pid: number }[] = [];
  const dead = new Set<number>();
  const fake: DetachedProcesses = {
    spawn(argv, options) {
      const pid = 4000 + started.length;
      started.push({ argv, ...options, pid });
      return pid;
    },
    alive: (pid) => pid >= 4000 && !dead.has(pid),
  };
  return { fake, started, dead };
}

function releases(fake: DetachedProcesses, now = () => NOW) {
  const published: ReleaseRun[] = [];
  const runs = new ReleaseRuns({
    script: SCRIPT,
    cwd: "/repo",
    env: { JAVA_HOME: "/jdk" },
    secrets: { bin: "/bin/secrets", namespace: "movies", names: ["APP_STORE_KEY_ID", "PLAY_SERVICE_ACCOUNT_JSON"] },
    stateDir,
    processes: fake,
    now,
    publish: (run) => published.push(run),
    pollMs: 60_000,
  });
  return { runs, published, latest: () => published.at(-1) };
}

const output = (lines: readonly string[]) => writeFileSync(join(stateDir, "output.log"), lines.map((line) => `${line}\n`).join(""));
const exit = (code: number) => writeFileSync(join(stateDir, "exit"), `${code}\n`);

/** A dry run that printed `lines` and exited `code`. */
function planned(runs: ReleaseRuns, lines: readonly string[] = PLAN_LINES, code = 0) {
  expect(runs.start("plan").status).toBe(202);
  output(lines);
  exit(code);
  runs.poll();
}

describe("the release button's runs of scripts/mobile-ship.sh", () => {
  it("plans with --dry-run under `secrets run`, detached, its output read back from the log", () => {
    const { fake, started } = processes();
    const { runs, latest } = releases(fake);
    expect(runs.start("plan")).toEqual({ status: 202, payload: { ok: true } });
    const [plan] = started;
    // The vault hands the lane its keys: `secrets` itself is this process's child, so the unlock
    // this dashboard holds covers it. The shell after it records the script's exit code.
    expect(plan?.argv.slice(0, 7)).toEqual(["/bin/secrets", "run", "movies", "APP_STORE_KEY_ID", "PLAY_SERVICE_ACCOUNT_JSON", "--", "/bin/sh"]);
    expect(plan?.argv.slice(-3)).toEqual([join(stateDir, "exit"), SCRIPT, "--dry-run"]);
    expect(plan).toMatchObject({ cwd: "/repo", env: { JAVA_HOME: "/jdk" }, log: join(stateDir, "output.log") });
    expect(latest()).toEqual({ mode: "plan", startedAt: NOW, lines: [], done: false, exit: null });

    output(PLAN_LINES.slice(0, 2));
    runs.poll();
    expect(latest()).toMatchObject({ lines: PLAN_LINES.slice(0, 2), done: false });
    output(PLAN_LINES);
    exit(0);
    runs.poll();
    expect(latest()).toEqual({ mode: "plan", startedAt: NOW, lines: PLAN_LINES, done: true, exit: 0 });
    runs.stop();
  });

  it("a run whose process died without an exit code is done, with none", () => {
    const { fake, started, dead } = processes();
    const { runs, latest } = releases(fake);
    runs.start("plan");
    output(["Error: locked — run `secrets unlock movies`"]);
    dead.add(started[0]?.pid ?? 0);
    runs.poll();
    expect(latest()).toMatchObject({ done: true, exit: null, lines: ["Error: locked — run `secrets unlock movies`"] });
  });

  it("refuses to release before a plan, and under a plan with nothing to ship or one that failed", () => {
    const { fake, started } = processes();
    const { runs } = releases(fake);
    expect(runs.start("release").status).toBe(409);
    planned(runs, ["10:00:01 → nothing to release: both stores have main"]);
    expect(runs.start("release").status).toBe(409);
    planned(runs, PLAN_LINES, 1);
    expect(runs.start("release").status).toBe(409);
    expect(started.every((run) => run.argv.at(-1) === "--dry-run")).toBe(true);
  });

  it("releases under a fresh successful plan, without --dry-run, one run at a time", () => {
    const { fake, started } = processes();
    const { runs, latest } = releases(fake);
    planned(runs);
    expect(runs.start("release")).toEqual({ status: 202, payload: { ok: true } });
    expect(started[1]?.argv.at(-1)).toBe(SCRIPT);
    // The new run starts from an empty console: the plan's output and exit code are gone.
    runs.poll();
    expect(latest()).toMatchObject({ mode: "release", lines: [], done: false, exit: null });
    expect(runs.start("release").status).toBe(409);
    expect(runs.start("plan").status).toBe(409);
    runs.stop();
  });

  it("hands the lane the What's new text typed on the page, one file per language, and refuses what Play would", () => {
    const { fake, started } = processes();
    const { runs } = releases(fake);
    planned(runs);
    expect(runs.start("release", { pl: "x".repeat(501) })).toMatchObject({ status: 400, payload: { error: expect.stringContaining("500") } });
    expect(runs.start("release", { fr: "Corrections" }).status).toBe(400);
    expect(runs.start("release", { pl: "  Nowe kina.  ", en: "New cinemas.", de: "   " })).toEqual({ status: 202, payload: { ok: true } });
    const notesDir = join(stateDir, "notes");
    expect(started[1]?.argv.slice(-3)).toEqual([SCRIPT, "--notes-dir", notesDir]);
    expect(readdirSync(notesDir).sort()).toEqual(["en.txt", "pl.txt"]);
    expect(readFileSync(join(notesDir, "pl.txt"), "utf8")).toBe("Nowe kina.");
    runs.stop();
  });

  it("without any text the lane keeps its generic line, and an earlier release's text is not reused", () => {
    const { fake, started } = processes();
    const { runs } = releases(fake);
    planned(runs);
    runs.start("release", { pl: "Nowe kina." });
    exit(0);
    runs.poll();
    planned(runs);
    runs.start("release", {});
    expect(started.at(-1)?.argv.at(-1)).toBe(SCRIPT);
    expect(readdirSync(join(stateDir, "notes"))).toEqual([]);
  });

  it("refuses a release under a plan older than PLAN_MAX_AGE_MS: the stores it read have moved on", () => {
    let now = NOW;
    const { fake } = processes();
    const { runs } = releases(fake, () => now);
    planned(runs);
    now += PLAN_MAX_AGE_MS + 1;
    expect(runs.start("release")).toMatchObject({ status: 409, payload: { error: expect.stringContaining("plan again") } });
  });

  it("a restarted dashboard picks a release still running back up, and will not start a second beside it", () => {
    const { fake, started } = processes();
    const before = releases(fake);
    planned(before.runs);
    before.runs.start("release");
    output(["10:05:00 [ios] ▸ archiving"]);
    before.runs.stop();

    const after = releases(fake);
    after.runs.resume();
    expect(after.latest()).toEqual({ mode: "release", startedAt: NOW, lines: ["10:05:00 [ios] ▸ archiving"], done: false, exit: null });
    expect(after.runs.start("plan").status).toBe(409);
    exit(0);
    after.runs.poll();
    expect(after.latest()).toMatchObject({ done: true, exit: 0 });
    expect(started).toHaveLength(2);
    after.runs.stop();
  });

  it("resumes nothing when no run was ever started", () => {
    const { fake } = processes();
    const { runs, published } = releases(fake);
    runs.resume();
    expect(published).toEqual([]);
  });
});
