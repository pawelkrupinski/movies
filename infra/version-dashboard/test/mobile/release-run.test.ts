import { afterEach, describe, expect, it } from "vitest";
import type { CommandOptions, CommandResult } from "../../src/exec.js";
import { drain, resetLifecycle, runningWork } from "../../src/lifecycle.js";
import type { ReleaseRun } from "../../src/mobile/model.js";
import { PLAN_MAX_AGE_MS, ReleaseRuns } from "../../src/mobile/release-run.js";
import { NOW } from "./states.js";

const SCRIPT = "/repo/scripts/mobile-ship.sh";
const PLAN_LINES = [
  "10:00:00 main        abc123456  mobile-version.txt 2.0.13",
  "10:00:01 → 2.0.14: main is newer than both stores",
  "10:00:01   iOS      archive + upload → new version → submit for review",
];

/** An executor whose runs stay open until the test finishes them, printing `lines` as they start. */
function scripted() {
  const runs: { argv: readonly string[]; options: CommandOptions; finish: (code: number) => void }[] = [];
  const run = (argv: readonly string[], options: CommandOptions) =>
    new Promise<CommandResult>((resolve) => {
      runs.push({ argv, options, finish: (code) => resolve({ code, stdout: "", stderr: "", timedOut: false }) });
    });
  return { run, runs };
}

function releases(now = () => NOW) {
  const executor = scripted();
  const published: ReleaseRun[] = [];
  const runs = new ReleaseRuns({ script: SCRIPT, cwd: "/repo", env: { JAVA_HOME: "/jdk" }, run: executor.run, now, publish: (run) => published.push(run) });
  return { runs, executor, published, latest: () => published.at(-1) };
}

const settle = () => new Promise((wake) => setTimeout(wake, 0));

afterEach(() => resetLifecycle());

describe("the release button's runs of scripts/mobile-ship.sh", () => {
  it("plans with --dry-run, streaming each line to the page as it arrives", async () => {
    const { runs, executor, latest } = releases();
    expect(runs.start("plan")).toEqual({ status: 202, payload: { ok: true } });
    const [plan] = executor.runs;
    expect(plan?.argv).toEqual([SCRIPT, "--dry-run"]);
    expect(plan?.options).toMatchObject({ cwd: "/repo", env: { JAVA_HOME: "/jdk" }, mergeStderr: true });
    PLAN_LINES.forEach((line) => plan?.options.onLine?.(line));
    expect(latest()).toEqual({ mode: "plan", startedAt: NOW, lines: PLAN_LINES, done: false, exit: null });
    plan?.finish(0);
    await settle();
    expect(latest()).toMatchObject({ done: true, exit: 0 });
  });

  it("refuses to release before a plan, and under a plan with nothing to ship or one that failed", async () => {
    const { runs, executor } = releases();
    expect(runs.start("release").status).toBe(409);
    for (const [lines, code] of [[["10:00:01 → nothing to release: both stores have main"], 0], [PLAN_LINES, 1]] as const) {
      runs.start("plan");
      lines.forEach((line) => executor.runs.at(-1)?.options.onLine?.(line));
      executor.runs.at(-1)?.finish(code);
      await settle();
      expect(runs.start("release").status).toBe(409);
    }
    expect(executor.runs.every((run) => run.argv.includes("--dry-run"))).toBe(true);
  });

  it("releases under a fresh successful plan, without --dry-run, as work a restart waits for", async () => {
    const { runs, executor, latest } = releases();
    runs.start("plan");
    PLAN_LINES.forEach((line) => executor.runs[0]?.options.onLine?.(line));
    executor.runs[0]?.finish(0);
    await settle();
    expect(runs.start("release")).toEqual({ status: 202, payload: { ok: true } });
    await settle();
    const release = executor.runs[1];
    expect(release?.argv).toEqual([SCRIPT]);
    expect(latest()).toMatchObject({ mode: "release", lines: [], done: false });
    expect(runningWork().map((work) => work.label)).toEqual(["mobile release"]);
    // One run at a time: neither a second release nor a plan may start beside it.
    expect(runs.start("release").status).toBe(409);
    expect(runs.start("plan").status).toBe(409);
    const drained = drain();
    release?.finish(0);
    await drained;
    expect(latest()).toMatchObject({ done: true, exit: 0 });
  });

  it("refuses a release under a plan older than PLAN_MAX_AGE_MS: the stores it read have moved on", async () => {
    let now = NOW;
    const { runs, executor } = releases(() => now);
    runs.start("plan");
    PLAN_LINES.forEach((line) => executor.runs[0]?.options.onLine?.(line));
    executor.runs[0]?.finish(0);
    await settle();
    now += PLAN_MAX_AGE_MS + 1;
    expect(runs.start("release")).toMatchObject({ status: 409, payload: { error: expect.stringContaining("plan again") } });
  });

  it("refuses a release once the dashboard is restarting", async () => {
    const { runs, executor } = releases();
    runs.start("plan");
    PLAN_LINES.forEach((line) => executor.runs[0]?.options.onLine?.(line));
    executor.runs[0]?.finish(0);
    await settle();
    await drain();
    expect(runs.start("release").status).toBe(503);
  });
});
