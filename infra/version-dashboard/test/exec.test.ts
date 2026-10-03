import { describe, expect, it } from "vitest";
import { keepTail, killRunningCommands, spawnExecutor, STREAMED_OUTPUT_CAP } from "../src/exec.js";

describe("a command that runs past its timeout", () => {
  it("is killed with everything it started, so a grandchild holding the pipes cannot hang the result", async () => {
    // `sh` starts a `sleep` that inherits stdout. Killing only `sh` left the sleep holding the pipe
    // open, `close` never fired, and the promise -- a job's `done`, a machine's slot -- never resolved.
    const result = await Promise.race([
      spawnExecutor(["sh", "-c", "sleep 30; echo never"], { timeoutMs: 200 }),
      new Promise<"hung">((resolve) => setTimeout(() => resolve("hung"), 5_000)),
    ]);
    expect(result).not.toBe("hung");
    expect(result).toMatchObject({ timedOut: true, code: null });
  });
});

describe("a command still running when the process shuts down", () => {
  it("is killed with everything it started, so no restart leaves it behind", async () => {
    const run = spawnExecutor(["sh", "-c", "sleep 30; echo never"], { timeoutMs: 60_000 });
    expect(killRunningCommands()).toBe(1);
    const result = await Promise.race([run, new Promise<"hung">((resolve) => setTimeout(() => resolve("hung"), 5_000))]);
    expect(result).toMatchObject({ code: null, timedOut: false });
    expect(killRunningCommands()).toBe(0);
  });
});

describe("a streamed command's output", () => {
  it("keeps only its tail in the result, behind a marker saying how much went", async () => {
    const lines: string[] = [];
    const result = await spawnExecutor(["sh", "-c", `i=0; while [ $i -lt 20000 ]; do echo line-$i; i=$((i+1)); done`],
      { timeoutMs: 30_000, onLine: (line) => lines.push(line) });
    expect(lines).toHaveLength(20_000);
    expect(result.stdout.length).toBeLessThan(STREAMED_OUTPUT_CAP + 100);
    expect(result.stdout).toMatch(/^\[… \d+ characters truncated …\]\n/);
    expect(result.stdout.trimEnd().endsWith("line-19999")).toBe(true);
  });

  it("counts every truncation into one marker", () => {
    let kept = "";
    for (let i = 0; i < 10; i++) kept = keepTail(kept, "x".repeat(10), 25);
    expect(kept).toBe(`[… 75 characters truncated …]\n${"x".repeat(25)}`);
  });

  it("is kept whole when it is not streamed", async () => {
    const result = await spawnExecutor(["sh", "-c", `head -c 100000 /dev/zero | tr '\\0' a`], { timeoutMs: 30_000 });
    expect(result.stdout).toHaveLength(100_000);
  });
});
