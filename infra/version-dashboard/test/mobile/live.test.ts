import Fastify from "fastify";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { nextBuildDelayMs, REBUILD_MS, RETRY_FLOOR_MS } from "../../src/mobile/build.js";
import { MobileLive, SELF_RESTART_AFTER, SELF_RESTART_GRACE_MS } from "../../src/mobile/live.js";
import { allPlatformsNetworkFailed, type MobileState } from "../../src/mobile/model.js";
import { createMobilePage, releaseEnvironment } from "../../src/mobile/page.js";
import { ReleaseRuns } from "../../src/mobile/release-run.js";
import { allFailed, NOW, oneOk, state } from "./states.js";

beforeEach(() => {
  vi.useFakeTimers();
  vi.spyOn(console, "error").mockImplementation(() => {});
});
afterEach(() => {
  vi.useRealTimers();
  vi.restoreAllMocks();
});

/** A live page whose builds answer from `next()`, counting them and any restart. */
function liveWith(next: () => MobileState | Promise<MobileState>) {
  let builds = 0;
  const restarts: number[] = [];
  const live = new MobileLive(null, {
    now: () => NOW,
    build: async () => {
      builds++;
      return next();
    },
    restart: () => restarts.push(builds),
  });
  return { live, builds: () => builds, restarts };
}

describe("pacing", () => {
  it("requires EVERY platform to have failed for a network reason", () => {
    expect(allPlatformsNetworkFailed(allFailed())).toBe(true);
    expect(allPlatformsNetworkFailed(oneOk())).toBe(false);
    expect(allPlatformsNetworkFailed(state([]))).toBe(false);
  });

  it("retries at the floor after a throw or an all-network failure, else the full cadence", () => {
    expect(nextBuildDelayMs(null)).toBe(RETRY_FLOOR_MS);
    expect(nextBuildDelayMs(allFailed())).toBe(RETRY_FLOOR_MS);
    expect(nextBuildDelayMs(oneOk())).toBe(REBUILD_MS);
    expect([RETRY_FLOOR_MS, REBUILD_MS]).toEqual([60_000, 600_000]);
  });
});

describe("the live mobile state", () => {
  it("builds at once, then rebuilds an all-network failure after the floor, not the full cadence", async () => {
    const page = liveWith(allFailed);
    page.live.start();
    await vi.advanceTimersByTimeAsync(0);
    expect(page.builds()).toBe(1);
    expect(page.live.store.get().state.ready).toBe(true);
    await vi.advanceTimersByTimeAsync(RETRY_FLOOR_MS);
    expect(page.builds()).toBe(2);
    page.live.stop();
  });

  it("does not rebuild a healthy state before the full cadence", async () => {
    const page = liveWith(oneOk);
    page.live.start();
    await vi.advanceTimersByTimeAsync(RETRY_FLOOR_MS + 1_000);
    expect(page.builds()).toBe(1);
    await vi.advanceTimersByTimeAsync(REBUILD_MS);
    expect(page.builds()).toBe(2);
    page.live.stop();
  });

  it("keeps the last platforms when a build throws, says so, and retries at the floor", async () => {
    let calls = 0;
    const page = liveWith(() => {
      if (++calls === 2) throw new Error("git exploded");
      return oneOk();
    });
    page.live.start();
    await vi.advanceTimersByTimeAsync(0);
    await page.live.rebuild();
    const shown = page.live.store.get().state;
    expect(shown.platforms).toEqual(oneOk().platforms);
    expect(shown.buildError).toBe("Error: git exploded");
    await vi.advanceTimersByTimeAsync(RETRY_FLOOR_MS);
    expect(page.builds()).toBe(3);
    expect(page.live.store.get().state.buildError).toBeNull();
    page.live.stop();
  });

  it("runs one build for two presses that arrive together", async () => {
    const page = liveWith(oneOk);
    await Promise.all([page.live.rebuild(), page.live.rebuild()]);
    expect(page.builds()).toBe(1);
    page.live.stop();
  });

  it("resets the failure count on a success", async () => {
    const page = liveWith(oneOk);
    page.live.consecutiveNetworkFailures = SELF_RESTART_AFTER - 1;
    await page.live.rebuild();
    expect(page.live.consecutiveNetworkFailures).toBe(0);
    page.live.stop();
  });

  it("never advances the count for a non-network failure", async () => {
    const page = liveWith(oneOk); // Android 401s
    page.live.consecutiveNetworkFailures = SELF_RESTART_AFTER - 1;
    await page.live.rebuild();
    await vi.advanceTimersByTimeAsync(SELF_RESTART_GRACE_MS * 2);
    expect(page.live.consecutiveNetworkFailures).toBe(0);
    expect(page.restarts).toEqual([]);
    page.live.stop();
  });

  it("restarts itself at the threshold -- after the grace, not on the spot", async () => {
    const page = liveWith(allFailed);
    page.live.consecutiveNetworkFailures = SELF_RESTART_AFTER - 1;
    await page.live.rebuild();
    expect(page.live.store.get().state.platforms).toEqual(allFailed().platforms);
    expect(page.restarts).toEqual([]); // whatever was mid-response gets to finish
    await vi.advanceTimersByTimeAsync(SELF_RESTART_GRACE_MS);
    expect(page.restarts).toEqual([1]);
    page.live.stop();
  });

  it("does not restart below the threshold", async () => {
    const page = liveWith(allFailed);
    await page.live.rebuild();
    await vi.advanceTimersByTimeAsync(SELF_RESTART_GRACE_MS * 2);
    expect(page.live.consecutiveNetworkFailures).toBe(1);
    expect(page.restarts).toEqual([]);
    page.live.stop();
  });
});

describe("the page", () => {
  it("rebuilds when the Mac wakes and when Refresh is posted", async () => {
    const page = liveWith(oneOk);
    let wake = () => {};
    const mobile = createMobilePage({ onWake: (listener) => (wake = listener) }, page.live);
    expect(mobile).toMatchObject({ name: "mobile", path: "/mobile", title: "Mobile releases" });
    wake();
    await vi.advanceTimersByTimeAsync(0);
    expect(page.builds()).toBe(1);

    const app = Fastify();
    mobile.routes?.(app);
    const reply = await app.inject({ method: "POST", url: "/mobile/refresh" });
    expect(reply.json()).toEqual({ ok: true });
    await vi.advanceTimersByTimeAsync(0);
    expect(page.builds()).toBe(2);
    // No GET does anything: a prefetch or reload must not be able to trigger a build.
    expect((await app.inject({ method: "GET", url: "/mobile/refresh" })).statusCode).toBe(404);
    await app.close();
    page.live.stop();
  });

  it("unlocks the vault for this process when asked, then rebuilds; a refused prompt is reported", async () => {
    const page = liveWith(oneOk);
    let unlocks = 0;
    let refuse = false;
    const mobile = createMobilePage({ onWake: () => {} }, page.live, async () => {
      unlocks += 1;
      if (refuse) throw new Error("Touch ID cancelled");
    });
    const app = Fastify();
    mobile.routes?.(app);
    expect((await app.inject({ method: "POST", url: "/mobile/unlock-vault" })).json()).toEqual({ ok: true });
    await vi.advanceTimersByTimeAsync(0);
    expect(unlocks).toBe(1);
    expect(page.builds()).toBe(1);
    refuse = true;
    expect((await app.inject({ method: "POST", url: "/mobile/unlock-vault" })).json()).toEqual({ ok: false, error: "Touch ID cancelled" });
    expect(page.builds()).toBe(1);
    expect((await app.inject({ method: "GET", url: "/mobile/unlock-vault" })).statusCode).toBe(404);
    await app.close();
    page.live.stop();
  });

  it("starts the release script's runs from POST /mobile/release, and keeps their output across rebuilds", async () => {
    // Fastify reads a request body on real timers: faked, the inject never answers.
    vi.useRealTimers();
    const stateDir = mkdtempSync(join(tmpdir(), "mobile-release-"));
    const page = liveWith(oneOk);
    const ran: (readonly string[])[] = [];
    const releases = new ReleaseRuns({
      script: "/repo/scripts/mobile-ship.sh",
      cwd: "/repo",
      env: {},
      secrets: { bin: "/bin/secrets", namespace: "movies", names: [] },
      stateDir,
      processes: {
        spawn: (argv, { log }) => {
          ran.push(argv);
          writeFileSync(log, "10:00:01 → 2.0.14: main is newer\n");
          writeFileSync(join(stateDir, "exit"), "0\n");
          return 4000;
        },
        alive: () => false,
      },
      now: () => NOW,
      publish: (run) => page.live.showRelease(run),
    });
    const mobile = createMobilePage({ onWake: () => {} }, page.live, async () => {}, releases);
    const app = Fastify();
    mobile.routes?.(app);
    expect((await app.inject({ method: "POST", url: "/mobile/release", payload: { mode: "plan" } })).statusCode).toBe(202);
    expect(ran.map((argv) => argv.at(-1))).toEqual(["--dry-run"]);
    await page.live.rebuild();
    expect(page.live.store.get().state.release).toMatchObject({ mode: "plan", done: true, exit: 0, lines: ["10:00:01 → 2.0.14: main is newer"] });
    expect((await app.inject({ method: "POST", url: "/mobile/release", payload: { mode: "ship" } })).statusCode).toBe(400);
    expect((await app.inject({ method: "GET", url: "/mobile/release" })).statusCode).toBe(404);
    await app.close();
    page.live.stop();
    rmSync(stateDir, { recursive: true, force: true });
  });
});

describe("releaseEnvironment", () => {
  it("puts sdkman's JDK first on PATH and names the Android SDK, where they exist", () => {
    const env = releaseEnvironment("/home", () => true);
    expect(env).toMatchObject({ JAVA_HOME: "/home/.sdkman/candidates/java/current", ANDROID_HOME: "/opt/homebrew/share/android-commandlinetools" });
    expect(env.PATH?.startsWith("/home/.sdkman/candidates/java/current/bin:")).toBe(true);
    expect(releaseEnvironment("/home", () => false)).not.toHaveProperty("JAVA_HOME");
  });
});
