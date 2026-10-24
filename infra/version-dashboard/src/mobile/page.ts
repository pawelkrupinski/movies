import type { FastifyInstance } from "fastify";
import { existsSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";
import { CACHE_DIR, REPO_DIR } from "../config.js";
import type { Page } from "../page.js";
import type { MobileSources } from "./build.js";
import { MobileLive } from "./live.js";
import type { MobileState } from "./model.js";
import { realDetachedProcesses, ReleaseRuns } from "./release-run.js";
import { androidReleaseState, ascApi, iosReleaseState, playClient, RELEASE_SECRET_NAMES, SECRETS_BIN, VAULT_NAMESPACE, vaultSecrets } from "./stores.js";
import { renderMobile } from "./view.js";

type WakeSource = { readonly onWake: (listener: () => void) => void };

/** The real sources: this checkout's git, App Store Connect and the Play Developer API. */
export function mobileSources(repoDir: string = REPO_DIR): MobileSources {
  const asc = ascApi();
  const play = playClient();
  return { repoDir, ios: () => iosReleaseState(asc.get), android: () => androidReleaseState(play), now: Date.now };
}

/**
 * What the release lane's builds need that launchd's PATH lacks: Gradle's JDK (sdkman's current
 * java) and the Android SDK, both where a login shell on this Mac finds them.
 */
export function releaseEnvironment(home = homedir(), exists: (path: string) => boolean = existsSync): Record<string, string> {
  const env: Record<string, string> = {};
  const path = [process.env.PATH ?? ""];
  const java = join(home, ".sdkman", "candidates", "java", "current");
  if (exists(java)) {
    env.JAVA_HOME = java;
    path.unshift(join(java, "bin"));
  }
  const android = "/opt/homebrew/share/android-commandlinetools";
  if (exists(android)) env.ANDROID_HOME = android;
  env.PATH = path.filter(Boolean).join(":");
  return env;
}

export function createMobilePage(
  wake: WakeSource,
  live = new MobileLive(mobileSources()),
  unlockVault: () => Promise<void> = vaultSecrets().unlock,
  releases: ReleaseRuns = new ReleaseRuns({
    script: join(REPO_DIR, "scripts", "mobile-ship.sh"),
    cwd: REPO_DIR,
    env: releaseEnvironment(),
    secrets: { bin: SECRETS_BIN, namespace: VAULT_NAMESPACE, names: RELEASE_SECRET_NAMES },
    stateDir: join(CACHE_DIR, "mobile-release"),
    processes: realDetachedProcesses,
    now: Date.now,
    publish: (run) => live.showRelease(run),
  }),
): Page<MobileState> & { live: MobileLive } {
  // A sleep outlives the 10-minute timer; the page re-reads the stores as soon as the Mac wakes.
  wake.onWake(() => void live.rebuild());
  return {
    name: "mobile",
    path: "/mobile",
    title: "Mobile releases",
    store: live.store,
    live,
    start: () => {
      live.start();
      releases.resume();
    },
    stop: () => {
      live.stop();
      releases.stop();
    },
    renderBody: (snapshot) => renderMobile(snapshot.state, Date.now()).__raw,
    routes(app: FastifyInstance) {
      // Starts a build and answers at once: the new state reaches every tab over the event stream,
      // so the wait is visible on the page rather than in a request somebody is staring at.
      app.post("/mobile/refresh", async () => {
        void live.rebuild();
        return { ok: true };
      });
      // Pops a Touch ID dialog on this Mac naming the dashboard's own process; the grant lasts until
      // the dashboard restarts. Answers once the person has touched the sensor (or refused).
      app.post("/mobile/unlock-vault", async () => {
        try {
          await unlockVault();
        } catch (error) {
          return { ok: false, error: error instanceof Error ? error.message : String(error) };
        }
        void live.rebuild();
        return { ok: true };
      });
      // The release button: `{"mode": "plan"}` is the dry run, `{"mode": "release", "notes": {"pl": …}}` ships under a
      // fresh plan. Answers once the run has started; its output reaches the page as snapshots, and
      // the run outlives this process (see release-run.ts).
      app.post<{ Body: { mode?: unknown; notes?: unknown } }>("/mobile/release", async (request, reply) => {
        const { mode, notes } = request.body ?? {};
        if (mode !== "plan" && mode !== "release") return reply.code(400).send({ error: "mode must be plan or release" });
        if (notes !== undefined && (typeof notes !== "object" || notes === null)) return reply.code(400).send({ error: "notes must be an object" });
        const answer = releases.start(mode, (notes ?? {}) as Record<string, unknown>);
        return reply.code(answer.status).send(answer.payload);
      });
    },
  };
}
