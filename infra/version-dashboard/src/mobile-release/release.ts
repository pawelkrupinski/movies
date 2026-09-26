/**
 * One command from main to both stores: inspect → plan → (bump main) → build both in parallel from
 * ONE commit → submit iOS → promote Android.
 *
 * ORDER OF THE PUBLIC STEPS. Nothing reaches a user until both builds have uploaded: a failed
 * build leaves an unsubmitted App Store build and an internal-track Play build, both invisible.
 * Then iOS is submitted BEFORE Android is promoted, because a submission can be cancelled and a
 * production release cannot be taken back -- if Apple's readiness check refuses the version, Play
 * has not moved and a rerun finishes the job (plan.ts's FINISH) under the same version.
 *
 * ONE COMMIT. Both builds run in one detached worktree at the release commit, locally: the Android
 * CI dispatch builds whatever main's head is when it starts (not necessarily this commit), and its
 * run_number version codes are now far below Play's epoch-second ones.
 */
import { appendFile, copyFile, mkdir, readFile } from "node:fs/promises";
import { existsSync } from "node:fs";
import { join } from "node:path";
import { describeFailure, type Executor } from "../exec.js";
import type { AscApi, PlayApi, Sleep } from "../mobile/stores.js";
import { inspectAndroid, internalBuildFor, nextVersionCode, promoteAndroid, type AndroidState } from "./android.js";
import { deliveryUuid, describeIosPlan, existingBuild, inspectIos, planIosVersion, submitIos, type IosState } from "./ios.js";
import type { Notes } from "./notes.js";
import { APP_PATHS, planRelease, storeVersion, tagName, type Plan, type Platform } from "./plan.js";

export interface ReleaseDeps {
  readonly asc: AscApi;
  readonly play: PlayApi;
  readonly run: Executor;
  readonly sleep: Sleep;
  readonly log: (line: string) => void;
  readonly nowSeconds: () => number;
  /** The main checkout: where .env.local and the Android signing files live. */
  readonly repoDir: string;
  /** Where the release worktree and build logs go. */
  readonly workDir: string;
}

export interface ReleaseOptions {
  readonly dryRun: boolean;
  readonly requested?: string;
  readonly force?: boolean;
  readonly notes: Notes;
  readonly overwriteNotes: boolean;
}

const MINUTE = 60_000;

class Git {
  constructor(private readonly deps: ReleaseDeps) {}

  async run(args: readonly string[], cwd = this.deps.repoDir, env?: Record<string, string>): Promise<string> {
    const argv = ["git", ...args];
    const result = await this.deps.run(argv, { cwd, timeoutMs: 5 * MINUTE, ...(env ? { env } : {}) });
    if (result.code !== 0) throw new Error(describeFailure(argv, result));
    return result.stdout.trim();
  }

  async succeeds(args: readonly string[], cwd = this.deps.repoDir): Promise<boolean> {
    return (await this.deps.run(["git", ...args], { cwd, timeoutMs: 5 * MINUTE })).code === 0;
  }

  /** Every mobile-* tag on origin → the commit it points at (peeled). */
  async remoteTags(): Promise<Map<string, string>> {
    const tags = new Map<string, string>();
    for (const line of (await this.run(["ls-remote", "--tags", "origin", "refs/tags/mobile-*"])).split("\n")) {
      const [sha, ref] = line.split("\t");
      if (!sha || !ref) continue;
      const name = ref.replace(/^refs\/tags\//, "").replace(/\^\{\}$/, "");
      // An annotated tag lists the tag object then the peeled commit (`^{}`); the commit wins.
      if (ref.endsWith("^{}") || !tags.has(name)) tags.set(name, sha);
    }
    return tags;
  }
}

export interface Inspection {
  readonly mainSha: string;
  readonly onMain: string;
  readonly ios: IosState;
  readonly android: AndroidState;
  readonly tags: ReadonlyMap<string, string>;
  readonly plan: Plan;
}

export async function inspect(deps: ReleaseDeps, options: Pick<ReleaseOptions, "requested" | "force">): Promise<Inspection> {
  const git = new Git(deps);
  await git.run(["fetch", "--quiet", "origin", "main"]);
  const mainSha = await git.run(["rev-parse", "origin/main"]);
  const onMain = await git.run(["show", `${mainSha}:mobile-version.txt`]);
  const [ios, android, tags] = await Promise.all([inspectIos(deps.asc), inspectAndroid(deps.play), git.remoteTags()]);
  // A tag's commit may not be local yet, and the diff below needs it. --force: the lanes force-move
  // these tags on a re-upload, so a stale local copy must not win.
  await git.run(["fetch", "--quiet", "--force", "origin", "refs/tags/mobile-*:refs/tags/mobile-*"]);
  const tagShas = [...new Set(tags.values())];
  const unchanged = new Map<string, boolean>();
  for (const sha of tagShas) unchanged.set(sha, await git.succeeds(["diff", "--quiet", sha, mainSha, "--", ...APP_PATHS]));
  const plan = planRelease({ onMain, ios, android, tags, appUnchangedSince: (sha) => unchanged.get(sha) ?? false, ...options });
  return { mainSha, onMain, ios, android, tags, plan };
}

export function describeInspection(inspection: Inspection): string[] {
  const { ios, android, plan } = inspection;
  const lines = [
    `main        ${inspection.mainSha.slice(0, 9)}  mobile-version.txt ${inspection.onMain}`,
    `App Store   ${storeVersion(inspection, "ios") ?? "none"}${ios.pending ? `  (pending: ${ios.pending.versionString} ${ios.pending.state})` : ""}`,
    `Play        ${storeVersion(inspection, "android") ?? "none"}${android.unreleased.length ? `  (testing tracks: ${android.unreleased.join(", ")})` : ""}`,
  ];
  if (plan.kind === "nothing") return [...lines, `→ nothing to release: ${plan.reason}`];
  const { decision, platforms } = plan;
  lines.push(`→ ${decision.version}: ${decision.reason}${decision.bump ? ` (main will be bumped to ${decision.version})` : ""}`);
  if (platforms.includes("ios")) {
    let iosPlan: string;
    try {
      iosPlan = describeIosPlan(planIosVersion(ios, decision.version), decision.version);
    } catch (error) {
      iosPlan = `BLOCKED: ${(error as Error).message}`;
    }
    lines.push(`  iOS      archive + upload → ${iosPlan} → submit for review`);
  }
  if (platforms.includes("android")) lines.push(`  Android  build + upload to internal → promote to production`);
  return lines;
}

/** Signing for the release worktree's Gradle: keystore.properties's values, with the store path made absolute. */
async function androidSigningEnv(repoDir: string): Promise<Record<string, string>> {
  const androidDir = join(repoDir, "android");
  const properties = new Map(
    (await readFile(join(androidDir, "keystore.properties"), "utf8"))
      .split("\n")
      .map((line) => line.trim())
      .filter((line) => line && !line.startsWith("#") && line.includes("="))
      .map((line) => [line.slice(0, line.indexOf("=")).trim(), line.slice(line.indexOf("=") + 1).trim()] as const),
  );
  const need = (key: string) => {
    const value = properties.get(key);
    if (!value) throw new Error(`android/keystore.properties has no ${key}`);
    return value;
  };
  const storeFile = need("storeFile");
  return {
    KINOWO_RELEASE_STORE_FILE: storeFile.startsWith("/") ? storeFile : join(androidDir, storeFile),
    KINOWO_RELEASE_STORE_PASSWORD: need("storePassword"),
    KINOWO_RELEASE_KEY_ALIAS: need("keyAlias"),
    KINOWO_RELEASE_KEY_PASSWORD: need("keyPassword"),
    KINOWO_PLAY_CREDENTIALS_FILE: join(androidDir, "play-credentials.json"),
  };
}

export async function release(deps: ReleaseDeps, options: ReleaseOptions): Promise<void> {
  const { log } = deps;
  const inspection = await inspect(deps, options);
  describeInspection(inspection).forEach((line) => log(line));
  const { plan } = inspection;
  if (plan.kind === "nothing" || options.dryRun) return;
  const { version } = plan.decision;
  const wants = (platform: Platform) => plan.platforms.includes(platform);
  if (wants("ios")) planIosVersion(inspection.ios, version); // throws on an approved-but-unreleased record, before any build

  const git = new Git(deps);
  const worktree = join(deps.workDir, `kinowo-mobile-release-${version}`);
  const logs = join(deps.workDir, `kinowo-mobile-release-${version}-logs`);
  await mkdir(logs, { recursive: true });
  if (existsSync(worktree)) await git.run(["worktree", "remove", "--force", worktree]);
  await git.run(["worktree", "add", "--quiet", "--detach", worktree, inspection.mainSha]);

  let releaseSha = inspection.mainSha;
  if (plan.decision.bump) releaseSha = await bumpMain(deps, git, worktree, version);

  const tags = inspection.tags;
  const resumable = (platform: Platform) => !plan.decision.bump && tags.get(tagName(platform, version)) === releaseSha;
  let iosBuildId: string | undefined;
  let androidVersionCode: number | undefined;
  const lanes: Promise<void>[] = [];
  if (wants("ios")) {
    lanes.push((async () => {
      const resumed = resumable("ios") ? await existingBuild(deps.asc, version) : null;
      iosBuildId = resumed ?? (await buildIos(deps, worktree, logs));
      log(`[ios] ${resumed ? "resuming with already-uploaded" : "uploaded"} build ${iosBuildId}`);
    })());
  }
  if (wants("android")) {
    lanes.push((async () => {
      const resumed = resumable("android") ? internalBuildFor(inspection.android, version) : null;
      androidVersionCode = resumed ?? (await buildAndroid(deps, worktree, logs, version, releaseSha, nextVersionCode(inspection.android, deps.nowSeconds())));
      log(`[android] ${resumed ? "resuming with internal-track" : "uploaded"} version code ${androidVersionCode}`);
    })());
  }
  const outcomes = await Promise.allSettled(lanes);
  const failures = outcomes.flatMap((outcome) => (outcome.status === "rejected" ? [outcome.reason as Error] : []));
  if (failures.length) {
    throw new Error(`build failed, nothing submitted or promoted (worktree kept at ${worktree}, logs in ${logs}):\n  ${failures.map((error) => error.message).join("\n  ")}`);
  }

  const pace = { sleep: deps.sleep, log: (line: string) => log(`[ios] ${line}`) };
  if (iosBuildId !== undefined) {
    await submitIos(deps.asc, { version, buildId: iosBuildId, notes: options.notes, overwriteNotes: options.overwriteNotes }, pace);
  }
  if (androidVersionCode !== undefined) {
    await promoteAndroid(deps.play, { version, versionCode: androidVersionCode, notes: options.notes }, (line) => log(`[android] ${line}`));
  }
  await git.run(["worktree", "remove", "--force", worktree]);
  log(`released ${version} from ${releaseSha.slice(0, 9)} (${plan.platforms.join(" + ")})`);
}

/**
 * Commit the bump on top of main and push it straight to main. Retries a race with another push by
 * rebasing: the bump touches only mobile-version.txt and the pbxproj, so it replays cleanly.
 * The pre-push hook's iOS compile check is skipped -- the archive that follows compiles the app
 * target for real, from this exact commit.
 */
async function bumpMain(deps: ReleaseDeps, git: Git, worktree: string, version: string): Promise<string> {
  const bump = await deps.run([join(worktree, "scripts", "mobile-release.sh"), version, "--here"], { cwd: worktree, timeoutMs: 2 * MINUTE });
  if (bump.code !== 0) throw new Error(describeFailure(["mobile-release.sh"], bump));
  for (let attempt = 1; ; attempt++) {
    const sha = await git.run(["rev-parse", "HEAD"], worktree);
    const push = await deps.run(["git", "push", "--quiet", "origin", "HEAD:refs/heads/main"], {
      cwd: worktree,
      timeoutMs: 10 * MINUTE,
      env: { PREPUSH_SKIP: "ios" },
      mergeStderr: true,
    });
    if (push.code === 0) {
      const remote = (await git.run(["ls-remote", "origin", "refs/heads/main"])).split("\t")[0];
      if (remote !== sha) throw new Error(`pushed ${sha} but origin/main is ${remote}`);
      deps.log(`main bumped to ${version} at ${sha.slice(0, 9)} (pre-push iOS compile check skipped: the archive compiles it)`);
      return sha;
    }
    if (attempt >= 3) throw new Error(describeFailure(["git", "push"], push));
    deps.log(`push raced another commit; rebasing (attempt ${attempt})`);
    await git.run(["fetch", "--quiet", "origin", "main"], worktree);
    await git.run(["rebase", "--quiet", "origin/main"], worktree);
  }
}

async function logged(deps: ReleaseDeps, argv: readonly string[], cwd: string, logFile: string, prefix: string, env?: Record<string, string>): Promise<void> {
  const writes: Promise<void>[] = [];
  const result = await deps.run(argv, {
    cwd,
    timeoutMs: 90 * MINUTE,
    mergeStderr: true,
    ...(env ? { env } : {}),
    onLine: (line) => {
      writes.push(appendFile(logFile, `${line}\n`));
      // The iOS script prints its own ▸/✓ milestones; Gradle's task lines are too many to echo.
      if (/^\S*[▸✓✗!]/.test(line) || /BUILD (SUCCESSFUL|FAILED)|^> Task .*publish/i.test(line)) deps.log(`${prefix} ${line}`);
    },
  });
  await Promise.all(writes);
  if (result.code !== 0) throw new Error(`${prefix} ${describeFailure(argv, result)} -- full log: ${logFile}`);
}

/** ios-release.sh: test, archive, upload, tag. Returns the build id from altool's Delivery UUID. */
async function buildIos(deps: ReleaseDeps, worktree: string, logs: string): Promise<string> {
  await logged(deps, [join(worktree, "scripts", "ios-release.sh")], worktree, join(logs, "ios.log"), "[ios]");
  const uploadLog = join(worktree, "ios", "build", "release", "upload.log");
  const id = deliveryUuid(await readFile(uploadLog, "utf8"));
  if (!id) throw new Error(`[ios] no Delivery UUID in ${uploadLog}`);
  return id;
}

/** Unit tests, signed AAB, upload to `internal`, then tag the commit it was built from. */
async function buildAndroid(deps: ReleaseDeps, worktree: string, logs: string, version: string, sha: string, versionCode: number): Promise<number> {
  const androidDir = join(worktree, "android");
  const localProperties = join(deps.repoDir, "android", "local.properties");
  if (existsSync(localProperties)) await copyFile(localProperties, join(androidDir, "local.properties"));
  const env = { ...(await androidSigningEnv(deps.repoDir)), KINOWO_VERSION_CODE: String(versionCode) };
  await logged(deps, ["./gradlew", "--no-daemon", ":app:testDebugUnitTest", ":app:publishReleaseBundle"], androidDir, join(logs, "android.log"), "[android]", env);
  const tag = await deps.run([join(worktree, "scripts", "tag-mobile-release.sh"), "android", version, sha], { cwd: worktree, timeoutMs: 2 * MINUTE });
  if (tag.code !== 0) deps.log(`[android] ${describeFailure(["tag-mobile-release.sh"], tag)}`);
  return versionCode;
}
