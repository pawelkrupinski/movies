import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import type { CommandOptions, CommandResult, Executor } from "../../src/exec.js";
import { notesFrom } from "../../src/mobile-release/notes.js";
import { release, type ReleaseDeps } from "../../src/mobile-release/release.js";
import { FakeAsc, FakePlay, fixture, fixtureJson, noSleep } from "./fakes.js";

const APP = "6792566321";
const MAIN_SHA = "1".repeat(40);
const BUMP_SHA = "2".repeat(40);
const TAG_SHA = "3".repeat(40);
const BUILD = "e352007c-db06-4c71-9d33-a9a5aab21183";
const NOW = 1_790_500_000;

let dir: string;
beforeEach(() => {
  dir = mkdtempSync(join(tmpdir(), "mobile-ship-"));
  mkdirSync(join(dir, "repo", "android"), { recursive: true });
  writeFileSync(join(dir, "repo", "android", "keystore.properties"), "storeFile=kinowo-release.jks\nstorePassword=sp\nkeyAlias=ka\nkeyPassword=kp\n");
});
afterEach(() => rmSync(dir, { recursive: true, force: true }));

const ok = (stdout = ""): CommandResult => ({ code: 0, stdout, stderr: "", timedOut: false });

interface Ran {
  readonly argv: readonly string[];
  readonly options: CommandOptions;
}

/** Answers the commands a release runs; `gradleExit` fails the Android build. */
function executor(events: string[], ran: Ran[], gradleExit = 0): Executor {
  return async (argv, options) => {
    ran.push({ argv, options });
    const line = argv.join(" ");
    events.push(`run ${line}`);
    if (line === "git rev-parse origin/main") return ok(MAIN_SHA);
    if (line === `git show ${MAIN_SHA}:mobile-version.txt`) return ok("2.0.9\n");
    if (line.startsWith("git ls-remote --tags")) return ok(`${TAG_SHA}\trefs/tags/mobile-ios-2.0.9\n${TAG_SHA}\trefs/tags/mobile-android-2.0.9\n`);
    if (line.startsWith("git diff --quiet")) return { ...ok(), code: 1 }; // the app changed since 2.0.9
    if (line.startsWith("git worktree add")) mkdirSync(argv[5] as string, { recursive: true });
    if (line === "git rev-parse HEAD") return ok(BUMP_SHA);
    if (line === "git ls-remote origin refs/heads/main") return ok(`${BUMP_SHA}\trefs/heads/main`);
    if (argv[0]?.endsWith("ios-release.sh")) {
      const logDir = join(options.cwd as string, "ios", "build", "release");
      mkdirSync(logDir, { recursive: true });
      writeFileSync(join(logDir, "upload.log"), fixture("altool-upload.txt"));
      options.onLine?.("\u001b[32m✓\u001b[0m uploaded");
    }
    if (argv[0] === "./gradlew") return { ...ok(), code: gradleExit };
    return ok();
  };
}

function stores(events: string[]) {
  const versions = fixtureJson("asc-app-store-versions.json");
  const asc = new FakeAsc({
    [`GET /v1/apps/${APP}/appStoreVersions?filter[platform]=IOS&limit=50`]: versions,
    [`GET /v1/builds/${BUILD}`]: fixtureJson("asc-build.json"),
    "POST /v1/appStoreVersions": { data: { id: "v10" } },
    "PATCH /v1/appStoreVersions/v10/relationships/build": null,
    "GET /v1/appStoreVersions/v10/appStoreVersionLocalizations": fixtureJson("asc-version-localizations.json"),
    [`GET /v1/reviewSubmissions?filter[app]=${APP}&filter[platform]=IOS&filter[state]=READY_FOR_REVIEW`]: { data: [] },
    "POST /v1/reviewSubmissions": { data: { id: "sub" } },
    "GET /v1/reviewSubmissions/sub/items?include=appStoreVersion": { data: [] },
    "POST /v1/reviewSubmissionItems": { data: { id: "item" } },
    "PATCH /v1/reviewSubmissions/sub": null,
    "GET /v1/appStoreVersions/v10": { data: { id: "v10", attributes: { appStoreState: "WAITING_FOR_REVIEW" } } },
  }, events);
  const play = new FakePlay({
    "POST /edits": (_: unknown, hit: number) => ({ id: `edit-${hit}` }),
    "GET /edits/edit-1/tracks": fixtureJson("play-tracks.json"),
    "DELETE /edits/edit-1": null,
    "GET /edits/edit-2/tracks/internal": { track: "internal", releases: [{ name: "2.0.10", status: "completed", versionCodes: [String(NOW)] }] },
    "GET /edits/edit-2/listings": fixtureJson("play-listings.json"),
    "PUT /edits/edit-2/tracks/production": null,
    "POST /edits/edit-2:commit": { id: "edit-2" },
    "GET /edits/edit-3/tracks/production": { track: "production", releases: [{ name: "2.0.10", status: "completed", versionCodes: [String(NOW)] }] },
    "DELETE /edits/edit-3": null,
  }, events);
  return { asc, play };
}

function deps(events: string[], ran: Ran[], gradleExit = 0): ReleaseDeps & { asc: FakeAsc; play: FakePlay; lines: string[] } {
  const lines: string[] = [];
  return {
    ...stores(events),
    run: executor(events, ran, gradleExit),
    sleep: noSleep,
    log: (line) => lines.push(line),
    lines,
    nowSeconds: () => NOW,
    repoDir: join(dir, "repo"),
    workDir: dir,
  };
}

const options = { dryRun: false, notes: notesFrom(new Map()), overwriteNotes: false };

describe("release", () => {
  it("--dry-run prints the plan and changes nothing", async () => {
    const events: string[] = [];
    const ran: Ran[] = [];
    const d = deps(events, ran);
    await release(d, { ...options, dryRun: true });
    expect(d.lines.join("\n")).toContain("→ 2.0.10: 2.0.9 is already released; bumping to 2.0.10 (main will be bumped to 2.0.10)");
    expect(d.lines.join("\n")).toContain("iOS      archive + upload → create App Store version 2.0.10 → submit for review");
    expect(ran.map((r) => r.argv.join(" ")).filter((line) => /worktree|push|gradlew|release\.sh/.test(line))).toEqual([]);
    expect(d.asc.writes()).toEqual([]);
    expect(d.play.writes().map((call) => call.method)).toEqual(["POST", "DELETE"]); // the read-only edit
  });

  it("bumps main, builds both from the bump commit, submits iOS before promoting Android", async () => {
    const events: string[] = [];
    const ran: Ran[] = [];
    const d = deps(events, ran);
    await release(d, options);

    const push = ran.find((r) => r.argv[1] === "push");
    expect(push?.argv).toEqual(["git", "push", "--quiet", "origin", "HEAD:refs/heads/main"]);
    expect(push?.options.env).toEqual({ PREPUSH_SKIP: "ios" });

    const gradle = ran.find((r) => r.argv[0] === "./gradlew");
    expect(gradle?.options.env).toMatchObject({
      KINOWO_VERSION_CODE: String(NOW),
      KINOWO_RELEASE_STORE_FILE: join(dir, "repo", "android", "kinowo-release.jks"),
      KINOWO_PLAY_CREDENTIALS_FILE: join(dir, "repo", "android", "play-credentials.json"),
    });
    expect(ran.find((r) => r.argv[0]?.endsWith("tag-mobile-release.sh"))?.argv.slice(1)).toEqual(["android", "2.0.10", BUMP_SHA]);

    const at = (needle: string) => events.findIndex((event) => event.includes(needle));
    expect(at("mobile-release.sh")).toBeLessThan(at("push"));
    expect(at("push")).toBeLessThan(at("ios-release.sh"));
    expect(at("push")).toBeLessThan(at("./gradlew"));
    expect(at("PATCH /v1/reviewSubmissions/sub")).toBeLessThan(at("PUT /edits/edit-2/tracks/production"));
    expect(events.at(-1)).toContain("git worktree remove --force");
    expect(d.lines.at(-1)).toBe(`released 2.0.10 from ${BUMP_SHA.slice(0, 9)} (ios + android)`);
  });

  it("submits and promotes nothing when either build fails", async () => {
    const events: string[] = [];
    const ran: Ran[] = [];
    const d = deps(events, ran, 1);
    await expect(release(d, options)).rejects.toThrow(/build failed, nothing submitted or promoted/);
    expect(d.asc.writes()).toEqual([]);
    expect(d.play.writes().some((call) => call.method === "PUT")).toBe(false);
    expect(events.some((event) => event.includes("worktree remove"))).toBe(false); // kept for the logs
  });
});
