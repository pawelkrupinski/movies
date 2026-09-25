import { execFileSync } from "node:child_process";
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from "vitest";
import { setExecutor, spawnExecutor, type Executor } from "../../src/exec.js";
import { buildMobile, type MobileSources } from "../../src/mobile/build.js";
import { fetchMobileTags, mobileTagSha, releaseCommitFor, unreleasedCommits } from "../../src/mobile/git.js";
import type { Platform } from "../../src/mobile/model.js";
import type { StoreState } from "../../src/mobile/stores.js";
import { TempRepo } from "./repo.js";

// These suites run REAL git, against throwaway repositories only.
let refuse: Executor;
beforeAll(() => {
  refuse = setExecutor(spawnExecutor);
});
afterAll(() => {
  setExecutor(refuse);
});

describe("release baseline", () => {
  let repo: TempRepo;
  beforeEach(() => {
    repo = new TempRepo();
  });
  afterEach(() => repo.cleanup());

  it("finds the exact release commit", async () => {
    repo.commit("README.md", "init");
    const target = repo.commit("ios/a.swift", "Release mobile 2.0.7");
    repo.commit("ios/b.swift", "unrelated");
    expect(await releaseCommitFor(repo.root, "2.0.7")).toBe(target);
  });

  it("does not match a longer version or a trailing suffix", async () => {
    repo.commit("README.md", "init");
    repo.commit("ios/a.swift", "Release mobile 2.0.17");
    repo.commit("ios/b.swift", "Release mobile 2.0.7 hotfix");
    repo.commit("ios/c.swift", "Release mobile 2x0x7");
    expect(await releaseCommitFor(repo.root, "2.0.7")).toBeNull();
  });

  it("looks up nothing for a missing version", async () => {
    expect(await releaseCommitFor(repo.root, null)).toBeNull();
  });

  it("trusts the platform tag over the bump commit (2.0.8: the real build was one commit later)", async () => {
    repo.commit("README.md", "init");
    repo.commit("ios/a.swift", "Release mobile 2.0.8");
    const realBuild = repo.commit("ios/b.swift", "Fix ambiguous LocalizedStringKey.init");
    repo.tag("mobile-ios-2.0.8", realBuild);
    expect(await releaseCommitFor(repo.root, "2.0.8", "ios")).toBe(realBuild);
  });

  it("falls back to the bump commit when there is no tag", async () => {
    repo.commit("README.md", "init");
    const target = repo.commit("ios/a.swift", "Release mobile 2.0.7");
    expect(await releaseCommitFor(repo.root, "2.0.7", "ios")).toBe(target);
  });

  it("falls back to the bump commit when the tag is off HEAD's history", async () => {
    repo.commit("README.md", "init");
    const bump = repo.commit("ios/a.swift", "Release mobile 2.0.9");
    const orphan = repo.orphanCommit("ios/b.swift", "built and tagged, never merged back");
    repo.tag("mobile-ios-2.0.9", orphan);
    expect(await releaseCommitFor(repo.root, "2.0.9", "ios")).toBe(bump);
  });

  it("does not let the other platform's tag satisfy the lookup", async () => {
    repo.commit("README.md", "init");
    const bump = repo.commit("ios/a.swift", "Release mobile 2.0.8");
    repo.tag("mobile-ios-2.0.8", repo.commit("ios/b.swift", "ios-only fix"));
    expect(await releaseCommitFor(repo.root, "2.0.8", "android")).toBe(bump);
  });

  it("scopes unreleased commits to the platform directory", async () => {
    const base = repo.commit("README.md", "Release mobile 1.0.0");
    repo.commit("web/x.scala", "web-only change");
    const ios = repo.commit("ios/a.swift", "ios change");
    expect((await unreleasedCommits(repo.root, base, "ios"))?.map((commit) => commit.sha)).toEqual([ios]);
    expect(await unreleasedCommits(repo.root, base, "android")).toEqual([]);
  });

  it("reports up to date as an empty list and unreadable history as null", async () => {
    const base = repo.commit("README.md", "Release mobile 1.0.0");
    expect(await unreleasedCommits(repo.root, base, "ios")).toEqual([]);
    expect(await unreleasedCommits(repo.root, "0".repeat(40), "ios")).toBeNull();
  });
});

describe("mobile tag fetch", () => {
  let origin: TempRepo;
  let cloneDir: string;
  let clone: string;
  let first: string;
  beforeEach(() => {
    origin = new TempRepo();
    first = origin.commit("android/a.kt", "Release mobile 2.0.9");
    origin.tag("mobile-android-2.0.9", first);
    cloneDir = mkdtempSync(join(tmpdir(), "mobile-clone-"));
    clone = join(cloneDir, "clone");
    execFileSync("git", ["clone", "-q", origin.root, clone]);
  });
  afterEach(() => {
    origin.cleanup();
    rmSync(cloneDir, { recursive: true, force: true });
  });

  it("moves a tag that was force-moved on origin", async () => {
    const second = origin.commit("android/b.kt", "fix the build");
    origin.tag("mobile-android-2.0.9", second);
    expect(await mobileTagSha(clone, "android", "2.0.9")).toBe(first); // the stale view
    expect(await fetchMobileTags(clone)).toBe(true);
    expect(await mobileTagSha(clone, "android", "2.0.9")).toBe(second);
  });

  it("is not fatal when origin cannot be reached", async () => {
    execFileSync("git", ["remote", "set-url", "origin", "/nonexistent"], { cwd: clone });
    expect(await fetchMobileTags(clone)).toBe(false);
    expect(await mobileTagSha(clone, "android", "2.0.9")).toBe(first);
  });
});

describe("build assembly", () => {
  let repo: TempRepo;
  beforeEach(() => {
    repo = new TempRepo();
  });
  afterEach(() => repo.cleanup());

  const live = (version: string | null, extra: string | null = null): StoreState => ({ error: null, liveVersion: version, liveExtra: extra, pending: null });
  const sources = (ios: StoreState, android: StoreState): MobileSources => {
    let tick = 0;
    return { repoDir: repo.root, ios: async () => ios, android: async () => android, now: () => 1_790_000_000_000 + 250 * tick++ };
  };
  const byName = async (s: MobileSources): Promise<Record<string, Platform>> =>
    Object.fromEntries((await buildMobile(s)).platforms.map((platform) => [platform.name, platform]));
  const errorOf = (platform: Platform | undefined) => platform?.error;
  const commitsOf = (platform: Platform | undefined) => (platform && !platform.fetchFailed ? platform.commits : undefined);

  it("diffs each platform from its OWN release, not the other's", async () => {
    repo.commit("README.md", "init");
    repo.commit("ios/a.swift", "Release mobile 2.0.6");
    repo.commit("android/a.kt", "Release mobile 2.0.6");
    repo.commit("ios/b.swift", "Release mobile 2.0.7"); // iOS-only bump, like the real one
    repo.commit("ios/c.swift", "ios-only follow-up");
    repo.commit("android/b.kt", "android-only follow-up");
    const state = await buildMobile(sources(live("2.0.7", "READY_FOR_SALE"), live("2.0.6", "309")));
    expect(state).toMatchObject({ ready: true, builtAt: 1_790_000_000_000, took: 0.25, buildError: null });
    const platforms = Object.fromEntries(state.platforms.map((platform) => [platform.name, platform]));
    expect(commitsOf(platforms.iOS)?.map((commit) => commit.subject)).toEqual(["ios-only follow-up"]);
    expect(commitsOf(platforms.Android)?.map((commit) => commit.subject)).toEqual(["android-only follow-up"]);
  });

  it("reports a fetch error as its own state, without touching git", async () => {
    const platforms = await byName(sources({ error: "HttpError: HTTP 401", networkError: false }, live(null)));
    expect(platforms.iOS).toEqual({ name: "iOS", fetchFailed: true, error: "HttpError: HTTP 401", networkError: false });
  });

  it("does not call a version never released a fetch error", async () => {
    const platforms = await byName(sources(live(null), live(null)));
    expect(platforms.iOS?.fetchFailed).toBe(false);
    expect(errorOf(platforms.iOS)).toBe("never released to this store yet");
  });

  it("makes a missing release commit a visible error, not a crash", async () => {
    repo.commit("README.md", "init");
    const platforms = await byName(sources(live("9.9.9", "READY_FOR_SALE"), live(null)));
    expect(errorOf(platforms.iOS)).toBe("no commit found matching 'Release mobile 9.9.9'");
    expect(commitsOf(platforms.iOS)).toBeNull();
  });

  it("names an unreachable tag in the error rather than a generic message", async () => {
    repo.commit("README.md", "init");
    const orphan = repo.orphanCommit("ios/a.swift", "built and tagged, never merged back");
    repo.tag("mobile-ios-9.9.9", orphan);
    const platforms = await byName(sources(live("9.9.9", "READY_FOR_SALE"), live(null)));
    expect(errorOf(platforms.iOS)).toContain(orphan.slice(0, 10));
    expect(errorOf(platforms.iOS)).toContain("reachable");
    expect(commitsOf(platforms.iOS)).toBeNull();
  });

  /** Counts `rev-parse` lookups of the iOS 9.9.9 tag while running real git. */
  function countTagLookups(): { count: () => number; restore: () => void } {
    let lookups = 0;
    const previous = setExecutor((argv, options) => {
      if (argv.includes("mobile-ios-9.9.9^{commit}")) lookups++;
      return spawnExecutor(argv, options);
    });
    return { count: () => lookups, restore: () => setExecutor(previous) };
  }

  it("looks an unreachable tag up once, though it is used twice", async () => {
    repo.commit("README.md", "init");
    repo.tag("mobile-ios-9.9.9", repo.orphanCommit("ios/a.swift", "never merged back"));
    const lookups = countTagLookups();
    try {
      await buildMobile(sources(live("9.9.9"), live(null)));
    } finally {
      lookups.restore();
    }
    expect(lookups.count()).toBe(1);
  });

  it("looks a tag that does not exist up once too (null is an answer, not 'not looked')", async () => {
    repo.commit("README.md", "Release mobile 1.0.0");
    const lookups = countTagLookups();
    let platforms: Record<string, Platform>;
    try {
      platforms = await byName(sources(live("9.9.9"), live(null)));
    } finally {
      lookups.restore();
    }
    expect(lookups.count()).toBe(1);
    expect(errorOf(platforms.iOS)).toBe("no commit found matching 'Release mobile 9.9.9'");
  });
});
