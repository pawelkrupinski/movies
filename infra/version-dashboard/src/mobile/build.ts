/**
 * One build of the mobile page: ask both stores, then diff each platform's OWN directory from ITS
 * OWN baseline -- the independence is the whole point (see stores.ts).
 */
import { fetchMobileTags, mobileTagSha, releaseCommitFor, unreleasedCommits } from "./git.js";
import { allPlatformsNetworkFailed, type MobileState, type Platform, type Submitted } from "./model.js";
import type { StoreState } from "./stores.js";

export interface MobileSources {
  readonly repoDir: string;
  readonly ios: () => Promise<StoreState>;
  readonly android: () => Promise<StoreState>;
  readonly now: () => number;
}

const PLATFORMS = [
  { name: "iOS", subdir: "ios", source: "ios" },
  { name: "Android", subdir: "android", source: "android" },
] as const;

/**
 * Which commit the pending version was built from -- found the same way as the live one's, so it
 * is the exact submitted build whenever the release tagged it -- and which unreleased commits that
 * build carries.
 */
async function submittedOf(repoDir: string, subdir: string, baseline: string | null, version: string | null): Promise<Submitted | null> {
  const commit = await releaseCommitFor(repoDir, version, subdir);
  if (!commit) return null;
  const carried = baseline ? await unreleasedCommits(repoDir, baseline, subdir, commit) : [];
  return { commit, includes: (carried ?? []).map((carriedCommit) => carriedCommit.sha) };
}

async function platformOf(repoDir: string, name: string, subdir: string, state: StoreState): Promise<Platform> {
  if (state.error !== null) return { name, fetchFailed: true, error: state.error, networkError: state.networkError };
  const version = state.liveVersion;
  // Looked up ONCE: it is both a candidate baseline and, when rejected, what the error names.
  const tagSha = await mobileTagSha(repoDir, subdir, version);
  const baseline = await releaseCommitFor(repoDir, version, subdir, tagSha);
  const commits = baseline ? await unreleasedCommits(repoDir, baseline, subdir) : null;
  const submitted = state.pending ? await submittedOf(repoDir, subdir, baseline, state.pending.version) : null;
  let error: string | null = null;
  if (version === null) error = "never released to this store yet";
  else if (!baseline && tagSha) {
    error = `mobile-${subdir}-${version} tags ${tagSha.slice(0, 10)}, which isn't reachable from HEAD — probably built from a branch never merged back`;
  } else if (!baseline) error = `no commit found matching 'Release mobile ${version}'`;
  return {
    name,
    fetchFailed: false,
    liveVersion: version,
    liveExtra: state.liveExtra,
    pending: state.pending,
    submitted,
    baseline,
    commits,
    error,
  };
}

export async function buildMobile(sources: MobileSources): Promise<MobileState> {
  const started = sources.now();
  // The tag fetch overlaps the two store round trips; it must land before any tag is resolved.
  const tags = fetchMobileTags(sources.repoDir);
  const states = await Promise.all(PLATFORMS.map(({ source }) => sources[source]()));
  await tags;
  const platforms = await Promise.all(
    PLATFORMS.map(({ name, subdir }, index) => platformOf(sources.repoDir, name, subdir, states[index] as StoreState)),
  );
  return { ready: true, builtAt: started, took: (sources.now() - started) / 1000, platforms, buildError: null };
}

/**
 * LONGER THAN THE FLEET PAGE'S CADENCE, because a build signs two fresh JWTs and round-trips to
 * Apple and Google, and the answer changes maybe once a day.
 */
export const REBUILD_MS = 600_000;
/**
 * How soon a build that failed for a NETWORK reason on every platform (or threw) is retried,
 * instead of sitting on the full cadence: a DNS blip that clears in under a minute should not need
 * somebody to notice a red box and press Refresh.
 */
export const RETRY_FLOOR_MS = 60_000;

/** When to build next, given the build just produced (null: it threw). */
export function nextBuildDelayMs(state: MobileState | null): number {
  return state === null || allPlatformsNetworkFailed(state) ? RETRY_FLOOR_MS : REBUILD_MS;
}
