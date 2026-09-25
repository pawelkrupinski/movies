/**
 * The git half of the mobile page: which commit shipped each store's live version, and what has
 * landed in that platform's directory since.
 */
import { runCommand } from "../exec.js";
import type { Commit } from "./model.js";

const GIT_TIMEOUT_MS = 30_000;
/** Short: it runs on every build, and a slow or unreachable origin must cost seconds, never the
 * whole git timeout. */
const TAG_FETCH_TIMEOUT_MS = 15_000;

async function git(repoDir: string, args: readonly string[], timeoutMs = GIT_TIMEOUT_MS) {
  return runCommand(["git", ...args], { cwd: repoDir, timeoutMs });
}

/**
 * Force-fetches origin's `mobile-*` tags into this checkout; returns whether that worked.
 *
 * FORCED BECAUSE THE TAGS MOVE: tag-mobile-release.sh force-moves `mobile-<platform>-<version>`
 * whenever a version is re-uploaded, and `git fetch`/`git pull` auto-follow a NEW tag but refuse to
 * move an existing one -- so without `+` this checkout would keep anchoring that version on its
 * first, superseded build. Only `mobile-*`, so no other local tag is ever overwritten.
 *
 * Non-fatal: a failure leaves the tags as they were.
 */
export async function fetchMobileTags(repoDir: string): Promise<boolean> {
  const result = await git(repoDir, ["fetch", "--quiet", "--no-tags", "origin", "+refs/tags/mobile-*:refs/tags/mobile-*"], TAG_FETCH_TIMEOUT_MS);
  if (result.code !== 0) {
    const why = result.timedOut ? "timed out" : result.stderr.trim() || `exit ${result.code}`;
    console.error(`mobile dashboard: could not fetch mobile-* tags (${why}); using local tags`);
  }
  return result.code === 0;
}

/** The commit a `mobile-<platform>-<version>` tag points at, or null if there is no such tag. */
export async function mobileTagSha(repoDir: string, platform: string, version: string | null): Promise<string | null> {
  if (!platform || !version) return null;
  const result = await git(repoDir, ["rev-parse", "-q", "--verify", `mobile-${platform}-${version}^{commit}`]);
  const sha = result.stdout.trim();
  return result.code === 0 && sha ? sha : null;
}

/** Whether `sha` is HEAD or an ancestor of it -- the precondition `unreleasedCommits`'s
 * `<sha>..HEAD` range silently assumes rather than checks. */
export async function reachableFromHead(repoDir: string, sha: string): Promise<boolean> {
  return (await git(repoDir, ["merge-base", "--is-ancestor", sha, "HEAD"])).code === 0;
}

/** Escaped for git's default (basic) --grep regex, where only these are special. */
const escapeBasicRegex = (text: string): string => text.replace(/[.*[\]^$\\]/g, "\\$&");

/**
 * The commit that actually produced a store version's artifact.
 *
 * FIRST CHOICE: the per-platform `mobile-<subdir>-<version>` tag that ios-release.sh (on upload)
 * and the Android CI workflow (on publish) push at the exact commit they built. It is right even
 * when that build ran from a commit AFTER the "Release mobile" bump, which is normal whenever a fix
 * lands between cutting the version and getting a working upload (2.0.8: the real iOS archive was
 * one commit past the bump, after a compile bug broke the first attempt).
 *
 * THE TAG MUST BE REACHABLE FROM HEAD, the same ref `unreleasedCommits` diffs against: a release
 * cut from a worktree branch never merged back would otherwise hand `<sha>..HEAD` a baseline outside
 * that history, which git answers with a misleading commit list instead of an error.
 *
 * FALLBACK, for versions released before the tagging existed: the exact `Release mobile X.Y.Z`
 * commit scripts/mobile-release.sh leaves -- NOT mobile-version.txt (only the newest version either
 * store was ever asked to build) and NOT `--all` (a stray worktree branch never merged). Anchored at
 * both ends, so 2.0.17 or "2.0.7 hotfix" is never credited with shipping 2.0.7.
 *
 * `tagSha`: a caller that already looked the tag up passes it (null included -- "looked, found
 * none") so the lookup is not repeated; undefined means "not looked up yet".
 */
export async function releaseCommitFor(
  repoDir: string,
  version: string | null,
  subdir?: string,
  tagSha?: string | null,
): Promise<string | null> {
  if (!version) return null;
  if (subdir) {
    const sha = tagSha === undefined ? await mobileTagSha(repoDir, subdir, version) : tagSha;
    if (sha && (await reachableFromHead(repoDir, sha))) return sha;
  }
  const result = await git(repoDir, ["log", "-1", "--format=%H", `--grep=^Release mobile ${escapeBasicRegex(version)}$`]);
  const sha = result.stdout.trim();
  return result.code === 0 && sha ? sha : null;
}

/** Commits touching `subdir` since `baseline`, newest first. [] when up to date; null when git
 * could not answer -- which the page says, rather than rendering it as "up to date". */
export async function unreleasedCommits(repoDir: string, baseline: string, subdir: string): Promise<Commit[] | null> {
  const result = await git(repoDir, ["log", "--format=%H%x1f%h%x1f%ad%x1f%s", "--date=short", `${baseline}..HEAD`, "--", subdir]);
  if (result.code !== 0) return null;
  return result.stdout.split("\n").filter(Boolean).map((line) => {
    const [sha = "", short = "", date = "", ...subject] = line.split("\x1f");
    return { sha, short, date, subject: subject.join("\x1f") };
  });
}
