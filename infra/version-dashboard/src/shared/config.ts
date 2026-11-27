import { homedir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

/** The package directory, infra/version-dashboard (this file is in its src/shared/). */
export const ROOT = resolve(dirname(fileURLToPath(import.meta.url)), "..", "..");
/** The flake lives here; `nix eval .` in it resolves to the enclosing checkout on its own. */
export const INFRA_DIR = resolve(ROOT, "..");
/**
 * WHERE `git log -- ios/` ACTUALLY RUNS: the repository root. A pathspec resolved against the wrong cwd fails silently by matching nothing.
 */
export const REPO_DIR = resolve(INFRA_DIR, "..");

/**
 * 8788, NOT 8787. bitcashier's version-dashboard already holds 8787 on this workstation, and two
 * processes racing for one port means whichever loses dies at boot -- silently, since launchd just
 * retries. Different estate, different port.
 */
export const port = (env: NodeJS.ProcessEnv): number => Number(env.KINOWO_DASHBOARD_PORT ?? 8788);
export const HOST = "127.0.0.1";
/** Set by the launchd plist: restart onto merged changes (see autodeploy.ts). Off for `npm start`,
 * where exiting on an edit would just stop the server. */
export const autodeploy = (env: NodeJS.ProcessEnv): boolean => env.KINOWO_AUTODEPLOY === "1";

/** The remote the mobile release lane and this page's tag reads use; scripts/tag-mobile-release.sh pushes its tags to
 * the same name. */
export const RELEASE_REMOTE = "origin";

/** Outside the repository on purpose: a state file inside it would be an uncommitted change,
 * which this page reports on its own header and which makes every `nix eval` copy a dirty tree. */
export const CACHE_DIR = join(homedir(), ".cache", "kinowo-nixos-dashboard");
