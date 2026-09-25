import { existsSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

/** This directory: infra/version-dashboard. */
export const ROOT = resolve(dirname(fileURLToPath(import.meta.url)), "..");
/** The flake lives here; `nix eval .` in it resolves to the enclosing checkout on its own. */
export const INFRA_DIR = resolve(ROOT, "..");
/**
 * WHERE .env.local, android/play-credentials.json AND `git log -- ios/` ACTUALLY LIVE: the
 * repository root. A pathspec resolved against the wrong cwd fails silently by matching nothing.
 */
export const REPO_DIR = resolve(INFRA_DIR, "..");

/**
 * 8788, NOT 8787. bitcashier's version-dashboard already holds 8787 on this workstation, and two
 * processes racing for one port means whichever loses dies at boot -- silently, since launchd just
 * retries. Different estate, different port.
 */
export const PORT = Number(process.env.KINOWO_DASHBOARD_PORT ?? 8788);
export const HOST = "127.0.0.1";
/** Set by the launchd plist: restart onto merged changes (see autodeploy.ts). Off for `npm start`,
 * where exiting on an edit would just stop the server. */
export const AUTODEPLOY = process.env.KINOWO_AUTODEPLOY === "1";

/** Outside the repository on purpose: a state file inside it would be an uncommitted change,
 * which this page reports on its own header and which makes every `nix eval` copy a dirty tree. */
export const CACHE_DIR = join(homedir(), ".cache", "kinowo-nixos-dashboard");

/**
 * A binary by name, falling back to where it is installed on this Mac. launchd's PATH lacks the
 * Nix profile, and a missing `nix` renders every row as "not reporting": a page that looks like a
 * fleet-wide outage but is really a missing binary.
 */
export function binary(name: string, fallbacks: readonly string[]): string | null {
  for (const dir of (process.env.PATH ?? "").split(":")) {
    if (dir && existsSync(join(dir, name))) return join(dir, name);
  }
  return fallbacks.find((path) => existsSync(path)) ?? null;
}
