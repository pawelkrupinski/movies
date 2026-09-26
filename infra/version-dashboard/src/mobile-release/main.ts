/**
 * `scripts/mobile-ship.sh` -- release main's app code to the App Store and Google Play in one go.
 * See release.ts for the order of the steps and plan.ts for how the version is chosen.
 */
import { execFileSync } from "node:child_process";
import { tmpdir } from "node:os";
import { dirname } from "node:path";
import { parseArgs } from "node:util";
import { REPO_DIR } from "../config.js";
import { runCommand } from "../exec.js";
import { ascApi, playClient } from "../mobile/stores.js";
import { notesFrom, readNotesDir } from "./notes.js";
import { release } from "./release.js";

const USAGE = `usage: scripts/mobile-ship.sh [--dry-run] [--version X.Y.Z] [--force] [--notes-dir DIR]

  --dry-run         inspect both stores and print the plan; change nothing
  --version X.Y.Z   ship under this version instead of the one picked automatically
  --force           ship even when ios/, android/ and mobile-version.txt are unchanged
  --notes-dir DIR   "What's new" text: DIR/<locale>.txt or DIR/<language>.txt (pl.txt, en.txt, ...);
                    without it, empty App Store locales and Play get a generic "bug fixes" line`;

const { values } = parseArgs({
  options: {
    "dry-run": { type: "boolean", default: false },
    version: { type: "string" },
    force: { type: "boolean", default: false },
    "notes-dir": { type: "string" },
    help: { type: "boolean", short: "h", default: false },
  },
});
if (values.help) {
  console.log(USAGE);
  process.exit(0);
}

// The MAIN checkout, even when this runs from a worktree: .env.local, the Play key and the Android
// keystore are gitignored and live only there.
const repoDir = dirname(execFileSync("git", ["rev-parse", "--path-format=absolute", "--git-common-dir"], { cwd: REPO_DIR, encoding: "utf8" }).trim());

const notesDir = values["notes-dir"];
const stamp = () => new Date().toTimeString().slice(0, 8);
try {
  await release(
    {
      asc: ascApi(repoDir),
      play: playClient(repoDir),
      run: runCommand,
      sleep: (ms) => new Promise((wake) => setTimeout(wake, ms)),
      log: (line) => console.log(`${stamp()} ${line}`),
      nowSeconds: () => Math.floor(Date.now() / 1000),
      repoDir,
      workDir: tmpdir(),
    },
    {
      dryRun: values["dry-run"],
      ...(values.version ? { requested: values.version } : {}),
      force: values.force,
      notes: notesFrom(notesDir ? await readNotesDir(notesDir) : new Map()),
      overwriteNotes: notesDir !== undefined,
    },
  );
} catch (error) {
  console.error(`✗ ${(error as Error).message}`);
  process.exit(1);
}
