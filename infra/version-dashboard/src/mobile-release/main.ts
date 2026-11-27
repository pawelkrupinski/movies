/**
 * `scripts/mobile-ship.sh` -- release main's app code to the App Store and Google Play in one go.
 * See release.ts for the order of the steps and plan.ts for how the version is chosen.
 */
import { tmpdir } from "node:os";
import { dirname } from "node:path";
import { parseArgs } from "node:util";
import { REPO_DIR } from "../shared/config.js";
import { describeFailure, runCommand } from "../shared/exec.js";
import { systemClock, systemEnvironment } from "../host/system.js";
import { ascApi, playClient, secretsFromEnvironmentFirst, vaultSecrets } from "../app-stores/clients.js";
import { notesFrom, readNotesDir } from "./notes.js";
import { PLATFORMS, type Platform } from "./plan.js";
import { release } from "./release.js";

const USAGE = `usage: scripts/mobile-ship.sh [--dry-run] [--version X.Y.Z] [--force] [--only ios|android] [--notes-dir DIR]

  --dry-run         inspect both stores and print the plan; change nothing
  --version X.Y.Z   ship under this version instead of the one picked automatically
  --force           ship even when ios/, android/ and mobile-version.txt are unchanged
  --only STORE      release to one store (ios or android) and leave the other exactly as it is
  --notes-dir DIR   "What's new" text: DIR/<locale>.txt or DIR/<language>.txt (pl.txt, en.txt, ...);
                    without it, empty App Store locales and Play get a generic "bug fixes" line`;

/** The command line, or the exit that answers a request for help or a bad `--only`. */
function readOptions() {
  const { values } = parseArgs({
    options: {
      "dry-run": { type: "boolean", default: false },
      version: { type: "string" },
      force: { type: "boolean", default: false },
      only: { type: "string" },
      "notes-dir": { type: "string" },
      help: { type: "boolean", short: "h", default: false },
    },
  });
  if (values.help) {
    console.log(USAGE);
    process.exit(0);
  }
  if (values.only !== undefined && !(PLATFORMS as readonly string[]).includes(values.only)) {
    console.error(`✗ --only takes ${PLATFORMS.join(" or ")}, not ${values.only}`);
    process.exit(2);
  }
  const only = values.only as Platform | undefined;
  return { values, only };
}

async function main(): Promise<void> {
  const { values, only } = readOptions();

  // The MAIN checkout, even when this runs from a worktree: android/local.properties is gitignored and
  // lives only there. (Every key -- App Store, Play, the upload keystore -- is in the Touch ID vault.)
  const gitCommonDir = ["git", "rev-parse", "--path-format=absolute", "--git-common-dir"];
  const commonDir = await runCommand(gitCommonDir, { timeoutMs: 10_000, cwd: REPO_DIR });
  if (commonDir.code !== 0) throw new Error(describeFailure(gitCommonDir, commonDir));
  const repoDir = dirname(commonDir.stdout.trim());

  // Run from the /mobile page, the keys arrive in the environment (see RELEASE_SECRET_NAMES).
  const secrets = secretsFromEnvironmentFirst(systemEnvironment(), vaultSecrets().get);
  const notesDir = values["notes-dir"];
  const stamp = () => new Date(systemClock()).toTimeString().slice(0, 8);
  try {
    await release(
      {
        asc: ascApi(secrets),
        play: playClient(secrets),
        secrets,
        run: runCommand,
        sleep: (ms) => new Promise((wake) => setTimeout(wake, ms)),
        log: (line) => console.log(`${stamp()} ${line}`),
        nowSeconds: () => Math.floor(systemClock() / 1000),
        repoDir,
        workDir: tmpdir(),
      },
      {
        dryRun: values["dry-run"],
        ...(values.version ? { requested: values.version } : {}),
        force: values.force,
        ...(only ? { only } : {}),
        notes: notesFrom(notesDir ? await readNotesDir(notesDir) : new Map()),
        overwriteNotes: notesDir !== undefined,
      },
    );
  } catch (error) {
    console.error(`✗ ${(error as Error).message}`);
    process.exit(1);
  }
}

await main();
