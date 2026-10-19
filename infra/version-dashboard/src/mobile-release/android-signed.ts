/**
 * `scripts/android-signed <command...>` -- run a command (from android/: `./gradlew runOnDevice`) with
 * the release signing env out of the Touch ID vault, the same `androidSigningEnv` the release lane
 * uses. The keystore and Play key live in a fresh temp dir for the length of the command only.
 */
import { spawn } from "node:child_process";
import { tmpdir } from "node:os";
import { vaultSecrets } from "../mobile/stores.js";
import { runSigned, type SignedRun } from "./signing.js";

/** Interactive: the child shares this terminal, and runs for as long as it takes (no timeout). */
const inheritTerminal: SignedRun = (argv, env) =>
  new Promise((resolve, reject) => {
    const child = spawn(argv[0] as string, argv.slice(1), { stdio: "inherit", env: { ...process.env, ...env } });
    child.on("error", reject);
    child.on("exit", (code, signal) => resolve(code ?? (signal ? 128 : 1)));
  });

// Ctrl-C reaches the child (same process group); this process waits for it to exit so the
// keys are removed, instead of dying first and leaving them behind.
process.on("SIGINT", () => {});
process.on("SIGTERM", () => {});

const argv = process.argv.slice(2);
try {
  process.exitCode = await runSigned(vaultSecrets().get, argv[0] === "--" ? argv.slice(1) : argv, inheritTerminal, tmpdir());
} catch (error) {
  console.error(`✗ ${(error as Error).message}`);
  process.exitCode = 1;
}
