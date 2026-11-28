/**
 * A launchd job runs on whatever node binary it started with. A brew upgrade deletes that binary under the running
 * process, and `secrets unlock` (which pins an unlock to its caller's executable path) then fails with "could not
 * identify caller process". The process notices its own binary is gone and asks to be restarted; KeepAlive in the
 * plist starts it again on the current node.
 */

/** What the dashboard says on the page when the unlock failed for this reason. */
export const BINARY_REPLACED_MESSAGE = "the dashboard's node binary was replaced: it is restarting";

export interface BinaryWatch {
  readonly executable: string;
  readonly exists: (path: string) => boolean;
  /** Runs `check` every `intervalMs`; returns how to stop it. */
  readonly every: (intervalMs: number, check: () => void) => () => void;
  readonly onGone: () => void;
  readonly log: (line: string) => void;
}

export const BINARY_CHECK_MS = 60_000;

/** Checks on the timer; the first time the binary is missing, logs one line and requests the restart. */
export function watchOwnBinary(watch: BinaryWatch, intervalMs: number = BINARY_CHECK_MS): () => void {
  let requested = false;
  return watch.every(intervalMs, () => {
    if (requested || watch.exists(watch.executable)) return;
    requested = true;
    watch.log(`${watch.executable} no longer exists (replaced by an upgrade): restarting on the current node`);
    watch.onGone();
  });
}

/** The unlock's own error, or the explanation when it is the vault failing to identify a process whose binary is gone. */
export function explainUnlockFailure(message: string, binaryGone: boolean): string {
  return binaryGone && /could not identify caller process/i.test(message) ? BINARY_REPLACED_MESSAGE : message;
}
