/**
 * The process's clock and environment, read in ONE place: the design rules table names this module as the adapter
 * for both, so every other module receives them (a `now` parameter, an `env` parameter) and a test passes its own.
 */
import { existsSync } from "node:fs";

/** Milliseconds since the epoch, as the system clock says. */
export const systemClock = (): number => Date.now();

/** The environment the process started with. */
export const systemEnvironment = (): NodeJS.ProcessEnv => process.env;

/** Whether a file exists, as the filesystem says. */
export const systemFileExists = (path: string): boolean => existsSync(path);

/** The node binary this process runs on. */
export const systemExecutable = (): string => process.execPath;

/** Calls `check` every `intervalMs` without keeping the process alive; returns how to stop it. */
export const systemEvery = (intervalMs: number, check: () => void): (() => void) => {
  const timer = setInterval(check, intervalMs);
  timer.unref();
  return () => clearInterval(timer);
};
