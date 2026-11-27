/**
 * The process's clock and environment, read in ONE place: the design rules table names this module as the adapter
 * for both, so every other module receives them (a `now` parameter, an `env` parameter) and a test passes its own.
 */

/** Milliseconds since the epoch, as the system clock says. */
export const systemClock = (): number => Date.now();

/** The environment the process started with. */
export const systemEnvironment = (): NodeJS.ProcessEnv => process.env;
