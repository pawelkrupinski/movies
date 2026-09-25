/**
 * Work that must not be cut off by a restart: a deploy dispatch, a check or switch running over
 * ssh on a real host.
 *
 * WHY THIS REPLACES THE PYTHON SELF-RELOAD. `os.execv` on every edit replaced the process image
 * under whatever was running: it once killed a check mid-ssh, orphaning a remote
 * `switch-to-configuration` whose output then went nowhere, and it could drop a deploy dispatch
 * so the client saw "Failed to fetch" over a deploy that had landed. Here a restart is only ever a
 * SIGTERM (from launchd, `npm run restart` or the watchdog), and SIGTERM drains: new work is
 * refused, running work finishes, then the process exits. launchd's ExitTimeOut is set above the
 * longest job so it does not SIGKILL a drain in progress.
 */
const running = new Map<number, { label: string; startedAt: number }>();
let nextId = 1;
let draining = false;
const idleWaiters: (() => void)[] = [];

export class ShuttingDown extends Error {
  constructor() {
    super("the dashboard is restarting -- try again in a few seconds");
  }
}

export function isDraining(): boolean {
  return draining;
}

/** Run `work` as protected work. Refused with ShuttingDown once a drain has begun. */
export async function protect<T>(label: string, work: () => Promise<T>): Promise<T> {
  if (draining) throw new ShuttingDown();
  const id = nextId++;
  running.set(id, { label, startedAt: Date.now() });
  try {
    return await work();
  } finally {
    running.delete(id);
    if (running.size === 0) idleWaiters.splice(0).forEach((wake) => wake());
  }
}

export function runningWork(): { label: string; startedAt: number }[] {
  return [...running.values()];
}

/** Stop accepting protected work and resolve once none is running. */
export function drain(): Promise<void> {
  draining = true;
  if (running.size === 0) return Promise.resolve();
  return new Promise((wake) => idleWaiters.push(wake));
}

/** Tests only. */
export function resetLifecycle(): void {
  draining = false;
  running.clear();
}
