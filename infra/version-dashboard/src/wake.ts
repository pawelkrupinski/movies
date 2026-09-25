/**
 * Notices that this Mac slept, so sources can re-poll and reconnect at once.
 *
 * WHY. Timers do not catch up after sleep and long-lived connections die without an error: a
 * Kubernetes watch or an SSE stream can sit half-open after wake, reporting nothing, which reads
 * as "nothing is changing" -- the silent staleness this dashboard exists to prevent. The events app
 * learned the same thing from node-cron (commit a1d5087) and fixed it the same way: a heartbeat
 * that treats a wall-clock gap much longer than its own interval as a wake.
 */
export const HEARTBEAT_MS = 5_000;
const WAKE_GAP_MS = HEARTBEAT_MS * 3;

export class WakeDetector {
  private readonly listeners: (() => void)[] = [];
  private last: number;
  private timer: NodeJS.Timeout | null = null;

  constructor(private readonly now: () => number = Date.now) {
    this.last = now();
  }

  onWake(listener: () => void): void {
    this.listeners.push(listener);
  }

  start(): void {
    this.timer = setInterval(() => this.tick(), HEARTBEAT_MS);
    this.timer.unref();
  }

  stop(): void {
    if (this.timer) clearInterval(this.timer);
  }

  /** One heartbeat; exposed so tests can drive it with a fake clock. */
  tick(): void {
    const now = this.now();
    const gap = now - this.last;
    this.last = now;
    if (gap > WAKE_GAP_MS) {
      console.log(`wake: ${Math.round(gap / 1000)}s gap in the heartbeat; reconnecting and re-polling`);
      this.listeners.forEach((listener) => listener());
    }
  }
}
