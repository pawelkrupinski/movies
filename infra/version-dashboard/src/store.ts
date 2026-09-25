import { createHash } from "node:crypto";

/** A stable content hash: equal data, equal fingerprint, whatever the key order. */
export function fingerprint(value: unknown): string {
  return createHash("sha1").update(stableJson(value)).digest("hex").slice(0, 16);
}

function stableJson(value: unknown): string {
  if (Array.isArray(value)) return `[${value.map(stableJson).join(",")}]`;
  if (value && typeof value === "object") {
    const entries = Object.entries(value as Record<string, unknown>)
      .filter(([, v]) => v !== undefined)
      .sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0));
    return `{${entries.map(([k, v]) => `${JSON.stringify(k)}:${stableJson(v)}`).join(",")}}`;
  }
  return JSON.stringify(value) ?? "null";
}

export interface Snapshot<T> {
  /** Bumps only when the content changes. */
  readonly version: number;
  /** Epoch ms at which the content last changed. */
  readonly changedAt: number;
  /** Epoch ms of the newest recompute, changed or not: how current the page is. */
  readonly checkedAt: number;
  readonly state: T;
}

/**
 * The single in-memory truth a page renders from. Sources update it as they learn things; every
 * open tab is sent each new snapshot over server-sent events, so a tab is never older than the
 * server's knowledge and there is no "stale tab" to detect by clock.
 */
export class Store<T> {
  private snapshot: Snapshot<T>;
  private print: string;
  private readonly listeners = new Set<(snapshot: Snapshot<T>) => void>();

  constructor(initial: T, private readonly now: () => number = Date.now) {
    const at = now();
    this.snapshot = { version: 1, changedAt: at, checkedAt: at, state: initial };
    this.print = fingerprint(initial);
  }

  get(): Snapshot<T> {
    return this.snapshot;
  }

  set(state: T): void {
    const at = this.now();
    const print = fingerprint(state);
    if (print === this.print) {
      this.snapshot = { ...this.snapshot, checkedAt: at };
      return;
    }
    this.print = print;
    this.snapshot = { version: this.snapshot.version + 1, changedAt: at, checkedAt: at, state };
    this.listeners.forEach((listener) => listener(this.snapshot));
  }

  subscribe(listener: (snapshot: Snapshot<T>) => void): () => void {
    this.listeners.add(listener);
    return () => this.listeners.delete(listener);
  }
}
