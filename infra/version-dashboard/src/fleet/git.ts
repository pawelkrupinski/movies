/**
 * How far behind main a host is, measured in the local checkout. NEVER A FETCH: loading a page must
 * not mutate a checkout somebody may be mid-rebase in, so the cached `origin/main` is what is read.
 */
import { runCommand } from "../exec.js";

const GIT_TIMEOUT_MS = 30_000;

async function git(cwd: string, ...args: string[]): Promise<string | null> {
  const result = await runCommand(["git", ...args], { cwd, timeoutMs: GIT_TIMEOUT_MS });
  return result.code === 0 ? result.stdout.trim() : null;
}

export interface Checkout {
  readonly head: string;
  readonly origin: string;
  readonly dirty: boolean;
}

export async function inspectCheckout(cwd: string): Promise<Checkout> {
  const [head, status, origin] = await Promise.all([
    git(cwd, "rev-parse", "HEAD"),
    git(cwd, "status", "--porcelain", "-uno"),
    git(cwd, "rev-parse", "origin/main"),
  ]);
  return { head: head ?? "", dirty: !!status, origin: origin ?? "" };
}

/**
 * How many commits `b` is ahead of `a`. Used only to phrase "N behind main", so a failure is
 * cosmetic and answers null rather than an error.
 *
 * REMEMBERED, because the answer for two commits never changes and the page asks every read. Only
 * answers are remembered; a failure is asked again.
 */
export class Distances {
  private readonly known = new Map<string, number>();

  constructor(private readonly cwd: string) {}

  async measure(a: string, b: string): Promise<void> {
    if (!a || !b || a === b || this.known.has(`${a}..${b}`)) return;
    const out = await git(this.cwd, "rev-list", "--count", `${a}..${b}`);
    const count = out === null ? Number.NaN : Number.parseInt(out, 10);
    if (Number.isInteger(count)) this.known.set(`${a}..${b}`, count);
  }

  /** Synchronous, over what `measure` has already learned. */
  between(a: string, b: string): number | null {
    if (!a || !b) return a === b ? 0 : null;
    if (a === b) return 0;
    return this.known.get(`${a}..${b}`) ?? null;
  }
}
