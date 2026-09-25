/**
 * AUTODEPLOY: a merge into the checkout launchd runs from is the deploy, as it is for the events
 * app -- but NOT the way the events app does it. `tsx watch` restarts by killing the process under
 * whatever it is doing, which here would cut a NixOS switch off mid-ssh and orphan a remote
 * `switch-to-configuration`. So this only NOTICES the change; the restart is the same drain a
 * SIGTERM gets (new checks and switches refused, running ones finish), and launchd's KeepAlive
 * starts the new code.
 *
 * TYPE-CHECKED BEFORE IT IS TAKEN. A change that does not compile is logged and the running server
 * keeps serving the old code, rather than exiting into a crash loop that leaves the page dark until
 * somebody notices. A changed lockfile installs its dependencies first, since new code against old
 * node_modules is the same crash loop by another route.
 */
import { readFileSync, watch, type FSWatcher } from "node:fs";
import { join, relative } from "node:path";
import { describeFailure, runCommand } from "./exec.js";

/** What the running server is built from. Tests, docs and the plists do not change it. */
export const WATCHED = ["src", "web", "package.json", "package-lock.json", "tsconfig.json"] as const;
const SOURCE = /\.(ts|css|sh|json)$/;
/** A merge rewrites many files within a moment; one deploy per burst. */
export const SETTLE_MS = 2_000;
const INSTALL_TIMEOUT_MS = 300_000;
const CHECK_TIMEOUT_MS = 120_000;

/** Whether a changed path (relative to the dashboard's root) is part of the running server. */
export function isSource(path: string): boolean {
  const top = path.split("/")[0] ?? "";
  if (!(WATCHED as readonly string[]).includes(top)) return false;
  return top === path || SOURCE.test(path);
}

export interface AutodeployOptions {
  readonly root: string;
  /** Drain and exit; KeepAlive brings up the new code. */
  readonly restart: (reason: string) => void;
  readonly watchFn?: (dir: string, onChange: (path: string) => void) => { close(): void };
  readonly readLock?: () => string;
  readonly settleMs?: number;
  readonly log?: (line: string) => void;
}

const watchTree = (dir: string, onChange: (path: string) => void): FSWatcher =>
  watch(dir, { recursive: true }, (_event, file) => {
    if (file) onChange(relative(dir, join(dir, file.toString())));
  });

export class Autodeploy {
  private watcher: { close(): void } | null = null;
  private timer: ReturnType<typeof setTimeout> | null = null;
  private changed = new Set<string>();
  private deploying: Promise<void> | null = null;
  private lock: string;

  constructor(private readonly options: AutodeployOptions) {
    this.lock = this.readLock();
  }

  start(): void {
    this.watcher = (this.options.watchFn ?? watchTree)(this.options.root, (path) => this.noticed(path));
  }

  stop(): void {
    this.watcher?.close();
    if (this.timer) clearTimeout(this.timer);
  }

  noticed(path: string): void {
    if (!isSource(path)) return;
    this.changed.add(path);
    if (this.timer) clearTimeout(this.timer);
    this.timer = setTimeout(() => void this.deploy(), this.options.settleMs ?? SETTLE_MS);
  }

  /** One deploy at a time; changes that land meanwhile are picked up by the next. */
  deploy(): Promise<void> {
    this.deploying ??= this.attempt().finally(() => (this.deploying = null));
    return this.deploying;
  }

  private async attempt(): Promise<void> {
    const paths = [...this.changed].sort();
    this.changed.clear();
    if (!paths.length) return;
    const log = this.options.log ?? console.log;
    const what = `${paths.slice(0, 5).join(", ")}${paths.length > 5 ? ` and ${paths.length - 5} more` : ""}`;
    const lock = this.readLock();
    if (lock !== this.lock) {
      const install = await runCommand(["npm", "ci", "--no-audit", "--no-fund"], { cwd: this.options.root, timeoutMs: INSTALL_TIMEOUT_MS });
      if (install.code !== 0) {
        log(`autodeploy: NOT deploying ${what} -- npm ci failed (${describeFailure(["npm ci"], install)}); still serving the old code`);
        return;
      }
      this.lock = lock;
    }
    const argv = [join(this.options.root, "node_modules", ".bin", "tsc"), "-p", "tsconfig.json"];
    const check = await runCommand(argv, { cwd: this.options.root, timeoutMs: CHECK_TIMEOUT_MS });
    if (check.code !== 0) {
      log(`autodeploy: NOT deploying ${what} -- it does not type-check; still serving the old code:\n${(check.stdout || check.stderr).trim().split("\n").slice(0, 20).join("\n")}`);
      return;
    }
    log(`autodeploy: ${what} changed and type-checks; draining and restarting onto it`);
    this.options.restart(`autodeploy (${what})`);
  }

  private readLock(): string {
    try {
      return (this.options.readLock ?? (() => readFileSync(join(this.options.root, "package-lock.json"), "utf8")))();
    } catch {
      return "";
    }
  }
}
