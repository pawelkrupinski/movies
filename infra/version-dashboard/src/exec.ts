import { spawn } from "node:child_process";

export interface CommandResult {
  /** Exit code; null when the process was killed (timeout) or could not start. */
  readonly code: number | null;
  readonly stdout: string;
  readonly stderr: string;
  readonly timedOut: boolean;
}

export interface CommandOptions {
  readonly timeoutMs: number;
  readonly cwd?: string;
  /** Written to stdin, then stdin is closed. Without it stdin is closed immediately -- an ssh that
   * inherits a live stdin swallows whatever follows it (a hand-deploy rule from the Python era). */
  readonly input?: string;
  /** Called per complete line of stdout+stderr as it arrives (a live job console). */
  readonly onLine?: (line: string) => void;
  /** Merge stderr into stdout, in arrival order (the fleet consoles want one stream). */
  readonly mergeStderr?: boolean;
  /** Added to this process's own environment for the child (secrets a build reads, never argv). */
  readonly env?: Readonly<Record<string, string>>;
}

export type Executor = (argv: readonly string[], options: CommandOptions) => Promise<CommandResult>;

/**
 * How much of a STREAMED command's output its result keeps: the tail, behind a marker saying how much
 * went. A command with `onLine` has its every line delivered as it arrives, so the result needs only
 * enough for a failure's last lines -- kept whole, a 90-minute Gradle build or a runaway switch grew
 * the heap without bound beside a console that is capped (`LOG_LINE_CAP`).
 */
export const STREAMED_OUTPUT_CAP = 64 * 1024;

/** `buffer` with `chunk` added, cut to its last `cap` characters behind a truncation marker. */
export function keepTail(buffer: string, chunk: string, cap: number): string {
  const whole = buffer + chunk;
  if (whole.length <= cap) return whole;
  const marker = /^\[… (\d+) characters truncated …\]\n/.exec(whole);
  const dropped = (marker ? Number(marker[1]) : 0) + whole.length - (marker?.[0].length ?? 0) - cap;
  const body = (marker ? whole.slice(marker[0].length) : whole).slice(-cap);
  return `[… ${dropped} characters truncated …]\n${body}`;
}

/** The process groups of every command still running, so a shutdown can end them. */
const running = new Set<number>();

/**
 * Kill every command this process started that is still running, with everything it started. For a
 * process about to exit: each child is in a process group of its own (see below), so an exit leaves
 * it running -- an autodeploy restart mid-`nix eval` left the old eval beside the new process's.
 */
export function killRunningCommands(): number {
  let killed = 0;
  for (const group of running) {
    try {
      process.kill(-group, "SIGKILL");
      killed++;
    } catch {
      // already gone
    }
  }
  running.clear();
  return killed;
}

/**
 * Run a command. NEVER THROWS: a process that could not start, exited non-zero or timed out is a
 * result the caller has to look at, which is the point -- nothing here can be swallowed by accident.
 */
export const spawnExecutor: Executor = (argv, options) =>
  new Promise((resolvePromise) => {
    const [command, ...args] = argv;
    if (!command) {
      resolvePromise({ code: null, stdout: "", stderr: "empty command", timedOut: false });
      return;
    }
    const child = spawn(command, args, {
      cwd: options.cwd,
      env: options.env ? { ...process.env, ...options.env } : undefined,
      stdio: ["pipe", "pipe", "pipe"],
      // ITS OWN PROCESS GROUP, so the timeout can kill everything it started: killing only the
      // direct child left a grandchild (npm ci's scripts, ssh's ProxyCommand) holding the pipes,
      // `close` never fired, and the result -- a job's `done`, its machine's slot -- never came.
      detached: true,
    });
    const group = child.pid;
    if (group) running.add(group);
    let stdout = "";
    let stderr = "";
    const append = (buffer: string, chunk: string) => (options.onLine ? keepTail(buffer, chunk, STREAMED_OUTPUT_CAP) : buffer + chunk);
    // One partial-line buffer PER STREAM: shared, an unterminated stdout chunk was glued onto the
    // next stderr line, garbling exactly the interleaved output a switch console shows.
    const pending = { stdout: "", stderr: "" };
    let timedOut = false;
    const emit = (stream: keyof typeof pending, chunk: string) => {
      if (!options.onLine) return;
      const lines = (pending[stream] + chunk).split("\n");
      pending[stream] = lines.pop() ?? "";
      lines.forEach((line) => options.onLine?.(line));
    };
    child.stdout.setEncoding("utf8").on("data", (chunk: string) => {
      stdout = append(stdout, chunk);
      emit("stdout", chunk);
    });
    child.stderr.setEncoding("utf8").on("data", (chunk: string) => {
      if (options.mergeStderr) stdout = append(stdout, chunk);
      else stderr = append(stderr, chunk);
      emit("stderr", chunk);
    });
    const timer = setTimeout(() => {
      timedOut = true;
      try {
        if (child.pid) process.kill(-child.pid, "SIGKILL");
      } catch {
        child.kill("SIGKILL"); // the group is already gone; make sure the child is too
      }
    }, options.timeoutMs);
    child.on("error", (error) => {
      clearTimeout(timer);
      if (group) running.delete(group);
      resolvePromise({ code: null, stdout, stderr: stderr + error.message, timedOut });
    });
    child.on("close", (code) => {
      clearTimeout(timer);
      if (group) running.delete(group);
      for (const rest of [pending.stdout, pending.stderr]) if (rest) options.onLine?.(rest);
      resolvePromise({ code, stdout, stderr, timedOut });
    });
    child.stdin.on("error", () => {}); // a process that exits without reading stdin is not an error
    child.stdin.end(options.input ?? "");
  });

let executor: Executor = spawnExecutor;

export const runCommand: Executor = (argv, options) => executor(argv, options);

/** Tests only: swap the executor (see test/setup.ts, which installs one that refuses). */
export function setExecutor(next: Executor): Executor {
  const previous = executor;
  executor = next;
  return previous;
}

export function describeFailure(argv: readonly string[], result: CommandResult): string {
  if (result.timedOut) return `${argv[0]} timed out`;
  const message = (result.stderr || result.stdout).trim().split("\n").slice(-3).join(" ");
  return `${argv[0]} exited ${result.code ?? "abnormally"}${message ? `: ${message}` : ""}`;
}
