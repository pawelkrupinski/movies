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
}

export type Executor = (argv: readonly string[], options: CommandOptions) => Promise<CommandResult>;

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
    const child = spawn(command, args, { cwd: options.cwd, stdio: ["pipe", "pipe", "pipe"] });
    let stdout = "";
    let stderr = "";
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
      stdout += chunk;
      emit("stdout", chunk);
    });
    child.stderr.setEncoding("utf8").on("data", (chunk: string) => {
      if (options.mergeStderr) stdout += chunk;
      else stderr += chunk;
      emit("stderr", chunk);
    });
    const timer = setTimeout(() => {
      timedOut = true;
      child.kill("SIGKILL");
    }, options.timeoutMs);
    child.on("error", (error) => {
      clearTimeout(timer);
      resolvePromise({ code: null, stdout, stderr: stderr + error.message, timedOut });
    });
    child.on("close", (code) => {
      clearTimeout(timer);
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
