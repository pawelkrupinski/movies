/**
 * THE ACTION -- activating the closure CI already staged. The one thing this page does besides
 * read, and the narrow shape of it: it NEVER BUILDS AND NEVER EVALUATES ANYTHING FOR A HOST. CI has
 * already staged a signed closure at /var/lib/nixdeploy/staged-system and nixos-auto-apply already
 * refuses to activate it whenever doing so would disturb a unit -- on this fleet the reason is
 * almost always `units_would_change`. So the button's whole job is the step auto-apply declines to
 * take by itself: activate that exact closure, accepting the unit restarts. It cannot reach a
 * revision CI has not staged, so it is not a third path to production; it is the manual half of
 * the path that already exists.
 *
 * TWO PHASES, `check` THEN `switch`, for a reason that is not ceremony. The check does a
 * `switch-to-configuration dry-activate` and changes nothing; its output is the list of units
 * activating WOULD disturb, which on this fleet is the whole question. Pressing one button and
 * finding out afterwards would be handing the operator the outcome instead of the choice.
 *
 * The second thing the split buys is that THE BROWSER NEVER NAMES A STORE PATH IT INVENTED. The
 * check reads the pin off the host and emits it as a `@@ CANSWITCH <path>` marker; the switch
 * phase accepts only a path some check in this process's lifetime actually read that way. Without
 * that, /fleet-apply would activate any store path anybody could POST to it.
 */
import { readFileSync } from "node:fs";
import { runCommand } from "../exec.js";
import { isDraining, protect, ShuttingDown } from "../lifecycle.js";
import { CLOSURE_PATH_RE, needsConfirmation, type MachineRow } from "./model.js";
import { fleetSshArgv, shellQuote } from "./ssh.js";

/** A script from src/fleet/scripts, read where it is used (inside a job, never at import). */
export const readScript = (name: string): string => readFileSync(new URL(`./scripts/${name}`, import.meta.url), "utf8");

export const LOG_LINE_CAP = 4000; // a runaway switch must not grow this process's memory without bound
export const CHECK_TIMEOUT_MS = 300_000; // ssh + a dry-activate
export const SWITCH_TIMEOUT_MS = 1_800_000; // a real switch, including whatever the activation scripts do
const JOB_RETENTION = 40;

export type Phase = "check" | "switch";

interface Job {
  readonly id: string;
  readonly machine: string;
  readonly phase: Phase;
  readonly lines: string[];
  done: boolean;
  exit: number | null;
  canSwitch: string | null;
  result: string | null;
  readonly started: number;
  /** For a switch: the store path activated, so there is something exact to wait for. */
  readonly closure: string;
}

export interface JobLog {
  readonly lines: string[];
  readonly done: boolean;
  readonly exit: number | null;
  readonly can_switch: string | null;
  readonly result: string | null;
  readonly machine: string;
  readonly phase: Phase;
}

export interface Reply {
  readonly status: number;
  readonly payload: { job: string; phase: Phase } | { error: string };
}

export interface JobsOptions {
  /**
   * The machine as THIS PAGE last showed it. Deliberately not a fresh read: the button may only act
   * on something the page has already displayed, so a name arriving from anywhere else resolves to
   * nothing. It is also what makes `actionable` a single decision rather than one the table and
   * the endpoint each make for themselves.
   */
  readonly machineOf: (name: string) => MachineRow | undefined;
  /** Called once a switch reports DONE. */
  readonly onSwitched: (machine: MachineRow, closure: string) => void;
  readonly timeouts?: Partial<Record<Phase, number>>;
  readonly loadScript?: (name: string) => string;
  readonly now?: () => number;
}

/**
 * ONE JOB PER MACHINE. THE THING THAT MUST NOT HAPPEN IS TWO SWITCHES ON ONE HOST: they would race
 * `nix-env --set` on the same profile and leave it pointing at whichever finished last. That is a
 * per-machine hazard and a per-machine guard answers it exactly.
 *
 * It USED to be one job fleet-wide, argued from these hosts depending on each other -- monitoring-1
 * runs the k3s server k3s-worker-1 joins, mongo-1 is the database both the worker pod and the web
 * tier talk to. That argument was for a person clicking one console at a time; a fleet-wide slot
 * makes "bring all to latest" strictly serial. The dependency risk is handled where it belongs:
 * the bulk run STOPS TAKING NEW MACHINES the moment any check or switch fails.
 */
export class FleetJobs {
  private readonly jobs = new Map<string, Job>();
  private seq = 0;

  constructor(private readonly options: JobsOptions) {}

  /** POST /fleet-apply -- start one check or one switch. */
  start(body: unknown): Reply {
    const request = (body && typeof body === "object" ? body : {}) as Record<string, unknown>;
    const name = text(request.machine).trim();
    const machine = this.options.machineOf(name);
    if (!machine) return fail(400, `no machine called ${pyRepr(name)} on this page`);
    const address = machine.public;
    if (!address) return fail(400, `${name} declares no publicAddress, so nothing here can reach it`);
    // The same rule the table renders, enforced again at the endpoint. The table not drawing a
    // button is a UI fact; this is the one that holds when the POST arrives from a stale tab.
    if (!machine.actionable) return fail(409, `${name} has nothing staged that differs from what it is running`);

    const phase = request.phase;
    let closure = "";
    if (phase === "switch") {
      closure = text(request.closure).trim();
      if (!CLOSURE_PATH_RE.test(closure)) return fail(400, "that is not a system closure path");
      // THE BROWSER MAY NOT NAME A CLOSURE THIS PROCESS HAS NOT SEEN STAGED -- the load-bearing
      // half of the pair: CLOSURE_PATH_RE only says the string is well formed.
      const offered = [...this.jobs.values()].some((job) => job.machine === name && job.canSwitch === closure);
      if (!offered) return fail(400, "run the check first: this closure was never read off that host");
      // THE PRODUCTION-DATABASE GATE, checked on the SERVER and not only in the browser. A prompt
      // in JS is a courtesy to the person clicking; it stops nothing that posts directly. The
      // confirmation is the machine's own name, typed, because a yes/no prompt is answered
      // reflexively and a name is not.
      if (needsConfirmation(machine) && text(request.confirm).trim() !== name) {
        return fail(400, `${name} is the production database: this switch needs the machine's name typed back as confirmation`);
      }
    } else if (phase !== "check") {
      return fail(400, "phase must be 'check' or 'switch'");
    }
    const running = [...this.jobs.values()].find((job) => job.machine === name && !job.done);
    if (running) return fail(409, `a ${running.phase} is already running against ${name}`);
    // A restart drains running jobs; a new one started now would be cut off by it.
    if (isDraining()) return fail(503, new ShuttingDown().message);

    const id = `j${++this.seq}`;
    const job: Job = {
      id, machine: name, phase, lines: [], done: false, exit: null, canSwitch: null, result: null,
      started: (this.options.now ?? Date.now)(), closure,
    };
    this.prune();
    this.jobs.set(id, job);
    void protect(`${phase} ${name}`, async () => {
      // ONE try/finally FROM THE MOMENT THE JOB EXISTS. `done` is what releases this machine's
      // slot, so a job left un-marked the first time ssh dies oddly is a machine no button works
      // on again until the process is restarted -- with nothing on screen explaining why. Reading
      // the script is inside it too.
      try {
        const load = this.options.loadScript ?? readScript;
        const timeouts = this.options.timeouts ?? {};
        if (phase === "check") {
          await this.run(job, fleetSshArgv(address), load("check.sh"), timeouts.check ?? CHECK_TIMEOUT_MS);
        } else {
          await this.run(job, fleetSshArgv(address, [closure]), load("switch.sh"), timeouts.switch ?? SWITCH_TIMEOUT_MS);
        }
      } catch (error) {
        this.emit(job, `!! ${error instanceof Error ? error.message : String(error)}`);
        job.exit ??= -1;
      } finally {
        job.done = true;
      }
      if (job.phase === "switch" && job.result === "DONE") this.options.onSwitched(machine, job.closure);
    }).catch((error: unknown) => {
      this.emit(job, `!! ${error instanceof Error ? error.message : String(error)}`);
      job.exit ??= -1;
      job.done = true;
    });
    return { status: 200, payload: { job: id, phase } };
  }

  /**
   * GET /fleet-apply/log -- everything after line `from`, plus how the job ended. THE BROWSER SENDS
   * AN OFFSET so a switch printing thousands of lines is not re-serialised on every poll.
   */
  log(id: string, from: number): JobLog | { error: string } {
    const job = this.jobs.get(id);
    if (!job) return { error: "no such job" };
    return {
      lines: job.lines.slice(Math.max(0, from)),
      done: job.done,
      exit: job.exit,
      can_switch: job.canSwitch,
      result: job.result,
      machine: job.machine,
      phase: job.phase,
    };
  }

  running(): string[] {
    return [...this.jobs.values()].filter((job) => !job.done).map((job) => job.id);
  }

  /**
   * Finished jobs cannot be dropped the moment they end: `can_switch` is read back out of them by
   * the switch phase, and the browser is still polling the log of the one that just completed. So
   * they are kept, and a day of clicking is prevented from accumulating output for ever instead.
   */
  private prune(): void {
    if (this.jobs.size <= JOB_RETENTION) return;
    const oldest = [...this.jobs.values()].sort((a, b) => a.started - b.started).slice(0, 10);
    for (const job of oldest) if (job.done) this.jobs.delete(job.id);
  }

  private emit(job: Job, line: string): void {
    if (job.lines.length < LOG_LINE_CAP) job.lines.push(line);
    else if (job.lines.length === LOG_LINE_CAP) job.lines.push(`!! output capped at ${LOG_LINE_CAP} lines`);
  }

  /**
   * One ssh invocation, streamed into the job. STDERR IS MERGED ON PURPOSE: switch-to-
   * configuration's narration, systemd's complaints and ssh's own failure to connect are all on
   * stderr, and a console that dropped them would show a switch that said nothing.
   */
  private async run(job: Job, argv: string[], input: string, timeoutMs: number): Promise<void> {
    // The exact command, first line of every log: an operator who can paste it into a terminal can
    // find out for themselves what it did without reading this file.
    this.emit(job, `$ ${argv.map(shellQuote).join(" ")}`);
    const result = await runCommand(argv, {
      timeoutMs,
      input,
      mergeStderr: true,
      onLine: (line) => {
        // `@@` markers are the script talking to THIS PROCESS, not to the reader. They carry the
        // one thing the browser is not permitted to make up -- the store path a switch may
        // activate -- and the switch's own verdict, so they are consumed here rather than printed.
        if (line.startsWith("@@ ")) {
          const marker = line.slice(3).trim();
          if (marker.startsWith("CANSWITCH ")) {
            const candidate = marker.slice("CANSWITCH ".length).trim();
            if (CLOSURE_PATH_RE.test(candidate)) job.canSwitch = candidate;
            else this.emit(job, `!! refusing to offer ${candidate}: not a system closure path`);
          } else {
            job.result = marker;
          }
          return;
        }
        this.emit(job, line);
      },
    });
    if (result.timedOut) this.emit(job, `!! timed out after ${Math.round(timeoutMs / 1000)}s and was killed (the Mac may have slept mid-job)`);
    else if (result.code === null) this.emit(job, `!! could not start ${argv[0]}: ${result.stderr.trim()}`);
    job.exit = result.code ?? (result.timedOut ? -9 : -1);
  }
}

const text = (value: unknown): string => (typeof value === "string" ? value : "");

function fail(status: number, error: string): Reply {
  return { status, payload: { error } };
}

/** Python's repr of a str, which the error texts have always used. */
function pyRepr(value: string): string {
  return value.includes("'") && !value.includes('"') ? `"${value}"` : `'${value.replaceAll("\\", "\\\\").replaceAll("'", "\\'")}'`;
}
