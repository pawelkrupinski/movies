import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { setExecutor, type CommandOptions, type CommandResult, type Executor } from "../../src/exec.js";
import { FleetJobs, LOG_LINE_CAP, readScript, type JobLog } from "../../src/fleet/jobs.js";
import type { MachineRow } from "../../src/fleet/model.js";
import { fleetSshArgv, shellQuote, sshBinary } from "../../src/fleet/ssh.js";
import { drain, resetLifecycle, runningWork } from "../../src/lifecycle.js";
import { machine } from "./fixtures.js";

const MONGO_CLOSURE = `/nix/store/${"a".repeat(32)}-nixos-system-mongo-1-26.05`;
const MONITORING_CLOSURE = `/nix/store/${"b".repeat(32)}-nixos-system-monitoring-1-26.05`;

interface Call {
  argv: readonly string[];
  options: CommandOptions;
  finish: (result: Partial<CommandResult>, lines?: string[]) => void;
}

let restore: Executor | null = null;
let calls: Call[] = [];
const flush = () => new Promise((wake) => setTimeout(wake, 0));

/** Every command is held until the test finishes it, streaming the lines it names. */
beforeEach(() => {
  delete process.env.KINOWO_FLEET_SSH_USER;
  calls = [];
  restore = setExecutor((argv, options) => new Promise((resolve) => {
    calls.push({
      argv,
      options,
      finish: (result, lines = []) => {
        lines.forEach((line) => options.onLine?.(line));
        resolve({ code: 0, stdout: lines.join("\n"), stderr: "", timedOut: false, ...result });
      },
    });
  }));
});
afterEach(() => {
  if (restore) setExecutor(restore);
  resetLifecycle();
});

const ROWS = [
  machine({ name: "mongo-1", role: "mongo", public: "1.1.1.1", actionable: true }),
  machine({ name: "monitoring-1", role: "monitoring", public: "2.2.2.2", actionable: true }),
  machine({ name: "k3s-worker-1", role: "k3s-worker", public: "3.3.3.3", actionable: false }),
  machine({ name: "hidden-1", role: "k3s-worker", public: "", actionable: true }),
];

function jobsFor(rows: MachineRow[] = ROWS, switched: [string, string][] = []) {
  let tick = 0;
  return new FleetJobs({
    machineOf: (name) => rows.find((row) => row.name === name),
    onSwitched: (row, closure) => switched.push([row.name, closure]),
    now: () => tick++,
  });
}

const jobOf = (reply: ReturnType<FleetJobs["start"]>): string => {
  expect(reply.status).toBe(200);
  return (reply.payload as { job: string }).job;
};

/** Runs a check on `name` that offers `closure`, to completion. */
async function offered(jobs: FleetJobs, name: string, closure: string): Promise<void> {
  jobOf(jobs.start({ machine: name, phase: "check" }));
  await flush();
  calls.at(-1)?.finish({ code: 0 }, [`@@ CANSWITCH ${closure}`]);
  await flush();
}

describe("the ssh command", () => {
  it("reaches the host's PUBLIC address as root, feeding the script over stdin", () => {
    expect(fleetSshArgv("1.1.1.1", [MONGO_CLOSURE])).toEqual([
      sshBinary(), "-o", "BatchMode=yes", "-o", "ConnectTimeout=8", "-o", "StrictHostKeyChecking=accept-new",
      "-l", "root", "1.1.1.1", "bash", "-s", "--", MONGO_CLOSURE,
    ]);
  });
});

describe("a check", () => {
  it("streams its output, hides its markers and offers only a closure path", async () => {
    const jobs = jobsFor();
    const id = jobOf(jobs.start({ machine: "monitoring-1", phase: "check" }));
    await flush();
    const [call] = calls;
    expect(call?.argv).toEqual(fleetSshArgv("2.2.2.2"));
    expect(call?.options).toMatchObject({ input: readScript("check.sh"), timeoutMs: 300_000, mergeStderr: true });
    call?.finish({ code: 0 }, ["· connected to monitoring-1 as root", "@@ CANSWITCH /nix/store/not-a-closure", `@@ CANSWITCH ${MONITORING_CLOSURE}`]);
    await flush();
    const log = jobs.log(id, 0) as JobLog;
    expect(log.lines).toEqual([
      `$ ${call?.argv.map(shellQuote).join(" ")}`,
      "· connected to monitoring-1 as root",
      "!! refusing to offer /nix/store/not-a-closure: not a system closure path",
    ]);
    expect(log).toMatchObject({ done: true, exit: 0, can_switch: MONITORING_CLOSURE, result: null, machine: "monitoring-1", phase: "check" });
    expect((jobs.log(id, 2) as JobLog).lines).toHaveLength(1);
  });

  it("reads the pin the staged closure is at, and dry-activates rather than switching", () => {
    const script = readScript("check.sh");
    expect(script).toContain("pin=/var/lib/nixdeploy/staged-system");
    expect(script).toContain("dry-activate");
    expect(script).not.toMatch(/switch-to-configuration" switch/);
    expect(script).toContain("pid=${pid%%/*}");
  });

  it("caps a runaway log", async () => {
    const jobs = jobsFor();
    const id = jobOf(jobs.start({ machine: "monitoring-1", phase: "check" }));
    await flush();
    calls[0]?.finish({ code: 0 }, Array.from({ length: LOG_LINE_CAP + 50 }, (_, index) => `line ${index}`));
    await flush();
    const lines = (jobs.log(id, 0) as JobLog).lines;
    expect(lines).toHaveLength(LOG_LINE_CAP + 1);
    expect(lines.at(-1)).toBe(`!! output capped at ${LOG_LINE_CAP} lines`);
  });

  it("frees the machine even when ssh could not start", async () => {
    const jobs = jobsFor();
    const id = jobOf(jobs.start({ machine: "monitoring-1", phase: "check" }));
    await flush();
    calls[0]?.finish({ code: null, stderr: "spawn ssh ENOENT" });
    await flush();
    expect(jobs.log(id, 0)).toMatchObject({ done: true, exit: -1 });
    expect((jobs.log(id, 0) as JobLog).lines.at(-1)).toContain("could not start");
    expect(jobs.start({ machine: "monitoring-1", phase: "check" }).status).toBe(200);
  });
});

describe("what the endpoint refuses", () => {
  it("names a machine the page never showed", () => {
    expect(jobsFor().start({ machine: "nope", phase: "check" })).toEqual({ status: 400, payload: { error: "no machine called 'nope' on this page" } });
  });

  it("refuses a host with no public address, and one with nothing staged", () => {
    expect(jobsFor().start({ machine: "hidden-1", phase: "check" }).status).toBe(400);
    expect(jobsFor().start({ machine: "k3s-worker-1", phase: "check" })).toEqual({
      status: 409, payload: { error: "k3s-worker-1 has nothing staged that differs from what it is running" },
    });
  });

  it("refuses an unknown phase and a malformed closure", () => {
    expect(jobsFor().start({ machine: "monitoring-1", phase: "reboot" }).status).toBe(400);
    expect(jobsFor().start({ machine: "monitoring-1", phase: "switch", closure: "/etc/passwd" })).toEqual({
      status: 400, payload: { error: "that is not a system closure path" },
    });
  });

  it("refuses a well-formed closure that no check in this process read off that host", async () => {
    const jobs = jobsFor();
    await offered(jobs, "monitoring-1", MONITORING_CLOSURE);
    expect(jobs.start({ machine: "monitoring-1", phase: "switch", closure: MONGO_CLOSURE }).payload)
      .toEqual({ error: "run the check first: this closure was never read off that host" });
  });

  it("refuses new work once a restart is draining", async () => {
    await drain();
    expect(jobsFor().start({ machine: "monitoring-1", phase: "check" }).status).toBe(503);
  });
});

describe("one job per machine", () => {
  it("lets two machines be checked at the same time", () => {
    const jobs = jobsFor();
    const first = jobOf(jobs.start({ machine: "mongo-1", phase: "check" }));
    const second = jobOf(jobs.start({ machine: "monitoring-1", phase: "check" }));
    expect(first).not.toBe(second);
  });

  it("refuses the same machine twice at once", () => {
    const jobs = jobsFor();
    jobs.start({ machine: "mongo-1", phase: "check" });
    expect(jobs.start({ machine: "mongo-1", phase: "check" })).toEqual({ status: 409, payload: { error: "a check is already running against mongo-1" } });
  });

  it("frees the machine when the job finishes, and counts it as protected work until then", async () => {
    const jobs = jobsFor();
    jobs.start({ machine: "mongo-1", phase: "check" });
    await flush();
    expect(runningWork().map((work) => work.label)).toEqual(["check mongo-1"]);
    calls[0]?.finish({ code: 0 });
    await flush();
    expect(runningWork()).toEqual([]);
    expect(jobs.start({ machine: "mongo-1", phase: "check" }).status).toBe(200);
  });
});

describe("a switch", () => {
  it("still cannot switch the database without the typed name, from a bulk run or anywhere", async () => {
    // THE GUARD THE BULK RUN MUST NOT BE A WAY AROUND: it posts the same endpoint as the single
    // button, so the server-side check is what makes collecting the confirmation a courtesy.
    const jobs = jobsFor();
    await offered(jobs, "mongo-1", MONGO_CLOSURE);
    const refused = jobs.start({ machine: "mongo-1", phase: "switch", closure: MONGO_CLOSURE });
    expect(refused.status).toBe(400);
    expect((refused.payload as { error: string }).error).toContain("typed back as confirmation");
    expect(jobs.start({ machine: "mongo-1", phase: "switch", closure: MONGO_CLOSURE, confirm: "mongo-2" }).status).toBe(400);
    jobOf(jobs.start({ machine: "mongo-1", phase: "switch", closure: MONGO_CLOSURE, confirm: "mongo-1" }));
  });

  it("needs no typed name on an ordinary machine, runs the switch script and reports DONE", async () => {
    const switched: [string, string][] = [];
    const jobs = jobsFor(ROWS, switched);
    await offered(jobs, "monitoring-1", MONITORING_CLOSURE);
    const id = jobOf(jobs.start({ machine: "monitoring-1", phase: "switch", closure: MONITORING_CLOSURE }));
    await flush();
    const call = calls.at(-1);
    expect(call?.argv).toEqual(fleetSshArgv("2.2.2.2", [MONITORING_CLOSURE]));
    expect(call?.options).toMatchObject({ input: readScript("switch.sh"), timeoutMs: 1_800_000 });
    call?.finish({ code: 4 }, ["· switch-to-configuration exited 4", "@@ DONE"]);
    await flush();
    // THE VERDICT IS THE MARKER, NOT THE EXIT CODE: one unit failing to come back is not "the
    // closure was not activated".
    expect(jobs.log(id, 0)).toMatchObject({ done: true, exit: 4, result: "DONE" });
    expect(switched).toEqual([["monitoring-1", MONITORING_CLOSURE]]);
  });

  it("does not report a switch that FAILED as switched", async () => {
    const switched: [string, string][] = [];
    const jobs = jobsFor(ROWS, switched);
    await offered(jobs, "monitoring-1", MONITORING_CLOSURE);
    jobOf(jobs.start({ machine: "monitoring-1", phase: "switch", closure: MONITORING_CLOSURE }));
    await flush();
    calls.at(-1)?.finish({ code: 1 }, ["@@ FAILED"]);
    await flush();
    expect(switched).toEqual([]);
  });

  it("sets the profile BEFORE activating, and judges by /run/current-system", () => {
    const script = readScript("switch.sh");
    expect(script.indexOf("nix-env --profile /nix/var/nix/profiles/system --set")).toBeLessThan(script.indexOf('"$staged/bin/switch-to-configuration" switch'));
    expect(script).toContain('if [ "$now" = "$staged" ]; then echo "@@ DONE"; else echo "@@ FAILED"; fi');
  });
});
