/**
 * The fleet page in the browser: re-render every pushed snapshot with the server's own view, and
 * drive the check → switch consoles.
 *
 * RE-RENDERING MUST NEVER DESTROY A CONSOLE. The console IS the record of what a switch did, so a
 * re-render carries every open (or busy, or switched) row's console and controls over into the new
 * markup, and the bulk run's console with them. That is what lets the state be pushed at all -- the
 * Python page could only reload the whole document, and so had to refuse to while a job ran.
 */
import type { FleetState } from "../src/fleet/model.js";
import { FLEET_COLUMNS, renderFleet } from "../src/fleet/view.js";
import { connect } from "./live.js";

export interface Deps {
  fetch: (url: string, init?: RequestInit) => Promise<Response>;
  confirm: (message: string) => boolean;
  prompt: (message: string) => string | null;
  alert: (message: string) => void;
  sleep: (ms: number) => Promise<void>;
  /** Unix seconds, for the ages in the re-rendered rows. */
  now: () => number;
}

const deps: Deps = {
  fetch: (url, init) => fetch(url, init),
  confirm: (message) => window.confirm(message),
  prompt: (message) => window.prompt(message),
  alert: (message) => window.alert(message),
  sleep: (ms) => new Promise((wake) => setTimeout(wake, ms)),
  now: () => Date.now() / 1000,
};

/** Tests only. */
export function setDeps(next: Partial<Deps>): void {
  Object.assign(deps, next);
}

/**
 * On a fleet this size it is `min(6, n)` -- in practice every machine at once. The number that
 * matters is the STOP rule, not the width: nothing new is started once anything fails.
 */
export const FLEET_BULK_THREADS = 6;
/** How long a switched row waits for the pushed state to show its new closure. */
export const LANDING_WAIT_MS = 240_000;

let latest: FleetState | null = null;
const landings = new Map<string, { want: string; done: (landed: boolean) => void }[]>();
let bulkRunning = false;

interface JobResult {
  lines?: string[];
  done?: boolean;
  exit?: number | null;
  can_switch?: string | null;
  result?: string | null;
  error?: string;
}

/** Anything with a writable textContent: a real console foot, or the bulk run's stand-in. */
interface Foot {
  textContent: string | null;
}

function app(): HTMLElement {
  const main = document.getElementById("app");
  if (!main) throw new Error("no #app to render into");
  return main;
}

const cellFor = (name: string): HTMLElement | undefined =>
  [...document.querySelectorAll<HTMLElement>(".actioncell")].find((cell) => cell.dataset.machine === name);

/**
 * THE TAG GOES ON THE TEXT, NOT IN A COLUMN: a bulk run has several machines writing into one
 * console out of order, and a `[T2] ` prefix is what makes that readable. Classified off the line's
 * own prefix -- the scripts are consistent about '· ', '!! ' and '--- ' so the server sends no
 * markup and the browser invents no meaning.
 */
export function writeLines(pre: HTMLElement, lines: readonly string[], prefix = ""): void {
  for (const line of lines) {
    const row = document.createElement("div");
    row.textContent = prefix + line;
    if (line.startsWith("!!")) row.className = "bad";
    else if (line.startsWith("· ")) row.className = "step";
    else if (line.startsWith("---")) row.className = "rule";
    else if (line.startsWith("$ ")) row.className = "cmd";
    pre.appendChild(row);
  }
  pre.scrollTop = pre.scrollHeight;
}

function consoleOf(cell: HTMLElement): HTMLDetailsElement {
  const box = cell.querySelector<HTMLDetailsElement>(".cons");
  if (!box) throw new Error("row has no console");
  box.classList.remove("hidden");
  box.open = true; // opened for you the first time; still collapsible after
  return box;
}

async function fleetPost(body: Record<string, string>): Promise<{ job?: string; error?: string }> {
  try {
    const response = await deps.fetch("/fleet-apply", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(body),
    });
    return (await response.json()) as { job?: string; error?: string };
  } catch {
    return { error: "could not reach the dashboard" };
  }
}

/** Polls one job's log until it ends. `from` is an offset so the server re-sends only what is new. */
export async function followJob(job: string, pre: HTMLElement, foot: Foot, prefix = ""): Promise<JobResult | null> {
  let from = 0;
  let misses = 0;
  for (;;) {
    let data: JobResult | null = null;
    try {
      const response = await deps.fetch(`/fleet-apply/log?job=${encodeURIComponent(job)}&from=${from}`);
      data = (await response.json()) as JobResult;
    } catch {
      data = null;
    }
    // A FEW MISSES ARE NOT A FAILURE -- a moment of this dashboard being unreachable (a restart
    // draining) says nothing about whether the switch on the host succeeded, and treating it as
    // failure would tell an operator to retry an activation that has already happened.
    if (data === null || data.error === "no such job") {
      if (++misses < 12) {
        if (misses === 3) foot.textContent = `${prefix}lost contact with the dashboard — retrying…`;
        await deps.sleep(1000);
        continue;
      }
      writeLines(pre, [
        "!! lost contact with the dashboard, so this job can no longer be read.",
        "!! It may still be running on the host — check there before retrying.",
      ], prefix);
      foot.textContent = "";
      return null;
    }
    misses = 0;
    if (data.error) {
      writeLines(pre, [`!! ${data.error}`], prefix);
      foot.textContent = "";
      return null;
    }
    if (data.lines?.length) {
      writeLines(pre, data.lines, prefix);
      from += data.lines.length;
    }
    if (data.done) {
      foot.textContent = `${prefix}finished — exit ${data.exit}`;
      return data;
    }
    await deps.sleep(600);
  }
}

/**
 * THE LIST IS EXACTLY THE "Bring to latest…" BUTTONS THE PAGE ALREADY RENDERED -- never a fresh
 * read of anything. `actionable` on the server decided each is worth offering, and that must not
 * be re-decided here by different logic. The dynamically created "Activate this closure now" lives
 * in a `.consfoot`, not an `.actionrow`, so it is never picked up as a second machine.
 */
export function actionableCells(): HTMLElement[] {
  return [...document.querySelectorAll<HTMLElement>(".actioncell")].filter(
    (cell) => cell.querySelector(".actionrow .applybtn") && (cell.dataset.address ?? ""),
  );
}

/** RE-COUNTED FROM THE LIVE DOM, TEXT ONLY -- never `.disabled`: a bulk run owns that for its whole
 * duration, and re-enabling it here mid-run would let a second run start on top of the first. */
export function updateBulkButtonCount(): void {
  const button = document.getElementById("fleetbulkbtn");
  if (button) button.textContent = `Bring all to latest… (${actionableCells().length})`;
}

/** Resolves true once a pushed state shows `name` on `closure`, false after the wait. */
export function waitForLanding(name: string, closure: string, timeoutMs = LANDING_WAIT_MS): Promise<boolean> {
  const want = (closure.split("/").pop() ?? "").slice(0, 12);
  if (!want) return Promise.resolve(false);
  if (landed(latest, name, want)) return Promise.resolve(true);
  return new Promise((resolve) => {
    let settled = false;
    const done = (value: boolean) => {
      if (settled) return;
      settled = true;
      resolve(value);
    };
    landings.set(name, [...(landings.get(name) ?? []), { want, done }]);
    void deps.sleep(timeoutMs).then(() => done(false));
  });
}

function landed(state: FleetState | null, name: string, want: string): boolean {
  const row = state?.rows.find((machine) => machine.name === name);
  return !!row?.closure && row.closure.startsWith(want);
}

/**
 * The button described a state that has passed. Left visible as a sentence rather than removed, so
 * the row does not silently reshape under a cursor.
 */
function markSwitched(name: string): void {
  const cell = cellFor(name);
  if (!cell) return;
  cell.dataset.switched = "1";
  const row = cell.querySelector(".actionrow");
  if (row) row.innerHTML = "<span class=hint>switched — reload the page to act on this machine again</span>";
  updateBulkButtonCount();
}

/**
 * THE TYPED CONFIRMATION FOR THE PRODUCTION DATABASE, asked in one place because it is asked from
 * two: a single switch, and the pre-flight of a bulk run. Typing the machine's name is a deliberate
 * act, and it is the same string the SERVER independently requires -- this prompt is the courtesy,
 * FleetJobs is the guard.
 */
export function typedConfirmation(name: string, closure: string): string | null {
  const typed = deps.prompt("This is the PRODUCTION DATABASE.\n\n"
    + `Activating ${closure || "the staged closure"} on ${name} can restart mongod. `
    + "The web tier holds change streams against it, and a dropped change stream does "
    + "not reconnect by itself, so the site can go on serving stale showtimes until the app is "
    + "restarted.\n\nType the machine name to confirm:");
  return (typed ?? "").trim() === name ? name : null;
}

export async function fleetCheck(button: HTMLButtonElement): Promise<void> {
  const cell = button.closest<HTMLElement>(".actioncell");
  if (!cell) return;
  const name = cell.dataset.machine ?? "";
  const box = consoleOf(cell);
  const pre = box.querySelector<HTMLElement>(".out")!;
  const foot = box.querySelector<HTMLElement>(".consfoot")!;
  pre.textContent = "";
  foot.textContent = "connecting…";
  // Disabled BEFORE the await, so a double-click cannot post twice. The server refuses the second
  // anyway (one job per machine); this keeps the page from showing an error nobody meant to cause.
  button.disabled = true;
  cell.dataset.busy = "1";
  const finish = () => {
    button.disabled = false;
    delete (cellFor(name) ?? cell).dataset.busy;
  };
  const started = await fleetPost({ machine: name, phase: "check" });
  if (started.error || !started.job) {
    writeLines(pre, [`!! ${started.error ?? "no job was started"}`]);
    foot.textContent = "";
    finish();
    return;
  }
  const result = await followJob(started.job, pre, foot);
  finish();
  if (!result) return;
  if (!result.can_switch) {
    foot.textContent = "nothing to activate on this host — see the output above";
    return;
  }
  // THE SWITCH BUTTON ONLY EXISTS ONCE A DRY RUN HAS SAID WHAT IT WOULD DISTURB: `units_would_change`
  // is the reason auto-apply declined, so which units is the decision being delegated.
  const closure = result.can_switch;
  foot.textContent = "";
  const go = document.createElement("button");
  go.className = "applybtn go needs-live";
  go.textContent = "Activate this closure now";
  go.onclick = () => void fleetSwitch(go, name, closure);
  const note = document.createElement("span");
  note.className = "hint";
  note.textContent = "nothing above has changed this host; this is the switch itself";
  foot.append(go, note);
}

export async function fleetSwitch(button: HTMLButtonElement, name: string, closure: string): Promise<void> {
  const cell = cellFor(name);
  if (!cell) return;
  let confirmation = "";
  if (cell.dataset.danger) {
    confirmation = typedConfirmation(name, closure) ?? "";
    if (!confirmation) return;
  } else if (!deps.confirm(`Activate the staged closure on ${name}?\n\n${closure}`
    + "\n\nThis runs switch-to-configuration switch over ssh as root. Every unit the dry run "
    + "listed above will be stopped, started, restarted or reloaded.")) {
    return;
  }
  button.disabled = true;
  cell.dataset.busy = "1";
  const box = consoleOf(cell);
  const pre = box.querySelector<HTMLElement>(".out")!;
  const foot = box.querySelector<HTMLElement>(".consfoot")!;
  writeLines(pre, ["", "--- activating ---"]);
  foot.textContent = "switching…";
  try {
    const started = await fleetPost({ machine: name, phase: "switch", closure, confirm: confirmation });
    if (started.error || !started.job) {
      writeLines(pre, [`!! ${started.error ?? "no job was started"}`]);
      foot.textContent = "";
      button.disabled = false;
      return;
    }
    const result = await followJob(started.job, pre, foot);
    // THE VERDICT IS THE MARKER, NOT THE EXIT CODE. switch-to-configuration exits non-zero when any
    // single unit fails to come back, which is worth reading but is not the same statement as "the
    // closure was not activated" -- the script decides by re-reading /run/current-system.
    if (result && result.result === "DONE") {
      // THE ROW IS WRONG THE MOMENT A SWITCH SUCCEEDS: Prometheus scrapes on its own cadence. The
      // server waits for the new closure and pushes it; say which of the two is showing.
      foot.textContent = "switched — waiting for the next scrape to confirm the row above…";
      const shown = await waitForLanding(name, closure);
      markSwitched(name);
      foot.textContent = shown
        ? "switched, and the row above now reflects the closure it is running."
        : "switched, but Prometheus has not reported the new closure within four minutes — the row above may be stale. Check the host itself before switching it again.";
    } else if (result) {
      foot.textContent = "the switch did not complete — read the output above before retrying";
      button.disabled = false;
    }
  } finally {
    delete (cellFor(name) ?? cell).dataset.busy;
  }
}

/**
 * ANSWERS "DID IT LAND" BY ASKING THE CONSUMER (Prometheus) RATHER THAN THE PROCESS THAT RAN IT. A
 * switch can finish on the host at the exact moment this dashboard is briefly unreachable, and
 * followJob giving up says nothing about whether switch-to-configuration succeeded.
 */
async function closureMatches(name: string, closure: string): Promise<boolean> {
  const want = (closure.split("/").pop() ?? "").slice(0, 12);
  if (!want) return false;
  try {
    const response = await deps.fetch(`/fleet-apply/machine?machine=${encodeURIComponent(name)}`);
    const data = (await response.json()) as { error?: string; store_hash?: string };
    return !data.error && !!data.store_hash && data.store_hash.startsWith(want);
  } catch {
    return false;
  }
}

/** A console foot for a worker with no row of its own: each assignment becomes one more line. */
function sharedFoot(pre: HTMLElement): Foot {
  return {
    set textContent(value: string | null) {
      if (value) writeLines(pre, [value]);
    },
    get textContent() {
      return "";
    },
  };
}

export async function bringAllToLatest(button: HTMLButtonElement): Promise<void> {
  const cells = actionableCells();
  if (!cells.length) {
    deps.alert("Nothing needs updating right now — no machine has a newer closure staged.");
    return;
  }
  const names = cells.map((cell) => cell.dataset.machine + (cell.dataset.env ? ` (${cell.dataset.env})` : ""));
  const threads = Math.min(FLEET_BULK_THREADS, cells.length);
  const danger = cells.filter((cell) => cell.dataset.danger);
  let message = `Bring ${cells.length} machine(s) to latest, ${threads} at a time:\n\n  ${names.join("\n  ")}`
    + "\n\nEach machine is checked, then switched if the check finds something staged. Once ANY "
    + "machine's check or switch fails to confirm success, no NEW machine is started — whatever "
    + "is already in flight on the other threads is left to finish, and whatever had already "
    + "switched stays switched.";
  if (danger.length) {
    message += `\n\n⚠ ${danger.length} of these is the PRODUCTION DATABASE, and will ask for its name to be typed before this run starts.`;
  }
  if (!deps.confirm(message)) return;

  // THE TYPED CONFIRMATIONS ARE COLLECTED BEFORE THE RUN STARTS, not when each machine's turn comes.
  // A prompt that appears twenty minutes into an unattended run is a prompt nobody is there to
  // answer. Declining excludes THAT machine rather than abandoning the run.
  const confirmations = new Map<string, string>();
  for (const cell of danger) {
    const name = cell.dataset.machine ?? "";
    const typed = typedConfirmation(name, "");
    if (typed) confirmations.set(name, typed);
  }
  const queue = cells
    .filter((cell) => !cell.dataset.danger || confirmations.has(cell.dataset.machine ?? ""))
    .map((cell) => ({ name: cell.dataset.machine ?? "", env: cell.dataset.env ?? "" }));
  const declined = danger.map((cell) => cell.dataset.machine ?? "").filter((name) => !confirmations.has(name));
  if (!queue.length) {
    deps.alert("Nothing left to do — every machine was declined.");
    return;
  }

  button.disabled = true;
  bulkRunning = true;
  const box = document.getElementById("fleetbulkcons") as HTMLDetailsElement;
  box.classList.remove("hidden");
  box.open = true;
  const pre = box.querySelector<HTMLElement>(".out")!;
  const foot = box.querySelector<HTMLElement>(".consfoot")!;
  pre.textContent = "";
  if (declined.length) writeLines(pre, [`· skipping (confirmation declined): ${declined.join(", ")}`]);

  // A SHARED QUEUE, DRAINED BY N WORKERS -- `.shift()` is synchronous and JS has no preemption, so
  // it needs no lock. Once any worker sets `stopped`, no worker takes a NEW machine, but nothing in
  // flight is aborted: an ssh job mid-activation finishes on its own.
  const total = queue.length;
  let switched = 0;
  let done = 0;
  let stopped = false;
  const stoppedAt: string[] = [];
  const reportProgress = () => {
    foot.textContent = `${stopped ? "stopping — " : ""}${done}/${total} done`
      + (switched ? `, ${switched} switched` : "")
      + (queue.length && !stopped ? `, ${queue.length} queued` : "");
  };
  reportProgress();

  const worker = async (label: string) => {
    const tag = `[${label}] `;
    const halt = (name: string, line: string) => {
      writeLines(pre, [line], tag);
      stopped = true;
      stoppedAt.push(name);
      done++;
      reportProgress();
    };
    for (;;) {
      if (stopped) return;
      const target = queue.shift();
      if (!target) return;
      const { name, env } = target;
      writeLines(pre, ["", `=== ${name}${env ? ` (${env})` : ""} ===`], tag);

      const startedCheck = await fleetPost({ machine: name, phase: "check" });
      if (startedCheck.error || !startedCheck.job) return halt(name, `!! ${startedCheck.error ?? "no job was started"}`);
      const check = await followJob(startedCheck.job, pre, sharedFoot(pre), tag);
      if (!check) return halt(name, `!! stopping: could not complete the check on ${name}`);
      if (check.exit !== 0) return halt(name, `!! stopping: check on ${name} exited ${check.exit}`);
      if (!check.can_switch) {
        writeLines(pre, [`· nothing to activate on ${name} — skipping`], tag);
        done++;
        reportProgress();
        continue;
      }
      const closure = check.can_switch;

      writeLines(pre, ["", `--- activating ${name} ---`], tag);
      const startedSwitch = await fleetPost({ machine: name, phase: "switch", closure, confirm: confirmations.get(name) ?? "" });
      if (startedSwitch.error || !startedSwitch.job) return halt(name, `!! ${startedSwitch.error ?? "no job was started"}`);
      const result = await followJob(startedSwitch.job, pre, sharedFoot(pre), tag);
      let ok = !!result && result.exit === 0 && result.result === "DONE";
      if (!ok && !result) {
        // followJob gave up because THIS DASHBOARD stopped answering, which says nothing about the
        // host. Ask Prometheus before halting every machine queued behind this one.
        writeLines(pre, [`!! lost contact with the dashboard mid-switch — checking whether ${name} landed on the target closure anyway`], tag);
        ok = await closureMatches(name, closure);
        writeLines(pre, [ok
          ? `· confirmed from Prometheus: ${name} IS running the target closure — continuing`
          : "!! Prometheus does not show it there (or could not be read)"], tag);
      }
      if (!ok) {
        return halt(name, result
          ? `!! stopping: switch on ${name} exited ${result.exit}${result.result ? ` (${result.result})` : ""}`
          : `!! stopping: could not confirm the switch on ${name} landed`);
      }
      // THE SWITCH HAS LANDED; only the row's cosmetic catch-up is left, and it can be a minute away
      // (the next scrape). Not awaited: the worker moves on, the row follows the pushed state.
      switched++;
      writeLines(pre, ["· landed on the target closure — confirming the row in the background, moving on"], tag);
      void waitForLanding(name, closure).then((shown) => {
        markSwitched(name);
        writeLines(pre, [shown
          ? "· switched, and the row above now reflects the closure it is running."
          : "!! switched, but Prometheus has not reported the new closure within four minutes — check the host before switching it again."], tag);
      });
      done++;
      reportProgress();
    }
  };

  try {
    await Promise.all(Array.from({ length: Math.min(threads, queue.length) }, (_, index) => worker(`T${index + 1}`)));
  } finally {
    bulkRunning = false;
    button.disabled = false;
  }
  foot.textContent = stoppedAt.length
    ? `stopped after ${stoppedAt.join(", ")} — ${switched} machine(s) switched`
      + (queue.length ? `, ${queue.length} never started` : "")
      + ". Read the console above, fix it, then reload and try again."
    : switched
      ? `done — ${switched} machine(s) switched.`
      : "done — nothing needed activating.";
}

/** Re-reads every source now. The new state arrives over the stream by itself. */
export async function refreshNow(button: HTMLButtonElement): Promise<void> {
  const note = () => document.getElementById("refreshnote");
  button.disabled = true;
  const setNote = (text: string) => {
    const element = note();
    if (element) element.textContent = text;
  };
  setNote("re-reading…");
  try {
    await deps.fetch("/nixos/refresh", { method: "POST" });
  } catch {
    setNote("could not reach the dashboard");
    button.disabled = false;
    return;
  }
  // The re-read is an ssh round trip; a snapshot that changed anything re-renders this toolbar.
  await deps.sleep(15_000);
  button.disabled = false;
  setNote("");
}

interface Carried {
  readonly cons: Element;
  readonly actionrow: Element | null;
  readonly busy: boolean;
  readonly switched: boolean;
  readonly address: string;
  readonly env: string;
  readonly danger: string;
}

/**
 * Re-render from a pushed state, carrying over every row console that is open, busy or switched
 * (with its controls, so a disabled button stays disabled) and the bulk block while it is in use.
 * A row whose action row disappeared -- a machine that is now current -- keeps its console in an
 * action row of its own rather than losing it.
 */
export function applySnapshot(state: FleetState): void {
  latest = state;
  const carried = new Map<string, Carried>();
  for (const cell of document.querySelectorAll<HTMLElement>(".actioncell")) {
    const cons = cell.querySelector(".cons");
    const busy = cell.dataset.busy === "1";
    const switched = cell.dataset.switched === "1";
    if (!cons || (cons.classList.contains("hidden") && !busy && !switched)) continue;
    carried.set(cell.dataset.machine ?? "", {
      cons,
      actionrow: busy || switched ? cell.querySelector(".actionrow") : null,
      busy,
      switched,
      address: cell.dataset.address ?? "",
      env: cell.dataset.env ?? "",
      danger: cell.dataset.danger ?? "",
    });
  }
  const oldBulk = document.querySelector(".fleetbulk");
  const bulkConsole = document.getElementById("fleetbulkcons");
  const keepBulk = oldBulk && (bulkRunning || (bulkConsole && !bulkConsole.classList.contains("hidden"))) ? oldBulk : null;
  const refreshNote = document.getElementById("refreshnote")?.textContent ?? "";
  const refreshBusy = !!document.querySelector<HTMLButtonElement>("#refreshbtn")?.disabled;

  const root = app();
  root.innerHTML = renderFleet(state, deps.now()).__raw;

  for (const [name, kept] of carried) {
    let cell = cellFor(name);
    if (!cell) cell = synthesizeActionCell(root, name, kept);
    cell.querySelector(".cons")?.replaceWith(kept.cons);
    if (kept.actionrow) cell.querySelector(".actionrow")?.replaceWith(kept.actionrow);
    if (kept.busy) cell.dataset.busy = "1";
    if (kept.switched) cell.dataset.switched = "1";
  }
  if (keepBulk) {
    const fresh = root.querySelector(".fleetbulk");
    if (fresh) fresh.replaceWith(keepBulk);
    else root.querySelector(".toolbar")?.after(keepBulk);
  }
  const note = document.getElementById("refreshnote");
  if (note) note.textContent = refreshNote;
  const refresh = document.querySelector<HTMLButtonElement>("#refreshbtn");
  if (refresh) refresh.disabled = refreshBusy;
  updateBulkButtonCount();

  for (const [name, waiting] of landings) {
    const remaining = waiting.filter(({ want, done }) => {
      if (!landed(state, name, want)) return true;
      done(true);
      return false;
    });
    if (remaining.length) landings.set(name, remaining);
    else landings.delete(name);
  }
}

function synthesizeActionCell(root: HTMLElement, name: string, kept: Carried): HTMLElement {
  const row = document.createElement("tr");
  row.className = "actions";
  const cell = document.createElement("td");
  cell.colSpan = FLEET_COLUMNS;
  cell.className = "actioncell";
  Object.assign(cell.dataset, { machine: name, address: kept.address, env: kept.env, danger: kept.danger });
  cell.innerHTML = "<div class=actionrow></div><details class='cons hidden'></details>";
  row.appendChild(cell);
  const machineRow = [...root.querySelectorAll<HTMLElement>("tr[data-machine]")].find((tr) => tr.dataset.machine === name);
  if (machineRow) {
    machineRow.after(row);
  } else {
    // Gone from the page entirely (dropped from the roster mid-job): the console still survives.
    const table = document.createElement("table");
    table.appendChild(row);
    (root.querySelector("#fleet") ?? root).appendChild(table);
  }
  return cell;
}

function onClick(event: MouseEvent): void {
  const button = (event.target as Element | null)?.closest<HTMLButtonElement>("button[data-action]");
  if (!button || button.disabled) return;
  if (button.dataset.action === "check") void fleetCheck(button);
  else if (button.dataset.action === "bulk") void bringAllToLatest(button);
  else if (button.dataset.action === "refresh") void refreshNow(button);
}

export function start(): void {
  const initial = document.getElementById("initial")?.textContent;
  if (initial) latest = (JSON.parse(initial) as { state: FleetState }).state;
  document.addEventListener("click", onClick);
  connect<FleetState>("fleet", (snapshot) => applySnapshot(snapshot.state));
}

/** Tests only: forget module state between cases. */
export function resetForTests(): void {
  latest = null;
  landings.clear();
  bulkRunning = false;
}

if (typeof document !== "undefined" && document.body?.dataset.page === "fleet" && document.getElementById("initial")) start();
