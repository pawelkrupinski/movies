/**
 * The fleet page's markup, as pure functions of the pushed state. The server renders the first
 * paint with these and the browser re-renders every snapshot with the SAME functions, so the two
 * can never disagree. Imports nothing from node:.
 */
import { ago } from "../age.js";
import { html, type Raw } from "../html.js";
import { needsConfirmation, type FleetState, type MachineRow } from "./model.js";
import { shortClosure } from "./read.js";

/**
 * The env pill's colour, keyed on the value `fleet.environment` actually carries. Anything
 * unrecognised gets the neutral grey rather than one of the three meaningful colours: inventing a
 * classification nobody declared is worse than declining to colour it.
 */
export const ENV_COLOR: Readonly<Record<string, string>> = {
  prod: "#e5484d", production: "#e5484d",
  sandbox: "#f5a623",
  dev: "#30a46c", development: "#30a46c",
  global: "#5b6472", infra: "#5b6472",
};
const NEUTRAL = "#5b6472";

/**
 * WHAT EACH BADGE MEANS, ON HOVER. These are not decoration: every one of them is a state an
 * operator has to tell apart from a neighbouring state that wants the OPPOSITE response -- "not
 * reporting" from "behind", "excluded" from "not covered", "dirty" from "staged". The word on the
 * badge cannot carry that, and a legend nobody scrolls to is a legend nobody reads.
 */
export const BADGE_MEANINGS: Readonly<Record<string, string>> = {
  current: "This machine is running the newest closure anything has given it: what CI staged "
    + "here is what is running. Whether that closure is origin/main is a separate "
    + "question — the revision under this badge is what answers it.",
  staged: "CI has already built and copied a newer closure onto this machine, and nothing has "
    + "switched to it. The change is here, one command away — this is the state the "
    + "button below acts on.",
  reboot: "The pending change replaces the kernel, initrd, kernel modules or systemd, which "
    + "activating cannot bring into use. Nothing on this fleet ever reboots a host "
    + "unattended; that stays a window somebody schedules.",
  blocked: "Auto-apply classified the staged change and refused to take it unattended, because "
    + "activating it would stop, start, restart or reload a unit this host has not "
    + "declared acceptable. That refusal is the design, not a fault.",
  dirty: "Built from a checkout with uncommitted changes on top of a real commit, so it is "
    + "reproducible from nothing in the repository. The commit it names is only the "
    + "committed part of what is running.",
  notreporting: "This machine publishes no nixos_* metric, so what it is running is UNKNOWN. "
    + "That is not the same as being out of date, and it wants the opposite repair: "
    + "find out why nobody can ask it.",
  apply_on: "Auto-apply is enabled here and reaching a verdict. When CI stages a closure whose "
    + "activation would disturb nothing this host has ruled out, it is taken unattended "
    + "within the timer's period.",
  apply_dry: "Auto-apply runs here in dry-run: it classifies every staged closure and never "
    + "switches. A change reaches this host only when a person takes it.",
  apply_blocked: "Auto-apply is enabled and is refusing this particular change — see the state "
    + "column for which check stopped it. It will keep refusing until somebody "
    + "activates it or the change stops touching units.",
  apply_undetermined: "Auto-apply's last pass could not measure anything, so it is not "
    + "refusing the change — it never got as far as classifying it.",
  apply_excluded: "Auto-apply is deliberately off on this host, with a written reason. "
    + "Excluded-on-purpose and never-wired-up are the same absence to a monitoring "
    + "system, which is why the reason is published.",
  apply_notcovered: "This host publishes no auto-apply metric at all, so nothing is watching "
    + "whether it ever activates what it is given.",
};

/** The console row spans the table, so it has to be told how wide the table is. */
export const FLEET_COLUMNS = 7;

/**
 * Past this, the page says out loud that what it shows is stale rather than looking fresh. With
 * the state pushed live that means the Prometheus read itself has been failing for this long.
 */
export const STALE_GRACE_SECONDS = 300;

/**
 * One badge, with its meaning on hover. `title` rather than a styled popover on purpose: this is a
 * dense table, and a positioned tooltip either clips against it or has to escape it, whereas the
 * browser's own never does.
 */
export function badge(kind: string, text: string, meaningKey?: string | null, extra = ""): Raw {
  let meaning = BADGE_MEANINGS[meaningKey ?? ""] ?? "";
  if (extra) meaning = `${extra} ${meaning}`.trim();
  return meaning
    ? html`<span class='badge ${kind} help' title='${meaning}'>${text}</span>`
    : html`<span class='badge ${kind}'>${text}</span>`;
}

/**
 * The auto-apply column: whether anything is taking staged closures on this host by itself.
 *
 * ITS STATES ARE KEPT DISTINCT FOR THE REASON `excludedBecause` EXISTS AT ALL. The applier's own
 * verdict (`up_to_date` / `applied`) is deliberately NOT the badge: both mean the same thing here
 * -- it is on and it reached a verdict -- and the state column beside it already says which. The
 * raw verdict stays on the hover so nothing is lost.
 */
export function autoApplyCell(row: MachineRow, now: number): Raw {
  if (!row.reporting) return html`<td class=none>&mdash;</td>`;
  if (row.excludedReason) {
    return html`<td>${badge("warn", "excluded", "apply_excluded", `Reason given: ${row.excludedReason}`)}<span class=hint>${row.excludedReason}</span></td>`;
  }
  if (!row.applyCovered) {
    return html`<td>${badge("alarm", "not covered", "apply_notcovered")}<span class=hint>publishes no auto-apply metric</span></td>`;
  }
  const state = row.autoApply;
  const cell = state === "blocked" && row.blockedReason === "dry_run"
    ? badge("warn", "dry-run", "apply_dry")
    : state === "blocked"
      ? badge("warn", "on, blocked", "apply_blocked")
      : state === "undetermined"
        ? badge("warn", "on, undetermined", "apply_undetermined")
        : badge("ok", "on", "apply_on", `Last verdict: ${state || "unknown"}.`);
  return html`<td>${cell}<span class=hint>last pass ${ago(row.lastVerdict, now)}</span></td>`;
}

/**
 * The commit this host's closure was built from, and how far that is from main.
 *
 * A LINE UNDER THE STATE RATHER THAN A COLUMN. It is the qualifier the `current` badge needs and
 * cannot carry: "current" means only that nothing newer has been GIVEN to this machine, which is a
 * different claim from being on origin/main -- a fleet whose staging run failed can be current
 * everywhere and behind everywhere at the same time.
 */
export function revisionLine(row: MachineRow): Raw {
  if (!row.revisionShort) {
    // NOT THE SAME AS "on main". A closure built from a tree with no git revision to stamp
    // matches no commit at all, so the distance is unmeasurable rather than zero.
    return html`built from no known commit, so its distance from main is unmeasurable`;
  }
  const distance = row.behind ? ` ${row.behind} behind main` : row.behind === 0 ? " on main" : " — distance from main unknown";
  const staged = row.stagedShort && row.stagedShort !== row.revisionShort
    ? html`, staged <span class=sha>${row.stagedShort}</span>`
    : "";
  return html`<span class=sha>${row.revisionShort}</span>${distance}${staged}`;
}

/**
 * One machine's `<tr>`, plus the console row under it when it has something to activate. ONE COPY
 * OF THIS MARKUP, for every render -- a second near-identical builder is how a refreshed row
 * quietly stops matching its neighbours.
 */
export function machineRows(row: MachineRow, now: number): Raw {
  // THE FULL STORE NAME ON HOVER, TWELVE CHARACTERS IN THE CELL. The hash is what an operator
  // compares between two hosts, and the first twelve characters settle it; truncating without
  // keeping the whole string reachable would make the one value you actually paste unpasteable.
  const closure = row.reporting
    ? html`<td class='sv' title='${row.closure}'>${shortClosure(row.closure)}${row.booted && row.booted !== row.closure ? html`<div class='sub'>booted ${shortClosure(row.booted)}</div>` : ""}</td><td>${row.nixpkgs}</td>`
    : html`<td class=none>&mdash;</td><td class=none>&mdash;</td>`;

  // WHAT WAS THE `revision` COLUMN, folded in under the state badge. Its two loud parts -- built
  // dirty, and staged-but-not-activated -- are already badges here, so a column of its own was
  // saying them twice; what is left is the SHA and its distance from main.
  const state = html`${badge(row.severity, row.state, row.stateKey, row.stateKey === "blocked" ? row.blockedReason : "")}${
    row.detail && row.severity !== "ok" ? html`<span class='hint detail'>${row.detail}</span>` : ""
  }${
    row.reporting
      ? html`<span class=hint>${revisionLine(row)}</span>`
      : html`<span class=hint>what it runs is UNKNOWN, which is not the same as behind</span>`
  }`;

  const env = row.env || "?";
  const main = html`<tr class='${row.severity}' data-machine='${row.name}'><td class='name'>${row.name}${
    row.hostname && row.hostname !== row.name ? html`<div class='sub'>${row.hostname}</div>` : ""
  }</td><td>${row.role || "?"} <span class=env style='background:${ENV_COLOR[env] ?? NEUTRAL}'>${env}</span></td><td class='mut'>${row.private || "—"}</td>${closure}${autoApplyCell(row, now)}<td>${state}</td></tr>`;
  return row.actionable ? html`${main}${actionRow(row)}` : main;
}

/**
 * The row under a machine that carries its button and the console the button writes into.
 *
 * A SECOND `<tr>` RATHER THAN AN EIGHTH COLUMN: the console needs the full width of the table, and a
 * column sized for a log would squeeze the seven columns that are the point of the page. `data-*`
 * attributes carry what the browser needs, so nothing is templated into a script.
 */
export function actionRow(row: MachineRow): Raw {
  const danger = needsConfirmation(row);
  // THE SAME WORDS AS THE SIBLING DASHBOARD -- "Bring to latest…" -- because the two screens are
  // read minutes apart and the same act must not be named twice. The production database keeps a
  // hint of its own: the first step is identical (a dry run that changes nothing), and what
  // differs is the confirmation the SWITCH demands, which is where the difference belongs.
  const hint = danger
    ? "⚠ production database. Activating can restart mongod, and the web tier's change streams stop with it — a dropped change stream does not reconnect by itself, so the site would serve stale showtimes until the app is restarted. Nothing changes until you confirm, and the confirmation is this machine's name typed out."
    : "reads the pin off the host and shows what activating it would restart — it changes nothing until you confirm";
  return html`<tr class='actions ${row.severity}'><td colspan='${FLEET_COLUMNS}' class='actioncell' data-machine='${row.name}' data-address='${row.public}' data-env='${row.env}' data-danger='${danger ? "1" : ""}'><div class='actionrow'><button class='applybtn needs-live' data-action=check>Bring to latest…</button><span class='hint'>${hint}</span></div>${consoleMarkup()}</td></tr>`;
}

/** `hidden` and closed to begin with: a page where every host has something staged would
 * otherwise be a stack of empty consoles pushing the table off the screen. */
export const consoleMarkup = (): Raw =>
  html`<details class='cons hidden'><summary>console</summary><pre class='out'></pre><div class='consfoot'></div></details>`;

/**
 * ONE BUTTON FOR THE WHOLE FLEET, built from the SAME `actionable` test as the per-machine buttons,
 * so its count and the buttons in the table can never disagree. The browser drives the same two
 * endpoints, in the same order, instead of a person clicking through them one at a time.
 */
export function bulkBlock(count: number): Raw {
  return html`<div class=fleetbulk><div class=applyrow><button id=fleetbulkbtn class='applybtn needs-live' data-action=bulk>Bring all to latest… (${count})</button><span class=hint>checks, then switches, every machine below with something staged — several at a time, in the order shown. Stops taking new machines at the first whose check or switch does not exit 0; whatever was already switched stays switched.</span></div><details id=fleetbulkcons class='cons hidden'><summary>console</summary><pre class=out></pre><div class=consfoot></div></details></div>`;
}

/** The whole page body for one state. `now` in unix seconds. */
export function renderFleet(state: FleetState, now: number): Raw {
  if (!state.ready) {
    return html`<div id=fleet><div class='note'>This page is still being read for the first time — the roster is a <code>nix eval</code> over the flake, which can take a minute or more on a cold start. It appears here by itself when it is done.</div></div>`;
  }
  if (state.buildError) {
    return html`<div id=fleet><div class='err'>This page could not be built at all, so it is telling you NOTHING about the fleet — not that the fleet is fine.<br><code>${state.buildError}</code></div></div>`;
  }
  const rows = state.rows;
  const count = (severity: MachineRow["severity"]) => rows.filter((row) => row.severity === severity).length;
  const parts: Raw[] = [];
  // "0 declared hosts" reads as an empty fleet rather than a broken query, which is the opposite
  // of what a failed roster means. Say which it is.
  const tally = rows.length
    ? html`${rows.length} declared host(s): ${count("ok")} current, ${count("warn")} needing attention, ${count("alarm")} not reporting`
    : html`<strong>roster unavailable</strong> — these counts are not a picture of the fleet`;
  const read = state.readAt ? html` · Prometheus read in ${state.took.toFixed(1)}s, ${ago(state.readAt / 1000, now)}` : "";
  parts.push(html`<div class='sub'>${tally}${read}</div>`);
  // THE WAY TO GET A CURRENT PAGE WITHOUT WAITING FOR THE TIMER: re-reads every source now; the
  // new state arrives by itself.
  parts.push(html`<div class=toolbar><button id=refreshbtn class=needs-live data-action=refresh>Refresh</button><span id=refreshnote class=hint></span></div>`);

  const age = state.readAt ? now - state.readAt / 1000 : 0;
  if (age > STALE_GRACE_SECONDS) {
    parts.push(html`<div class='err'>What these machines are running was last read ${Math.trunc(age)}s ago — every read since has failed. Everything below may be out of date.</div>`);
  }
  for (const error of state.errors) parts.push(html`<div class='err'>${error}</div>`);
  if (state.dirtyCheckout) {
    parts.push(html`<div class='note'>The infra checkout has uncommitted changes, so &ldquo;behind main&rdquo; below is measured against a tree that is not what CI would build.</div>`);
  }
  if (state.undeclared.length) {
    parts.push(html`<div class='err'>Publishing metrics but not declared in the flake: ${state.undeclared.join(", ")}</div>`);
  }
  const actionable = rows.filter((row) => row.actionable);
  if (actionable.length) parts.push(bulkBlock(actionable.length));
  parts.push(html`<table class=fleet><tr><th>machine</th><th>role &middot; env</th><th>address</th><th>closure</th><th>nixpkgs</th><th>auto-apply</th><th>state</th></tr>${rows.map((row) => machineRows(row, now))}</table>`);
  parts.push(html`<div class='sub'>The button activates the closure <b>CI already staged</b> on that host — nothing is built or evaluated here, so it can never reach a revision CI has not staged. Everything else is still nixos-auto-apply's job, or <code>nixos-rebuild switch --flake infra#&lt;host&gt;</code>.</div>`);
  return html`<div id=fleet>${parts}</div>`;
}

