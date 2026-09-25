/**
 * The mobile page in the browser: re-render every pushed snapshot with the server's own view, and
 * the Refresh button.
 */
import type { MobileState } from "../src/mobile/model.js";
import { renderMobile } from "../src/mobile/view.js";
import { connect } from "./live.js";

export interface Deps {
  fetch: (url: string, init?: RequestInit) => Promise<Response>;
  /** Epoch ms, for the ages in the re-rendered page. */
  now: () => number;
}

const deps: Deps = {
  fetch: (url, init) => fetch(url, init),
  now: () => Date.now(),
};

/** Tests only. */
export function setDeps(next: Partial<Deps>): void {
  Object.assign(deps, next);
}

let latest: MobileState | null = null;

export function applySnapshot(state: MobileState): void {
  latest = state;
  const app = document.getElementById("app");
  if (app) app.innerHTML = renderMobile(state, deps.now()).__raw;
}

/** Asks for a rebuild; the result arrives as a snapshot, which re-renders the page by itself. */
export async function refresh(): Promise<void> {
  const note = document.getElementById("refreshnote");
  if (note) note.textContent = "rebuilding…";
  try {
    await deps.fetch("/mobile/refresh", { method: "POST" });
  } catch {
    if (note) note.textContent = "could not reach the dashboard";
  }
}

export function start(): void {
  const initial = document.getElementById("initial")?.textContent;
  if (initial) latest = (JSON.parse(initial) as { state: MobileState }).state;
  document.addEventListener("click", (event) => {
    const button = (event.target as Element | null)?.closest<HTMLButtonElement>("button[data-action=refresh]");
    if (button && !button.disabled) void refresh();
  });
  connect<MobileState>("mobile", (snapshot) => applySnapshot(snapshot.state));
  // The "built … ago" line ages between snapshots; re-render it from the state already held.
  setInterval(() => {
    if (latest) applySnapshot(latest);
  }, 30_000);
}

if (typeof document !== "undefined" && document.body?.dataset.page === "mobile" && document.getElementById("initial")) start();
