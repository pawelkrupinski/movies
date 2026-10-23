/**
 * `npm run once`: read the fleet once, print it as text, exit. It never serves, so nothing can post
 * to it, and it starts no action of its own -- safe to run from a script or a cron job.
 */
import { FleetLive } from "fleetkit/fleet/live";
import { kinowoFleet } from "./fleet.js";

const live = new FleetLive(kinowoFleet());
await live.boot();
const { state } = live.store.get();
for (const error of [state.buildError, state.rosterError, state.promError, state.forgeError, state.checkout.error]) {
  if (error) console.log(`error: ${error}`);
}
for (const row of state.machines) {
  const what = !row.reporting ? "not reporting" : row.stagedPending ? "staged, not activated" : row.rebootRequired ? "reboot owed" : "current";
  console.log(`${row.name.padEnd(14)} ${row.severity.padEnd(6)} ${what.padEnd(28)} closure=${(row.storeHash ?? "").slice(0, 12).padEnd(14)} rev=${(row.revisionCommit ?? "").slice(0, 8)} nixpkgs=${row.nixpkgs ?? ""}`);
}
process.exit(0);
