/**
 * `npm run once`: read the fleet once, print it as text, exit. It never serves, so nothing can post
 * to it, and it starts no action of its own -- safe to run from a script or a cron job.
 */
import { INFRA_DIR } from "./config.js";
import { FleetLive, realSources } from "./fleet/live.js";
import { shortClosure } from "./fleet/read.js";

const live = new FleetLive((onRosterChange) => realSources(INFRA_DIR, onRosterChange));
await live.boot();
const { state } = live.store.get();
for (const error of state.errors) console.log(`error: ${error}`);
for (const row of state.rows) {
  console.log(`${row.name.padEnd(14)} ${row.severity.padEnd(6)} ${row.state.padEnd(28)} closure=${shortClosure(row.closure).padEnd(14)} rev=${row.revisionShort} nixpkgs=${row.nixpkgs}`);
}
process.exit(0);
