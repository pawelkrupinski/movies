import type { FastifyInstance } from "fastify";
import type { Snapshot, Store } from "./store.js";

/**
 * One screen of the dashboard. The server renders `renderBody` for the first paint; the browser
 * bundle `web/<name>.ts` then subscribes to `/events/<name>` and re-renders from each snapshot with
 * the SAME view functions, so the two can never disagree about what a state looks like.
 */
export interface Page<T> {
  readonly name: string;
  readonly path: string;
  readonly title: string;
  readonly store: Store<T>;
  start(): void | Promise<void>;
  stop(): void;
  renderBody(snapshot: Snapshot<T>): string;
  routes?(app: FastifyInstance): void;
}
