/**
 * THE RUNNING STATE -- what each host says about itself, through the `nixos_*` metrics that
 * modules/fleet/observability.nix publishes via node_exporter's textfile collector.
 *
 * HOW PROMETHEUS IS REACHED, and why it is not a plain URL. Prometheus binds its PRIVATE address
 * only (roles/prometheus.nix: never 0.0.0.0, because its web UI has no authentication). That
 * subnet is not routable from a laptop without a tunnel, so the path from here is ssh to
 * monitoring-1's public address and curl from there: one extra hop, and no tunnel that has to be
 * up first for the page to say anything.
 */
import { runCommand } from "../exec.js";
import type { Sample } from "./model.js";
import { shellQuote, sshBinary, SSH_OPTIONS } from "./ssh.js";

export const MONITORING_SSH = (): string => process.env.KINOWO_MONITORING_SSH ?? "root@128.140.49.167";
export const PROM_URL = (): string => process.env.KINOWO_PROM_URL ?? "http://10.20.0.11:9090";

/**
 * The series the page reads. One query, not one per metric: Prometheus returns them all in a
 * single instant vector and joining locally is far cheaper than a dozen round trips down an ssh
 * pipe.
 */
export const PROM_SELECTOR = '{__name__=~"nixos_.*|node_os_info"}';
export const SSH_TIMEOUT_MS = 30_000;

/** Percent-encodes everything but [A-Za-z0-9-_.~] -- stricter than encodeURIComponent, which
 * leaves `'()*!` bare, and this URL crosses a remote shell. */
export const urlQuote = (value: string): string =>
  [...new TextEncoder().encode(value)]
    .map((byte) => (/[A-Za-z0-9\-_.~]/.test(String.fromCharCode(byte)) ? String.fromCharCode(byte) : `%${byte.toString(16).toUpperCase().padStart(2, "0")}`))
    .join("");

export function promArgv(): string[] {
  const url = `${PROM_URL()}/api/v1/query?query=${urlQuote(PROM_SELECTOR)}`;
  // The `--` before the remote command is not decoration: without it an address beginning with a
  // dash would be read by ssh as a flag. The URL is quoted because ssh hands the remote side one
  // string for its shell to split, and `?` is a glob there.
  return [sshBinary(), ...SSH_OPTIONS, MONITORING_SSH(), "--", "curl", "-sS", "--max-time", "15", shellQuote(url)];
}

/** One instant query. A failed read is an error, NEVER an empty fleet. */
export async function promSeries(): Promise<{ series: Sample[] } | { error: string }> {
  const result = await runCommand(promArgv(), { timeoutMs: SSH_TIMEOUT_MS });
  if (result.code !== 0) {
    const why = result.timedOut ? `timed out after ${SSH_TIMEOUT_MS / 1000}s` : result.stderr.trim() || "no output";
    return { error: `could not reach Prometheus through ${MONITORING_SSH()}: ${why}` };
  }
  let body: { status?: string; data?: { result?: Sample[] } };
  try {
    body = JSON.parse(result.stdout) as typeof body;
  } catch {
    return { error: "Prometheus returned unparseable JSON (is it bound to the address we queried?)" };
  }
  if (body.status !== "success") return { error: `Prometheus answered status=${body.status ?? "None"}` };
  return { series: body.data?.result ?? [] };
}
