/**
 * Every ssh this page makes, built one way -- to monitoring-1 for Prometheus, and to a fleet host
 * for a check or a switch.
 */
import { binary } from "../config.js";

export const SSH_OPTIONS: readonly string[] = ["-o", "BatchMode=yes", "-o", "ConnectTimeout=8", "-o", "StrictHostKeyChecking=accept-new"];

export const sshBinary = (): string => binary("ssh", ["/usr/bin/ssh"]) ?? "ssh";

/**
 * ROOT, not an admin over sudo. This is where this fleet differs from bitcashier's, whose hosts
 * grant root no keys at all and whose scripts are therefore full of `sudo -n`. Here root ssh is the
 * only access there is -- it is already how this page reads Prometheus -- so the scripts run their
 * commands bare. If that ever changes, the change is `sudo -n` in two scripts and a user here, not
 * a redesign.
 */
export const fleetSshUser = (): string => process.env.KINOWO_FLEET_SSH_USER ?? "root";

/**
 * The ssh command, built the same way for every machine and every phase.
 *
 * THE PUBLIC ADDRESS, taken from `fleet.publicAddress` in the flake -- never hardcoded here and
 * never the private one. This fleet has no VPN and no jump host, so 10.20.0.x is unreachable from a
 * laptop. Reading the address from the roster means a host that gets a new public IP needs no edit
 * here.
 *
 * `bash -s --` feeds the script over stdin instead of interpolating it into a remote command line:
 * the script is multi-line and quoted, and building it into an argv is how a shell-quoting bug
 * turns into an arbitrary remote command.
 */
export function fleetSshArgv(address: string, args: readonly string[] = []): string[] {
  return [sshBinary(), ...SSH_OPTIONS, "-l", fleetSshUser(), address, "bash", "-s", "--", ...args];
}

/** POSIX shell quoting, as Python's shlex.quote. */
export function shellQuote(value: string): string {
  if (value === "") return "''";
  if (/^[\w@%+=:,./-]+$/.test(value)) return value;
  return `'${value.replaceAll("'", `'"'"'`)}'`;
}
