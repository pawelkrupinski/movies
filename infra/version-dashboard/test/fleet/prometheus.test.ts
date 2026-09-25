import { afterEach, describe, expect, it } from "vitest";
import { setExecutor, type CommandResult, type Executor } from "../../src/exec.js";
import { promArgv, promSeries, urlQuote } from "../../src/fleet/prometheus.js";
import { sshBinary } from "../../src/fleet/ssh.js";

let restore: Executor | null = null;
afterEach(() => {
  if (restore) setExecutor(restore);
  restore = null;
});

function answer(result: Partial<CommandResult>): string[][] {
  const seen: string[][] = [];
  restore = setExecutor(async (argv) => {
    seen.push([...argv]);
    return { code: 0, stdout: "", stderr: "", timedOut: false, ...result };
  });
  return seen;
}

describe("the Prometheus read", () => {
  it("is one ssh to monitoring-1 that curls the private address, with the URL quoted for the remote shell", () => {
    expect(promArgv()).toEqual([
      sshBinary(), "-o", "BatchMode=yes", "-o", "ConnectTimeout=8", "-o", "StrictHostKeyChecking=accept-new",
      "root@128.140.49.167", "--", "curl", "-sS", "--max-time", "15",
      "'http://10.20.0.11:9090/api/v1/query?query=%7B__name__%3D~%22nixos_.%2A%7Cnode_os_info%22%7D'",
    ]);
  });

  it("encodes everything but unreserved characters, as the Python page did", () => {
    expect(urlQuote("a-b_c.d~e f'()*!é")).toBe("a-b_c.d~e%20f%27%28%29%2A%21%C3%A9");
  });

  it("returns the series of a successful query", async () => {
    const seen = answer({ stdout: JSON.stringify({ status: "success", data: { result: [{ metric: { __name__: "x" }, value: [1, "1"] }] } }) });
    expect(await promSeries()).toEqual({ series: [{ metric: { __name__: "x" }, value: [1, "1"] }] });
    expect(seen).toHaveLength(1);
  });

  it("says which failure it was -- never an empty fleet", async () => {
    answer({ code: 255, stderr: "ssh: connect to host 128.140.49.167 port 22: Operation timed out" });
    expect(await promSeries()).toEqual({ error: "could not reach Prometheus through root@128.140.49.167: ssh: connect to host 128.140.49.167 port 22: Operation timed out" });
    answer({ stdout: "<html>" });
    expect(await promSeries()).toEqual({ error: "Prometheus returned unparseable JSON (is it bound to the address we queried?)" });
    answer({ stdout: JSON.stringify({ status: "error" }) });
    expect(await promSeries()).toEqual({ error: "Prometheus answered status=error" });
    answer({ code: null, timedOut: true });
    expect(await promSeries()).toEqual({ error: "could not reach Prometheus through root@128.140.49.167: timed out after 30s" });
  });
});
