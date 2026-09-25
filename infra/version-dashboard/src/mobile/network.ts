/**
 * Telling a network blip from an answer. The line drawn here is the one the retries, the retry
 * floor and the self-restart all key off.
 */

/** The store answered, with a status that is not 2xx. */
export class HttpError extends Error {
  constructor(readonly url: string, readonly status: number, body: string) {
    super(`HTTP ${status} from ${url}${body.trim() ? `: ${body.trim().slice(0, 300)}` : ""}`);
    this.name = "HttpError";
  }
}

/** Codes Node and undici give a round trip the network refused before any server answered. */
const TRANSIENT_CODES = new Set([
  "ENOTFOUND", "EAI_AGAIN", "ECONNREFUSED", "ECONNRESET", "ECONNABORTED", "ETIMEDOUT", "EPIPE",
  "EHOSTUNREACH", "EHOSTDOWN", "ENETUNREACH", "ENETDOWN",
  "UND_ERR_CONNECT_TIMEOUT", "UND_ERR_HEADERS_TIMEOUT", "UND_ERR_BODY_TIMEOUT", "UND_ERR_SOCKET", "UND_ERR_CLOSED",
]);

/** A certificate or TLS-protocol refusal: the same answer on every retry. */
const TLS_REFUSAL = /^(CERT_|ERR_SSL_|ERR_TLS_|UNABLE_TO_|DEPTH_ZERO_SELF_SIGNED_CERT$|SELF_SIGNED_CERT_IN_CHAIN$|HOSTNAME_MISMATCH$)/;

function causes(error: unknown): unknown[] {
  const chain: unknown[] = [];
  for (let current = error; current && chain.length < 5; current = (current as { cause?: unknown }).cause) chain.push(current);
  return chain;
}

const codeOf = (error: unknown): string => {
  const code = (error as { code?: unknown } | null)?.code;
  return typeof code === "string" ? code : "";
};

/**
 * Whether `error` is the OS/network refusing the round trip outright -- DNS failing to resolve, a
 * connection refused or reset, a timeout -- as opposed to the server answering with an error (401,
 * 429, a real 5xx outage). Retrying the first kind a few seconds later routinely succeeds; retrying
 * the second immediately repeats the same answer and burns a JWT signature for nothing.
 *
 * A TLS REFUSAL IS NOT TRANSIENT EITHER: an expired or untrusted certificate, or a protocol
 * mismatch, answers the same way on every retry, and counting it towards the self-restart would
 * restart the process over something a fresh one cannot fix. The exception is the far end DROPPING
 * the connection mid-handshake -- that is ECONNRESET, the network, same as any reset.
 *
 * A LOCAL ERROR IS NOT THE NETWORK: a missing .p8 or credentials file (ENOENT) is a fault here.
 */
export function isTransientNetworkError(error: unknown): boolean {
  if (error instanceof HttpError) return false;
  for (const link of causes(error)) {
    if (link instanceof HttpError) return false;
    const name = (link as { name?: unknown } | null)?.name;
    // AbortSignal.timeout: the round trip took longer than we were willing to wait.
    if (name === "TimeoutError") return true;
    const code = codeOf(link);
    if (TLS_REFUSAL.test(code)) return false;
    if (TRANSIENT_CODES.has(code)) return true;
  }
  return false;
}

/** An error as one line with its cause chain: Node's bare "fetch failed" says nothing. */
export function describeError(error: unknown): string {
  const [first, ...rest] = causes(error);
  const line = (link: unknown) => (link instanceof Error ? link.message : String(link));
  const head = first instanceof Error ? `${first.name}: ${first.message}` : String(first);
  const detail = rest.map(line).filter((message) => message && !head.includes(message));
  return detail.length ? `${head} (${detail.join("; ")})` : head;
}

export const NETWORK_RETRY_DELAYS_MS: readonly number[] = [1_000, 3_000];

/**
 * Runs `work` up to three times (the delays above between attempts), retrying ONLY a transient
 * network failure -- the class a fresh attempt a few seconds later can plausibly clear. Anything
 * else (an HTTP status, bad JSON, a missing credential) propagates on the first attempt untouched,
 * since another attempt right away would not change it. Long enough to ride out a one-off blip,
 * short enough that a build never waits long on a retry that was always going to fail.
 */
export async function withNetworkRetries<T>(
  work: () => Promise<T>,
  sleep: (ms: number) => Promise<void>,
  delays: readonly number[] = NETWORK_RETRY_DELAYS_MS,
): Promise<T> {
  let last: unknown;
  for (const delay of [0, ...delays]) {
    if (delay) await sleep(delay);
    try {
      return await work();
    } catch (error) {
      if (!isTransientNetworkError(error)) throw error;
      last = error;
    }
  }
  throw last;
}
