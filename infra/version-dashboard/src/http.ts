export interface HttpResponse {
  readonly status: number;
  readonly headers: Headers;
  readonly body: string;
}

export interface HttpRequest {
  /** GET when omitted. */
  readonly method?: string;
  readonly headers?: Record<string, string>;
  readonly body?: string;
  readonly timeoutMs: number;
  /** Non-2xx statuses this caller reads as an answer (a 404 meaning "none here"). Every other
   * non-2xx throws an HttpError: a status nobody named is a failed read, never data. */
  readonly acceptStatuses?: readonly number[];
}

/** The server answered, with a status that is not 2xx and that the caller did not accept. */
export class HttpError extends Error {
  constructor(readonly url: string, readonly status: number, body: string) {
    super(`HTTP ${status} from ${url}${body.trim() ? `: ${body.trim().slice(0, 300)}` : ""}`);
    this.name = "HttpError";
  }
}

export type HttpClient = (url: string, init: HttpRequest) => Promise<HttpResponse>;

const fetchClient: HttpClient = async (url, init) => {
  const response = await fetch(url, {
    method: init.method ?? "GET",
    headers: init.headers,
    body: init.body,
    signal: AbortSignal.timeout(init.timeoutMs),
  });
  return { status: response.status, headers: response.headers, body: await response.text() };
};

let client: HttpClient = fetchClient;

/** Every outbound HTTP request goes through here. Throws on a network failure, and an HttpError on
 * any non-2xx the caller did not list in `acceptStatuses` -- it used to hand every status back for
 * the caller to interpret, which is how a failed read gets interpreted as "no data". */
export const httpRequest: HttpClient = async (url, init) => {
  const response = await client(url, init);
  const ok = response.status >= 200 && response.status < 300;
  if (!ok && !(init.acceptStatuses ?? []).includes(response.status)) throw new HttpError(url, response.status, response.body);
  return response;
};

/** Tests only: swap the client (test/setup.ts installs one that refuses). */
export function setHttpClient(next: HttpClient): HttpClient {
  const previous = client;
  client = next;
  return previous;
}
