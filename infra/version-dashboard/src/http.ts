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

/** Every outbound HTTP request goes through here. Throws on a network failure; any status is an
 * answer the caller interprets. */
export const httpRequest: HttpClient = (url, init) => client(url, init);

/** Tests only: swap the client (test/setup.ts installs one that refuses). */
export function setHttpClient(next: HttpClient): HttpClient {
  const previous = client;
  client = next;
  return previous;
}
