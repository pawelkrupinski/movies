#!/usr/bin/env python3
"""tool_http.py: ONE contract suite run against both transports -- urllib against a local server and the in-memory
one -- both answering from the same `answer` function, so the in-memory `Http` a unit test hands a tool behaves as
the real one: a GET's query encoded, a POST's JSON body and content type, a 4xx an answer and not an error, a body
decoded by its charset, a redirect followed (a POST's turned into a GET) or not when asked, a timeout required, and
nothing answering an HttpFailed."""
from __future__ import annotations

import json
import os
import socket
import sys
import threading
import unittest
import urllib.parse
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from typing import Any, Optional

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from seams import tool_http  # noqa: E402
from seams.tool_http import Http, HttpFailed, Request, Response  # noqa: E402

DOWN = "http://127.0.0.1:9"  # the discard port: nothing listens there


def answer(request: Request) -> Response:
    """The one server both legs answer from."""
    if request.url.startswith(DOWN):
        raise HttpFailed(f"{request.url}: connection refused")
    parsed = urllib.parse.urlsplit(request.url)
    if parsed.path == "/films":
        query = dict(urllib.parse.parse_qsl(parsed.query))
        return Response(200, json.dumps({"city": query.get("city")}).encode(), (("content-type", "application/json"),))
    if parsed.path == "/echo":
        said = {"method": request.method, "body": (request.body or b"").decode(),
                "type": dict(request.headers).get("content-type"), "token": dict(request.headers).get("x-token")}
        return Response(200, json.dumps(said).encode(), (("content-type", "application/json"),))
    if parsed.path == "/moved":
        return Response(302, b"", (("location", "/echo"),))
    if parsed.path == "/latin":
        return Response(200, "zażółć".encode("iso-8859-2"), (("content-type", "text/plain; charset=iso-8859-2"),))
    return Response(404, b"none here", (("content-type", "text/plain"),))


class Contract:
    """Holds the suite out of unittest's discovery: only its subclasses run."""

    class HttpContract(unittest.TestCase):
        base = ""

        def http(self) -> Http:
            raise NotImplementedError

        def setUp(self) -> None:
            self.h = self.http()

        def test_a_get_encodes_its_query(self) -> None:
            got = self.h.get(self.base + "/films", 5, query={"city": "Łódź & co"})
            self.assertEqual((200, True, {"city": "Łódź & co"}), (got.status, got.ok, got.json()))

        def test_a_post_sends_its_json_body_and_headers(self) -> None:
            got = self.h.post(self.base + "/echo", 5, json_body={"a": 1}, headers={"X-Token": "t"}).json()
            self.assertEqual({"method": "POST", "body": '{"a": 1}', "type": "application/json", "token": "t"}, got)

        def test_a_4xx_is_an_answer_not_an_error(self) -> None:
            got = self.h.get(self.base + "/nothing", 5)
            self.assertEqual((404, False, "none here"), (got.status, got.ok, got.text()))

        def test_a_body_is_decoded_by_its_charset(self) -> None:
            self.assertEqual("zażółć", self.h.get(self.base + "/latin", 5).text())

        def test_a_redirect_is_followed_a_post_turning_into_a_get_or_not_when_asked(self) -> None:
            followed = self.h.post(self.base + "/moved", 5, body=b"x").json()
            self.assertEqual(("GET", ""), (followed["method"], followed["body"]))
            kept = self.h.post(self.base + "/moved", 5, body=b"x", follow_redirects=False)
            self.assertEqual((302, "/echo"), (kept.status, kept.header("Location")))

        def test_a_timeout_is_required(self) -> None:
            with self.assertRaises(ValueError):
                self.h.get(self.base + "/films", 0)

        def test_nothing_answering_is_http_failed(self) -> None:
            with self.assertRaises(HttpFailed):
                self.h.get(DOWN + "/films", 2)


class Server(BaseHTTPRequestHandler):
    def log_message(self, format: str, *args: Any) -> None:  # noqa: A002 (http.server's name)
        return None

    def reply(self, method: str) -> None:
        length = int(self.headers.get("Content-Length") or 0)
        body: Optional[bytes] = self.rfile.read(length) if length else None
        url = f"http://{self.headers['Host']}{self.path}"
        got = answer(Request(method, url, tool_http.lowered(dict(self.headers.items())), body, 5))
        self.send_response(got.status)
        for name, value in got.headers:
            self.send_header(name, value)
        self.send_header("Content-Length", str(len(got.body)))
        self.end_headers()
        self.wfile.write(got.body)

    def do_GET(self) -> None:  # noqa: N802 (http.server's name)
        self.reply("GET")

    def do_POST(self) -> None:  # noqa: N802
        self.reply("POST")


class UrllibContractTest(Contract.HttpContract):
    @classmethod
    def setUpClass(cls) -> None:
        cls.server = ThreadingHTTPServer(("127.0.0.1", 0), Server)
        threading.Thread(target=cls.server.serve_forever, daemon=True).start()
        cls.base = f"http://127.0.0.1:{cls.server.server_address[1]}"

    @classmethod
    def tearDownClass(cls) -> None:
        cls.server.shutdown()
        cls.server.server_close()

    def http(self) -> Http:
        return Http.here()

    def test_the_discard_port_is_really_closed(self) -> None:
        with socket.socket() as s:
            self.assertNotEqual(0, s.connect_ex(("127.0.0.1", 9)))


class InMemoryContractTest(Contract.HttpContract):
    base = "http://films.test"

    def http(self) -> Http:
        self.transport = tool_http.InMemoryTransport(answer)
        return Http(self.transport)

    def test_every_request_sent_is_recorded_with_its_timeout(self) -> None:
        self.h.get(self.base + "/moved", 7)
        self.assertEqual([("GET", self.base + "/moved", 7), ("GET", self.base + "/echo", 7)],
                         [(r.method, r.url, r.timeout) for r in self.transport.sent])


if __name__ == "__main__":
    unittest.main()
