#!/usr/bin/env python3
"""Is the Decodo residential proxy pool up? One line per port, exit 0 if at least one port reached the origin.

A port PASSES when https://kinowo.net/ answered THROUGH the tunnel with an HTTP status below 500 (2xx-4xx; a 3xx is
not followed, it is the origin answering). It FAILS on a proxy-side error: CONNECT `Tunnel connection failed: 503`,
407, a timeout, a refusal, a reset. Exit 0: at least one port passed; 1: none did (Decodo is down); 2: no credentials
(a config error, not a proxy outage).
"""
import os
import sys
import urllib.error
import urllib.request

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import script_module  # noqa: E402

headers = script_module.load(os.path.join(HERE, "check-security-headers.py"))
PROPERTIES = os.path.join(HERE, "..", "..", "worker", "src", "main", "resources", "residential-proxy.properties")
URL = "https://kinowo.net/"
TIMEOUT = 20


def proxy_pool(path=PROPERTIES):
    """(host, [ports]) from the committed residential-proxy.properties."""
    props = {}
    with open(path) as handle:
        for line in handle:
            line = line.strip()
            if line and not line.startswith("#") and "=" in line:
                key, value = line.split("=", 1)
                props[key.strip()] = value.strip()
    return props["host"], [p.strip() for p in props["ports"].split(",") if p.strip()]


def probe(host, port, user, password, url=URL, timeout=TIMEOUT):
    """(passed, text) for one port."""
    proxy = "http://%s:%s@%s:%s" % (urllib.request.quote(user, safe=""), urllib.request.quote(password, safe=""), host, port)
    opener = urllib.request.build_opener(
        headers._NoRedirect(), urllib.request.ProxyHandler({"https": proxy, "http": proxy}))
    request = urllib.request.Request(url, method="HEAD", headers={"User-Agent": "kinowo-decodo-probe"})
    try:
        with opener.open(request, timeout=timeout) as response:
            status = response.status
    except urllib.error.HTTPError as error:
        status = error.code
    except Exception as error:  # noqa: BLE001 -- any failure is a proxy-side failure, never a traceback
        return False, "FAIL " + headers.redact("%s" % (getattr(error, "reason", None) or error))
    return (True, "ok %s" % status) if status < 500 else (False, "FAIL origin answered %s" % status)


def main():
    user, password = headers._proxy_credentials()
    if not user or not password:
        print("DECODO_PROXY_USER / DECODO_PROXY_PASS are not set: config error, not a proxy outage")
        return 2
    host, ports = proxy_pool()
    passed = 0
    for port in ports:
        ok, text = probe(host, port, user, password)
        passed += ok
        print("port %s: %s" % (port, text), flush=True)
    if passed:
        print("DECODO UP: %d/%d ports" % (passed, len(ports)))
        return 0
    print("DECODO DOWN: 0/%d ports reached %s through %s" % (len(ports), URL, host))
    return 1


if __name__ == "__main__":
    sys.exit(main())
