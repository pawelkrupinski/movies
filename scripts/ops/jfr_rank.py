#!/usr/bin/env python3
"""Where a JFR recording's CPU went: its jdk.ExecutionSample stacks ranked four ways — by thread
(numbers folded, so a pool's threads add up), by top frame, by the first frame of OUR code, and by
our code's frames inclusively (a frame counts once per sample it appears in).

  python3 scripts/ops/jfr_rank.py <recording.jfr> [--focus TEXT] [--app-packages services,models,...]

  --focus          rank only the samples whose stack contains TEXT (a class or method name)
  --app-packages   the package roots that are "our code" (default: this repo's)

Needs the JDK's `jfr` on PATH. A live JVM records one with `jcmd <pid> JFR.start duration=120s filename=<f>.jfr`."""
import argparse
import collections
import re
import subprocess
import sys

APP_PACKAGES = "services,models,modules,tools,clients,settings,controllers,views"


def samples(jfr_print_text):
    """Each jdk.ExecutionSample's (thread name, frames top first) from `jfr print` text."""
    out = []
    for event in jfr_print_text.split("jdk.ExecutionSample {")[1:]:
        thread = re.search(r'sampledThread = "([^"]*)"', event)
        frames = re.findall(r"\n\s+([\w.$]+\.[\w$<>]+)\(", event)
        out.append((thread.group(1) if thread else "?", frames, event))
    return out


def rank(events, app_packages, focus=None):
    """{"by thread"|"top frame"|"first app frame"|"inclusive app": Counter} and the sample count."""
    app = re.compile(r"^(" + "|".join(re.escape(p) for p in app_packages) + r")\.")
    if focus:
        events = [e for e in events if focus in e[2]]
    thread, top, first, inclusive = (collections.Counter() for _ in range(4))
    for name, frames, _ in events:
        thread[re.sub(r"\d+", "N", name)] += 1
        if frames:
            top[frames[0]] += 1
        ours = [f for f in frames if app.match(f)]
        if ours:
            first[ours[0]] += 1
        for f in set(ours):
            inclusive[f] += 1
    return {"by thread": thread, "top frame": top, "first app frame": first, "inclusive app": inclusive}, len(events)


def main(argv):
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("recording")
    parser.add_argument("--focus")
    parser.add_argument("--app-packages", default=APP_PACKAGES)
    args = parser.parse_args(argv)
    text = subprocess.run(["jfr", "print", "--events", "jdk.ExecutionSample", "--stack-depth", "64", args.recording],
                          capture_output=True, text=True, check=True).stdout
    ranks, n = rank(samples(text), args.app_packages.split(","), args.focus)
    print(f"CPU samples {n}" + (f" (containing {args.focus})" if args.focus else ""))
    for title, limit in [("by thread", 12), ("top frame", 12), ("first app frame", 25), ("inclusive app", 30)]:
        print("--", title)
        for name, v in ranks[title].most_common(limit):
            print(f"{v / max(1, n) * 100:5.1f}% {name}")


if __name__ == "__main__":
    main(sys.argv[1:])
