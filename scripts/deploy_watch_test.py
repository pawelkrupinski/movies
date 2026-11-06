#!/usr/bin/env python3
"""Unit tests for deploy_watch.py (scripts/deploy-watch): the rollout waited on per country from kube-state-metrics'
pod images, then each check read every interval over the window and judged PASS/FAIL per country -- a value over its
threshold, a country with no series (a gauge the image does not export yet), a rollout that never completes. Every
Prometheus query is answered by a fake, the clock is a fake one. The last test runs scripts/deploy-watch itself with
DEPLOY_WATCH_QUERY pointing at a stub.

    python3 scripts/deploy_watch_test.py
"""
import io
import json
import os
import subprocess
import sys
import tempfile
import unittest
from contextlib import redirect_stderr, redirect_stdout

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import deploy_watch  # noqa: E402

NEW, OLD = "main-20261106132736-9cff742", "main-20261031123203-f32515d"
ALL = ["pl", "de", "uk", "us", "es"]


def pods(tags):
    """kube_pod_container_info as Prometheus answers it: one series per worker pod, its image."""
    return [{"metric": {"container": "worker", "namespace": "kinowo", "pod": f"worker-{cc}-68c567d7dd-{i}{n}",
                        "image": f"registry.kinowo.net/movies-worker:{tag}"}, "value": [0, "1"]}
            for cc, ts in tags.items() for n, tag in enumerate(ts if isinstance(ts, list) else [ts]) for i in "a"]


def series(values):
    return [{"metric": {"country": cc}, "value": [0, str(v)]} for cc, v in values.items()]


CHECKS = ("# name\top\tthreshold\tmissing\tpromql\n"
          "held listings\t<=\t1000\tfail\tmax by (country) (kinowo_worker_identity_model_held_listings)\n"
          "flips\t<=\t2\tzero\tsum by (country) (increase(kinowo_worker_identity_take_flips_total[1h]))\n")


class Fake:
    """Prometheus answering from scripted readings: each rollout read pops the next pod set (the last one stays)."""

    def __init__(self, rollouts, readings):
        self.rollouts, self.readings, self.now, self.asked = list(rollouts), readings, 0.0, []

    def query(self, promql):
        self.asked.append(promql)
        if "kube_pod_container_info" in promql:
            return self.rollouts.pop(0) if len(self.rollouts) > 1 else self.rollouts[0]
        for key, answers in self.readings.items():
            if key in promql:
                return answers.pop(0) if len(answers) > 1 else answers[0]
        raise AssertionError(f"unexpected query {promql}")

    def clock(self):
        return self.now

    def sleep(self, seconds):
        self.now += seconds


def watch(fake, *args, checks=CHECKS):
    with tempfile.NamedTemporaryFile("w", suffix=".tsv", delete=False) as f:
        f.write(checks)
    out, err = io.StringIO(), io.StringIO()
    try:
        with redirect_stdout(out), redirect_stderr(err):
            code = deploy_watch.main(["--image", NEW, "--checks", f.name, *args],
                                     query=fake.query, clock=fake.clock, sleep=fake.sleep)
    finally:
        os.unlink(f.name)
    return code, out.getvalue(), err.getvalue()


class DeployWatchTest(unittest.TestCase):
    def test_waits_for_every_worker_on_the_image_then_passes_a_clean_window(self):
        half = pods({**{cc: NEW for cc in ALL[:3]}, **{cc: OLD for cc in ALL[3:]}})
        fake = Fake([half, pods({cc: NEW for cc in ALL})],
                    {"held_listings": [series({cc: 0 for cc in ALL})], "take_flips": [series({"pl": 1})]})
        code, out, err = watch(fake, "--every", "5m", "--window", "15m", "--poll", "30s")
        self.assertEqual(0, code, out + err)
        self.assertIn("ROLLED", err)
        self.assertIn("waiting: us es", err, "names the countries still on the old image")
        self.assertIn("PASS\theld listings", out)
        self.assertIn("PASS\tflips", out)
        self.assertEqual(4, sum("held_listings" in q for q in fake.asked), "readings at 0, 5, 10 and 15 minutes")

    def test_a_pod_of_the_old_image_beside_the_new_one_is_not_rolled(self):
        fake = Fake([pods({**{cc: NEW for cc in ALL}, "de": [NEW, OLD]})], {})
        code, _, err = watch(fake, "--poll", "1m", "--rollout-timeout", "3m")
        self.assertEqual(1, code, err)
        self.assertIn("NOT ROLLED", err)
        self.assertIn("de", err.split("NOT ROLLED")[1])

    def test_a_value_over_its_threshold_fails_naming_country_value_and_reading(self):
        fake = Fake([pods({cc: NEW for cc in ALL})],
                    {"held_listings": [series({cc: 0 for cc in ALL}), series({**{cc: 0 for cc in ALL}, "de": 1500})],
                     "take_flips": [series({})]})
        code, out, err = watch(fake, "--every", "5m", "--window", "5m")
        self.assertEqual(1, code, out + err)
        self.assertIn("FAIL\theld listings", out)
        self.assertIn("de=1500", out)
        self.assertIn("RED", err, "a failing reading is printed when it is read, not only at the end")
        self.assertIn("PASS\tflips", out)

    def test_a_country_without_the_series_fails_unless_missing_reads_zero(self):
        fake = Fake([pods({cc: NEW for cc in ALL})],
                    {"held_listings": [series({cc: 0 for cc in ALL if cc != "es"})], "take_flips": [series({})]})
        code, out, _ = watch(fake, "--window", "0m")
        self.assertEqual(1, code)
        self.assertIn("es=no series", out)
        self.assertIn("PASS\tflips", out, "a counter not yet incremented reads 0")

    def test_a_gauge_not_yet_reported_after_boot_waits_for_its_first_value(self):
        # The worker's gauges read NaN from boot until their first report (the held gauges: the model's first batch,
        # ~1.5-2 min after a pod starts; Germany 2026-11-06): no verdict for that country in that reading, never a FAIL.
        nan = float("nan")
        fake = Fake([pods({cc: NEW for cc in ALL})],
                    {"held_listings": [series({**{cc: 0 for cc in ALL}, "de": nan}), series({cc: 0 for cc in ALL})],
                     "take_flips": [series({})]})
        code, out, err = watch(fake, "--every", "5m", "--window", "5m")
        self.assertEqual(0, code, out + err)
        self.assertIn("PASS\theld listings", out)
        self.assertIn("de=not reported yet", err)
        self.assertNotIn("RED", err)

    def test_a_gauge_never_reported_over_the_window_fails(self):
        fake = Fake([pods({cc: NEW for cc in ALL})],
                    {"held_listings": [series({**{cc: 0 for cc in ALL}, "de": float("nan")})], "take_flips": [series({})]})
        code, out, _ = watch(fake, "--every", "5m", "--window", "10m")
        self.assertEqual(1, code)
        self.assertIn("FAIL\theld listings", out)
        self.assertIn("de never reported a value", out)

    def test_one_country_canary_waits_and_judges_that_country_alone(self):
        fake = Fake([pods({**{cc: OLD for cc in ALL}, "es": NEW})],
                    {"held_listings": [series({"es": 3, "de": 5000})], "take_flips": [series({})]})
        code, out, err = watch(fake, "--countries", "es", "--window", "0m")
        self.assertEqual(0, code, out + err)
        self.assertNotIn("de=", out)

    def test_operators_and_a_malformed_checks_file(self):
        self.assertTrue(deploy_watch.Check("x", ">=", 1.0, "fail", "q").ok(1.0))
        self.assertFalse(deploy_watch.Check("x", "<", 1.0, "fail", "q").ok(1.0))
        code, _, err = watch(Fake([pods({})], {}), checks="only\ttwo\n")
        self.assertEqual(2, code)
        self.assertIn("line 1", err)
        code, _, err = watch(Fake([pods({})], {}), checks="n\t~\t1\tfail\tq\n")
        self.assertEqual(2, code)
        self.assertIn("operator", err)

    def test_the_named_identity_gate_checks_parse(self):
        checks = deploy_watch.load_checks("identity-gate")
        self.assertGreaterEqual(len(checks), 4)
        self.assertTrue(any("held_oldest_seconds" in c.promql for c in checks))
        self.assertTrue(any("promoted_total" in c.promql for c in checks))
        self.assertTrue(any("take_flips_total" in c.promql for c in checks))

    def test_the_wrapper_runs_end_to_end_against_a_stub_query(self):
        with tempfile.TemporaryDirectory() as d:
            stub = os.path.join(d, "query")
            answer = {"kube": pods({cc: NEW for cc in ALL}), "other": series({cc: 0 for cc in ALL})}
            with open(stub, "w") as f:
                f.write("#!/bin/sh\ncase \"$1\" in *kube_pod*) cat \"$0.kube\";; *) cat \"$0.other\";; esac\n")
            os.chmod(stub, 0o755)
            for k, v in answer.items():
                with open(f"{stub}.{k}", "w") as f:
                    json.dump({"status": "success", "data": {"resultType": "vector", "result": v}}, f)
            checks = os.path.join(d, "checks.tsv")
            with open(checks, "w") as f:
                f.write(CHECKS)
            done = subprocess.run([os.path.join(HERE, "deploy-watch"), "--image", NEW, "--window", "0m", "--checks", checks],
                                  env={**os.environ, "DEPLOY_WATCH_QUERY": stub}, capture_output=True, text=True, timeout=60)
            self.assertEqual(0, done.returncode, done.stdout + done.stderr)
            self.assertIn("DEPLOY-WATCH: PASS", done.stdout)
            usage = subprocess.run([os.path.join(HERE, "deploy-watch"), "--help"], capture_output=True, text=True)
            self.assertIn("deploy-watch --image", usage.stdout)


if __name__ == "__main__":
    unittest.main()
