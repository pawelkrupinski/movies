#!/usr/bin/env python3
"""Where harvest_kinoprogramm.py caches its responses.

Run: python3 data/germany/scripts/test_harvest_kinoprogramm.py
"""
import importlib.util
import os
import pathlib

HERE = pathlib.Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location("harvest", HERE / "harvest_kinoprogramm.py")
harvest = importlib.util.module_from_spec(spec)
spec.loader.exec_module(harvest)


def test_the_cache_survives_a_session_rather_than_living_in_one_agents_scratchpad():
    # The default was a Claude session's scratchpad, wiped on restart, so "reruns are free" was not.
    assert harvest.cache_dir({}, pathlib.Path("/home/u")) == "/home/u/.cache/kinowo/kp-harvest"
    assert harvest.cache_dir({"XDG_CACHE_HOME": "/c"}, pathlib.Path("/home/u")) == "/c/kinowo/kp-harvest"
    assert harvest.cache_dir({"KP_CACHE_DIR": "/x"}, pathlib.Path("/home/u")) == "/x"
    assert "claude" not in harvest.CACHE_DIR


if __name__ == "__main__":
    for name, test in list(globals().items()):
        if name.startswith("test_"):
            test()
            print(f"ok  {name}")
