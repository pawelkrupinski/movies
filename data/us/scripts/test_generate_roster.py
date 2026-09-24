#!/usr/bin/env python3
"""
Unit test for the metro centroid in generate_roster: the one number in the
generated roster that must come out the same on every machine.

The drift guard (scripts/ci/generated-artefacts-drift.sh) regenerates
UsRosterData.scala on CI and fails on any difference, so a centroid that depends
on HOW the platform adds floats turns into a red build nobody can reproduce.
That happened: Spencer, Iowa's longitude is exactly -94.923165, a rounding
boundary, and a naive float sum (Python <= 3.11, macOS's /usr/bin/python3)
lands just below it while 3.12's compensated sum lands on it, so one machine
wrote -94.92317 and CI -94.92316.

Run: python3 data/us/scripts/test_generate_roster.py
"""
import importlib.util
import itertools
import math
import os
import sys

sys.dont_write_bytecode = True
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
_spec = importlib.util.spec_from_file_location('generate_roster', os.path.join(HERE, 'generate_roster.py'))
gr = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(gr)

# The five venues of the Spencer, Iowa metro, as data/us/venues.json has them.
SPENCER_LAT = ['43.3911923', '43.3241310', '42.7360066', '43.1122839', '43.4332674']
SPENCER_LON = ['-95.1247219', '-95.1499786', '-94.6684079', '-94.6794721', '-94.9932445']


def test_spencer_centroid_is_the_exact_mean_rounded_half_even():
    assert gr.mean_coordinate(SPENCER_LAT) == 43.19938
    assert gr.mean_coordinate(SPENCER_LON) == -94.92316


def test_the_float_paths_really_disagree_on_spencer():
    # The positive control: without it, the test above could pass on a platform
    # where the bug simply never shows. Naive left-to-right addition and a
    # correctly-rounded sum give different five-place answers for these inputs.
    floats = [float(v) for v in SPENCER_LON]
    naive = 0.0
    for f in floats:
        naive += f
    assert round(naive / len(floats), 5) == -94.92317
    assert round(math.fsum(floats) / len(floats), 5) == -94.92316


def test_the_centroid_does_not_depend_on_venue_order():
    # A float sum's answer depends on the order it adds in; the exact one cannot.
    for order in itertools.permutations(SPENCER_LON):
        assert gr.mean_coordinate(order) == -94.92316


def test_an_exact_half_rounds_to_even():
    # Guam's two venues average to exactly 13.504855: half-even keeps the even 6
    # rather than whatever side of the half a binary float happens to fall on.
    assert gr.mean_coordinate(['13.5195000', '13.4902100']) == 13.50486
    assert gr.mean_coordinate(['0.000005', '0.000005']) == 0.0     # 0.000005 → even 0
    assert gr.mean_coordinate(['0.000015', '0.000015']) == 0.00002  # 0.000015 → even 2


def _generate_with(summer, out):
    """Run the whole generator with the builtin `sum` swapped for `summer`."""
    import builtins
    original = builtins.sum
    builtins.sum = summer
    try:
        gr.main(os.path.join(HERE, '..', 'venues.json'), out)
    finally:
        builtins.sum = original
    with open(out, encoding='utf-8') as f:
        return f.read()


def _naive_sum(values, start=0):
    total = start
    for v in values:
        total = total + v
    return total


def _compensated_sum(values, start=0):
    values = list(values)
    if values and all(isinstance(v, float) for v in values):
        return start + math.fsum(values)
    return _naive_sum(values, start)


def test_the_whole_roster_is_identical_under_either_float_sum():
    # The same generator, once adding floats the pre-3.12 way and once the way
    # 3.12+ does (correctly rounded): the roster it writes must not tell them apart.
    import tempfile
    with tempfile.TemporaryDirectory() as tmp:
        naive = _generate_with(_naive_sum, os.path.join(tmp, 'naive.scala'))
        compensated = _generate_with(_compensated_sum, os.path.join(tmp, 'compensated.scala'))
    assert naive == compensated


def main():
    tests = [v for k, v in sorted(globals().items()) if k.startswith('test_')]
    for test in tests:
        test()
        print(f"  ok  {test.__name__}")
    print(f"{len(tests)} passed")


if __name__ == '__main__':
    sys.exit(main())
