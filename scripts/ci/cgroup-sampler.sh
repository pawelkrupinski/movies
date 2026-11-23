#!/bin/sh
# Purpose: The job's own memory and disk writes, read from its cgroup
# The name job_metrics.py and the convergence leg run: scripts/ci/cgroup_sampler.py is the tool (its header says what it does).
exec python3 "$(dirname "$0")/cgroup_sampler.py" "$@"
