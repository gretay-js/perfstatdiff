#!/bin/bash

set -e

if [ -z $TASK_CPUID ]; then
    TASK_CPUID=2
fi
if [ -z $TASK_PRIORITY ]; then
    TASK_PRIORITY=-20
fi
if [ -z $PERF_REPEATS ]; then
    PERF_REPEATS=3
fi
if [ -z $PERF_EVENTS ]; then
    PERF_EVENTS="-e task-clock,cycles,instructions,cache-misses,L1-icache-misses"
fi

if [ $# -gt 1 ]; then
    exe1="$1"
    shift
    exe2="$1"
    shift
    args=$@
    echo $exe1
    echo $exe2
    echo $args
else
    cat <<EOF
ERROR: missing executable names
Usage:
  $0 <exe1> <exe2> <arg1> <arg2> ...
EOF
    exit 1
fi



cmd1="taskset -c $TASK_CPUID nice -n $TASK_PRIORITY $exe1 $args"
cmd2="taskset -c $TASK_CPUID nice -n $TASK_PRIORITY $exe2 $args"
mkdir -p csv

set -x
perf stat -o csv/$exe1.csv -x , -r $PERF_REPEATS $PERF_EVENTS $cmd1
perf stat -o csv/$exe2.csv -x , -r $PERF_REPEATS $PERF_EVENTS $cmd2
cd csv
perfstatdiff.exe $exe1.csv $exe2.csv -o out.csv
set +x

set +e

