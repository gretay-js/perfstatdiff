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

execs=()
if [ $# -gt 1 ]; then
    while [ $# -ge 1 ]; do
        if [ "$1" = "--" ]; then
            shift
            args=$@
            break
        fi
        exes+=("$1")
        shift
    done
else
    cat <<EOF
ERROR: missing executable names, requires at least 2.
Usage:
  $0 <exe1> <exe2> ... <exeN> -- <arg1> <arg2> ...
EOF
    exit 1
fi

mkdir -p csv
csvs=""
for exe in "${exes[@]}"; do
    cmd="taskset -c $TASK_CPUID nice -n $TASK_PRIORITY $exe $args"
    perfcmd="perf stat -o csv/$exe.csv -x , -r $PERF_REPEATS $PERF_EVENTS $cmd"
    echo $perfcmd
    $perfcmd
    csvs+=("$exe.csv")
done

cd csv
# compare on at once - not working yet.
# perfstatdiff.exe "${csvs[@]}" -o out.csv

len=${#csvs[@]}
baseline=${csvs[1]}
for (( i=2; i<${len}; i++ ));
do
  perfstatdiff.exe $baseline ${csvs[$i]}
done

set +e

