#!/usr/bin/env bash

NUM_RUNS=$1
ALLIES_WIN=0
EMPIRES_WIN=0

for i in $(seq 1 $NUM_RUNS); do
    printf "Run $i\x1b[0G"
    RUST_BACKTRACE=1 ./target/debug/der-des-ders --empires robot --seed $i --allies robot
    case $? in
        255)
            EMPIRES_WIN=$((EMPIRES_WIN+1))
            ;;
        1)
            ALLIES_WIN=$((ALLIES_WIN+1))
            ;;
        *)
            echo "Unknown result"
            exit 1
            ;;
    esac
done

echo "Empires win: $EMPIRES_WIN"
echo "Allies win: $ALLIES_WIN"
