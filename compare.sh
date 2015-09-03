#!/bin/bash

# Script to validate the entire ip space for two erl_ip_index implementations
# Parameters are directories.

OLD_IMPL="$1"
NEW_IMPL="$2"

FIRST_IP=0
LAST_IP=$((2**22))
CHUNK=$((2**22))

CURRENT_IP=$FIRST_IP

for (( CURRENT=$FIRST_IP; CURRENT<$LAST_IP; CURRENT+=$CHUNK ))
do
    FINISH=$(($CURRENT+$CHUNK))
    printf "Running for 0x%x to 0x%x\n" $CURRENT $FINISH
    COMMAND="erl -pa ebin -noshell -run erl_ip_index_debug test_range_results iplists_validate.bert blacklisted-ip-ranges.csv results $CURRENT $FINISH -run init stop"
    (cd $OLD_IMPL; $COMMAND) &
    (cd $NEW_IMPL; $COMMAND) &
    wait
    diff $OLD_IMPL/results $NEW_IMPL/results > /dev/null
    if [ $? -ne 0 ]; then
        printf "Results differ for 0x%x to 0x%x, aborting\n" $CURRENT $FINISH
        exit 1
    fi
done

printf "Success! The entire ipspace has been validated.\n"
