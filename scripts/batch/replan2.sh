#!/bin/bash

RESULT=replan_0_1000.txt

clear

for i in `seq 0 50 1000`
do
	echo "Tasks to execute: " $i
        ../obj/expres-main-replan2 10 $i 2>&1 | grep '\[Sol\]' | cut -d ' ' -f 2-7 >> $RESULT
        echo " " >> $RESULT
done
