#!/bin/bash

RESULT=replan3_0_1000.txt

clear

for i in `seq 0 100 1000`
do
	echo "Tasks to execute: " $i
        ( ../obj/expres-main-replan3 10 $i 2>&1 | grep '\[Sol\]' | cut -d ' ' -f 2-7 >> $RESULT ) || (echo 'ERROR' && exit)

        echo " " >> $RESULT
done
