#!/bin/bash

RESULT=replan5_0_1000.txt

clear

for i in `seq 100 100 1500`
do
	echo "Tasks to execute: " $i
        ../obj/expres-main-replan5 10 $i 2>&1 | grep '\[Sol\]' | cut -d ' ' -f 2-7 >> $RESULT
        echo " " >> $RESULT
	grep ERROR $RESULT && echo ERROR && exit
done
