#!/bin/bash

RESULT=replan4_0_1000.txt

clear

for i in `seq 0 100 1000`
do
	echo "Tasks to execute: " $i
        ../obj/expres-main-replan4 10 $i 2>&1 | grep '\[Sol\]' | cut -d ' ' -f 2-7 >> $RESULT
        echo " " >> $RESULT
	grep ERROR $RESULT && echo ERROR && exit
done
