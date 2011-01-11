#!/bin/bash
# The special thing here is that we use 20 known tasks instead of 10

RESULT=replan3c_500_1500.txt

clear

for i in `seq 500 100 1500`
do
	echo "Tasks to execute: " $i
        ( ../obj/expres-main-replan3 20 $i 2>&1 | grep '\[Sol\]' | cut -d ' ' -f 2-7 >> $RESULT ) || ( echo ERROR; exit)
        echo " " >> $RESULT
done
