#!/bin/bash

RESULT=replan3b_500_1500.txt

clear

for i in `seq 500 100 1500`
do
	echo "Tasks to execute: " $i
        ( ../obj/expres-main-replan3 10 $i 2>&1 | grep '\[Sol\]' | cut -d ' ' -f 2-7 >> $RESULT ) || (echo ERROR; exit)
        echo " " >> $RESULT
done
