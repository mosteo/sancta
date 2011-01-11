#!/bin/bash

RESULT=replan7closed_1000_1500.txt
EXPRES=/home/jano/prog/expres

clear

for i in `seq 1000 100 1500`
do
	echo "Tasks to execute: " $i
        ( $EXPRES/obj/expres-main-replan7closed 10 $i 2>&1 | grep '\[Sol\]' | cut -d ' ' -f 2-7 >> $RESULT ) || (echo ERROR; exit)
        echo " " >> $RESULT
done
