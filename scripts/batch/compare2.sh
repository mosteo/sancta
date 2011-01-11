#!/bin/bash

clear && ../obj/expres-main-compare2 $1 $2 1 2>&1 | grep '\[Sol\]' | cut -d ' ' -f 2-7 >> result-${1}-${2}.txt
