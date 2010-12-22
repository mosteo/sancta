runs=10
problem=41t_2r

while (($runs)); do
	$EXPRES/obj/expres-main-planner_anneal_batch_$problem --id Yue --config $EXPRES/config-planner.xml >> results_${problem}.txt
	echo Iteration $runs done.
	runs=$((runs - 1))
done
