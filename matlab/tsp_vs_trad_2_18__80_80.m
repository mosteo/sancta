clear mode bots tasks optimal_cost trading_cost ratio;

% Read the contents...

[mode, bots, tasks, optimal_cost, trading_cost, ratio] = textread ('../results/tsp-vs-trad-2-18--80-80.txt', '%s %d %d %f %f %f');

figure (1);
hold on;

axis ([2 18 0.85 1.0]);
plot (bots, ratio, '-o');
title ('Spread start: Traderbot / Optimal (80 tasks)');
legend ('Optimal/Heuristic ratio');
xlabel ('Robots');
ylabel ('Ratio');
