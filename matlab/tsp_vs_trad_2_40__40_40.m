clear mode bots tasks optimal_cost trading_cost ratio;

num_teams = 39;
team_rows = 1; % the number of data rows in each team size
mode_rows = team_rows * num_teams; % the number of data rows in each starting mode

% Read the contents...

[mode, bots, tasks, optimal_cost, trading_cost, ratio] = textread ('../results/tsp-vs-trad-2-40--40-40.txt', '%s %d %d %f %f %f');

figure (1);
hold on;

axis ([2 40 0.85 1.0]);
plot (bots, ratio, '-o');
title ('Spread start: Traderbot / Optimal (40 tasks)');
legend ('Optimal/Heuristic ratio');
xlabel ('Robots');
ylabel ('Ratio');
