function compare(file)
% compare(file)
% Read contents in file and plot the chart.

disp (sprintf ('Reading file: %s\n', file));

clear mode bots tasks optimal_cost trading_cost ratio;

% Read the contents...

[mode, bots, tasks, optimal_cost, trading_cost, ratio] = textread (file, '%s %d %d %f %f %f');

hold on;
plot (tasks(1:10), ratio(1:10), '-o');
plot (tasks(11:20), ratio(11:20), '-og');
plot (tasks(21:30), ratio(21:30), '-or');
legend ('2 robots', '3 robots', '4 robots');