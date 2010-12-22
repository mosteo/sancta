clear mode bots tasks optimal_cost trading_cost ratio;

num_teams = 3;
team_rows = 10; % the number of data rows in each team size
mode_rows = team_rows * num_teams; % the number of data rows in each starting mode

% Read the contents...

[mode, bots, tasks, optimal_cost, trading_cost, ratio] = textread ('../results/tsp-vs-trad-2-4--10-100.txt', '%s %d %d %f %f %f');

titles={'Close start', 'Spread start', 'Fartest start', 'Random start'};
% plot_modes={'-o', '--d', ':x'}; % Monocrome modes
plot_modes={'-o', '-dg', '-xr'}; % Color modes

for i=1:4,
   
   delta = (i - 1) * mode_rows;

   figure (i);
   hold on;

   axis([10 100 0.85 1.0]);

   for j=1:num_teams,   
      idx = (j - 1) * team_rows + 1;
      plot (...
         tasks(idx + delta : (idx + team_rows - 1) + delta), ...
         ratio(idx + delta : (idx + team_rows - 1) + delta), ...
         plot_modes{j});
   end

   title (strcat (titles(i), ': Traderbot / Optimal'));
   legend ('2 robots', '3 robots', '4 robots');
   
end