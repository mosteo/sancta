% Read and plot two box plots about the three replanning methods:
% Greedy, greedy + tsp, tsp

% In this run, there are 10 known tasks at each moment.
% There are 100 total tasks to execute (including previous 10).

x=textread('/home/jano/prog/expres/results/replan-10-100.txt');
figure(1)
subplot(1,2,1)
boxplot(x(:,1:3));
subplot(1,2,2)
boxplot(x(:,4:6));
