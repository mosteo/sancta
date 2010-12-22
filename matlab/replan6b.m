% Read and plot two box plots about the three replanning methods:
% Greedy, greedy + tsp, tsp

% In this run, there are 10 known tasks at each moment.

% Difference with ~b version is that this shows costs instead of ratios.

% This is growing gloud, open-ended

f=fopen('/home/jano/prog/expres/results/replan6_100_1000.txt');

w = [];
x = [];
y = [];
z = [];
while ~feof(f),
    % Each 100 rows make a sampling
    a = textscan(f, '%f %f %f %f %f %f', 100);

    if size(cell2mat(a),1) ~= 100 && size(cell2mat(a),1) ~= 0,
       error(sprintf('%s: %d', 'Wrong number of lines in data file', size(cell2mat(a),1)));
    end

    w = [w, cell2mat(a(:,1:3))];
    x = [x, cell2mat(a(:,1))];
    y = [y, cell2mat(a(:,2))];
    z = [z, cell2mat(a(:,3))];
end 

ax=[100:100:1000];

figure(1);
hold;
%boxplot(w(:,4:size(w,2)), 'whis', 10000);
%errorbar(mean(w),std(w))
errorbar(ax,mean(x),std(x), 'r')
errorbar(ax + 0,mean(y),std(y), '-.g') % Offset is 0 because it makes a wrong looking graph otherwise.
errorbar(ax + 0,mean(z),std(z), '--b')
legend('Greedy','Greedy+TSP','TSP')
title('Ratio to best, growing&moving cloud')
xlabel('# Tasks')
ylabel('Ratio')
