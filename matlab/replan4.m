% Read and plot two box plots about the three replanning methods:
% Greedy, greedy + tsp, tsp

% In this run, there are 10 known tasks at each moment.
% There are 100 total tasks to execute (including previous 10).

f=fopen('/home/jano/prog/expres/results/replan4_0_1000.txt');

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

    w = [w, cell2mat(a(:,4:6))];
    x = [x, cell2mat(a(:,4))];
    y = [y, cell2mat(a(:,5))];
    z = [z, cell2mat(a(:,6))];
end 

ax=[0:100:1000];

figure(1);
hold;
%boxplot(w(:,4:size(w,2)), 'whis', 10000);
%errorbar(mean(w),std(w))
errorbar(ax,mean(x),std(x), 'r')
errorbar(ax + 10,mean(y),std(y), '-.g')
errorbar(ax + 20,mean(z),std(z), '--b')
legend('Greedy','Greedy+TSP','TSP')
title('Ratio to best, growing&moving cloud (all tasks completed)')
xlabel('# Tasks')
ylabel('Ratio')
