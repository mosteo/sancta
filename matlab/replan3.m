% Read and plot two box plots about the three replanning methods:
% Greedy, greedy + tsp, tsp

% In this run, there are 10 known tasks at each moment.
% There are 100 total tasks to execute (including previous 10).

%f=fopen('/home/jano/prog/expres/results/replan3_0_1000.txt');

%f=fopen('/home/jano/prog/expres/results/replan3b_500_1500.txt');

f=fopen('/home/jano/prog/expres/results/replan3c_500_1500.txt');
% Here we know 20 instead of 10 tasks

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

ax=[500:100:1500];
%ax=[0:100:1000];

figure(1);
hold;
%boxplot(w(:,4:size(w,2)), 'whis', 10000);
%errorbar(mean(w),std(w))
errorbar(ax,mean(x),std(x), 'r')
errorbar(ax + 10,mean(y),std(y), '-.g')
errorbar(ax + 20,mean(z),std(z), '--b')
legend('Greedy','Greedy+TSP','TSP')
title('Ratio to best, moving cloud')
xlabel('# Tasks')
ylabel('Ratio')
return

figure(2)
%boxplot(x(:,2:size(x,2)), 'whis', 10000);
errorbar(ax,mean(x),std(x),'r')
legend('Greedy')
title('Ratio to best, moving cloud')
xlabel('# Tasks')
ylabel('Ratio')

figure(3)
%boxplot(y(:,2:size(y,2)), 'whis', 10000);
errorbar(ax,mean(y),std(y),'g')
legend('Greedy+TSP')
title('Ratio to best, moving cloud')
xlabel('# Tasks')
ylabel('Ratio')

figure(4)
%boxplot(z(:,2:size(z,2)), 'whis', 10000);
errorbar(ax,mean(z),std(z),'b')
legend('TSP')
title('Ratio to best, moving cloud')
xlabel('# Tasks')
ylabel('Ratio')
