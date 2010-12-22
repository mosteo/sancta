% Read and plot two box plots about the three replanning methods:
% Greedy, greedy + tsp, tsp

% In this run, there are 10 known tasks at each moment.
% There are 100 total tasks to execute (including previous 10).

f=fopen('/home/jano/prog/expres/results/replan_0_1000.txt');

w = [];
x = [];
y = [];
z = [];
while ~feof(f),
    % Each 100 rows make a sampling
    a = textscan(f, '%f %f %f %f %f %f', 100);
    w = [w, cell2mat(a(:,4:6))];
    x = [x, cell2mat(a(:,4))];
    y = [y, cell2mat(a(:,5))];
    z = [z, cell2mat(a(:,6))];
end 

ax=[0:50:1000];

figure(1);
hold;
%boxplot(w(:,4:size(w,2)), 'whis', 10000);
%errorbar(mean(w),std(w))
errorbar(ax,mean(x),std(x), 'r')
errorbar(ax + 10,mean(y),std(y), 'g')
errorbar(ax + 20,mean(z),std(z), 'b')
legend('Greedy','Greedy+TSP','TSP')
title('Ratio to best with std. dev.')
xlabel('# Tasks')
ylabel('Ratio')

figure(2)
%boxplot(x(:,2:size(x,2)), 'whis', 10000);
errorbar(ax,mean(x),std(x),'r')
legend('Greedy')
title('Ratio to best with std. dev.')
xlabel('# Tasks')
ylabel('Ratio')

figure(3)
%boxplot(y(:,2:size(y,2)), 'whis', 10000);
errorbar(ax,mean(y),std(y),'g')
legend('Greedy+TSP')
title('Ratio to best with std. dev.')
xlabel('# Tasks')
ylabel('Ratio')

figure(4)
%boxplot(z(:,2:size(z,2)), 'whis', 10000);
errorbar(ax,mean(z),std(z),'b')
legend('TSP')
title('Ratio to best with std. dev.')
xlabel('# Tasks')
ylabel('Ratio')
