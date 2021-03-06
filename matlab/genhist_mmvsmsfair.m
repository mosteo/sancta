s1=textread('bomb.halftimefair_minmix_lagotail', '%f');
s2=textread('bomb.fair_minsum', '%f');

[x1 y1]=histnz(s1);
[x2 y2]=histnz(s2);

fsize=12.0;

hold;
plot(x1,y1, ':ob')
plot(x2,y2, ':xr')
legend('MinMax','MinSum')
xlabel('Objects found', 'FontSize', fsize)
ylabel(sprintf ('Occurrences (out of %d runs)', length(s1)), 'FontSize', fsize)