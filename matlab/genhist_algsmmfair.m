s1=textread('bomb.halftimefair_minmix_lago', '%f');
s2=textread('bomb.halftimefair_minmix_lagotail', '%f');
s3=textread('bomb.halftimefair_minmix_trad', '%f');
s4=textread('bomb.halftimefair_minmix_fifo', '%f');

[x1 y1]=histnz(s1);
[x2 y2]=histnz(s2);
[x3 y3]=histnz(s3);
[x4 y4]=histnz(s4);

hold;
plot(x1,y1, ':ob')
plot(x2,y2, ':*r')
plot(x3,y3, ':+', 'color', [0.5 0 0.2])
plot(x4,y4, ':p', 'color', [0 0.5 0])
legend('Lago','Tail', 'Single', 'Fifo')
xlabel('Objects found')
ylabel(sprintf ('Occurrences (out of %d runs)', length(s1)))