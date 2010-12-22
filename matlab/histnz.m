function [x y]=histnz(a)

y=histc(a, (0:100));
x=find(y);
y=nonzeros(y);