clear all
x=[-500:1:500];
f = -x.*sin(sqrt(abs(x)));
plot(x, f)
G = max(0,(x.^2-10000));
f_new = f + 0.01*G;
hold on
plot(x, f_new, 'r')
f_new = f + 1*G;
hold
hold on