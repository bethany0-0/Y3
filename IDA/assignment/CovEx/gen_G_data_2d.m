clear all;
close all;

N = 500;

sig1 = 100;
sig2 = 100;
sig = [sig1, sig2];

mu = [0, 0];
Sigma =diag(sig);
R = chol(Sigma);

x = repmat(mu,N,1) + randn(N,2)*R;
plot(x(:,1), x(:,2), '.'); 
axis([-30 30 -30 30]);
save data1.dat x -ascii;
print data1.jpg -djpeg;

%%%%%%%%%%%%%%%%%%%%%%%%%%%

sig1 = 100;
sig2 = 30;
sig = [sig1, sig2];

mu = [0, 0];
Sigma =diag(sig);
R = chol(Sigma);

x = repmat(mu,N,1) + randn(N,2)*R;
plot(x(:,1), x(:,2), '.'); 
axis([-30 30 -30 30]);
save data2.dat x -ascii;
print data2.jpg -djpeg;


%%%%%%%%%%%%%%%%%%%%%%%%%%%

sig1 = 100;
sig2 = 1;
sig = [sig1, sig2];

mu = [0, 0];
Sigma =diag(sig);
R = chol(Sigma);

x = repmat(mu,N,1) + randn(N,2)*R;
plot(x(:,1), x(:,2), '.'); 
axis([-30 30 -30 30]);
save data3.dat x -ascii;
print data3.jpg -djpeg;

%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Rotation
v1 = sqrt(2)*[1;1];
v2 = sqrt(2)*[1;-1];
V = [v1, v2];

sig1 = 100;
sig2 = 30;
sig = [sig1, sig2];

mu = [0, 0];
Sigma =diag(sig);
R = chol(Sigma);

x = repmat(mu,N,1) + randn(N,2)*R;
xt = V'*x';
x = xt';

plot(x(:,1), x(:,2), '.'); 
axis([-30 30 -30 30]);
save data4.dat x -ascii;
print data4.jpg -djpeg;


%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Rotation
v1 = sqrt(2)*[1;1];
v2 = sqrt(2)*[1;-1];
V = [v1, v2];

sig1 = 100;
sig2 = 1;
sig = [sig1, sig2];

mu = [0, 0];
Sigma =diag(sig);
R = chol(Sigma);

x = repmat(mu,N,1) + randn(N,2)*R;
xt = V'*x';
x = xt';

plot(x(:,1), x(:,2), '.'); 
axis([-30 30 -30 30]);
save data5.dat x -ascii;
print data5.jpg -djpeg;


%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Rotation
v1 = sqrt(2)*[1;1];
v2 = sqrt(2)*[-1;1];
V = [v1, v2];

sig1 = 100;
sig2 = 30;
sig = [sig1, sig2];

mu = [0, 0];
Sigma =diag(sig);
R = chol(Sigma);

x = repmat(mu,N,1) + randn(N,2)*R;
xt = V'*x';
x = xt';

plot(x(:,1), x(:,2), '.'); 
axis([-30 30 -30 30]);
save data6.dat x -ascii;
print data6.jpg -djpeg;


%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Rotation
v1 = sqrt(2)*[1;1];
v2 = sqrt(2)*[-1;1];
V = [v1, v2];

sig1 = 100;
sig2 = 1;
sig = [sig1, sig2];

mu = [0, 0];
Sigma =diag(sig);
R = chol(Sigma);

x = repmat(mu,N,1) + randn(N,2)*R;
xt = V'*x';
x = xt';

plot(x(:,1), x(:,2), '.'); 
axis([-30 30 -30 30]);
save data7.dat x -ascii;
print data7.jpg -djpeg;





