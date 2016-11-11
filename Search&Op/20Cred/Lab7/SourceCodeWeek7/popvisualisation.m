function  popvisualisation(fname, population, iteration, MaxIter)
%Input Arguments:
%   fname           - the name of the evaluation .m function
%   population      - current population
%   iteration       - current iteration
%   MaxIter         - maximum iteration

PopSize = size(population,1);
Bound=eval(fname);
step = (Bound(:,2)-Bound(:,1))/100;
%% Plot 3D function
[X,Y] = meshgrid(Bound(:,1):step:Bound(:,2));
[xx yy] = size(X);
Z = zeros(size(X));
for ii=1:xx
    for jj=1:yy
        Z(ii,jj) = feval(fname, [X(ii,jj) Y(ii,jj)]);
    end
end
mesh(X,Y,Z)
hold on
plot3(0,0,0,'rx', 'LineWidth',2,'MarkerSize',8)

%% Plot 3D visulisation of the current population and previous population
hold on;
for jj=1:PopSize
    position = population(jj,:);
    z = feval(fname,position);    
    plot3(position(1), position(2), z,'o','LineWidth',2,'MarkerSize',8);

    
end
titlestr = ['Step ', num2str(iteration)];
title(titlestr);
hold off;
%M(iteration) = getframe;

