function  popvisualisation(fname, population, previous_pop, iteration, MaxIter)
Bound=eval(fname);
step = (Bound(:,2)-Bound(:,1))/100;
PopSize = size(population, 1);
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

%% Plot 3D visulisation
hold on;

for jj=1:PopSize
    position = population(jj,:);
    z = feval(fname,position);
    
    pre_pos = previous_pop(jj,:);
    pre_z = feval(fname,pre_pos);
    plot3(pre_pos(1), pre_pos(2), pre_z,'kx', 'LineWidth',2,'MarkerSize',8);
    plot3(position(1), position(2), z,'o','Color',[mod(iteration,2),iteration/MaxIter, mod(iteration+1,2)], 'LineWidth',2,'MarkerSize',8);
    plot3([pre_pos(1),position(1)], [pre_pos(2),position(2)], [pre_z,z], '-.k');
    
end
titlestr = ['Step ', num2str(iteration)];
title(titlestr);
hold off;
%M(iteration) = getframe;
pause
end