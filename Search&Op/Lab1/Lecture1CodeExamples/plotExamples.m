close all
%% Construct a sphere model
k = 5;
n = 2^k-1;
[x,y,z] = sphere(n);
c = hadamard(2^k);


%% Plot 3D lines
plot3(x,y,z);
axis equal

%% Plot surface
figure(2)
surf(x,y,z,c);
colormap([1  1  0; 0  1  1])
axis equal

%% Plot surface with shading
figure(3)
surf(x,y,z,c);
shading interp;
colormap([1  1  0; 0  1  1])
axis equal


%% Plot 3D mesh plot
figure(4)
mesh(x,y,z);
axis equal

%% Plot contour 
figure(5)
contourf(x,y,z);
