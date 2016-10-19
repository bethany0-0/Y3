tic
A = 0;
for i = 1:50000
    A(i) = i;
end
without = toc

clear A
tic
A = zeros(50000,1);      % Preallocate B with the zeros command.
for i = 1:50000
    A(i) = i;
end
with = toc
ratio = without / with
