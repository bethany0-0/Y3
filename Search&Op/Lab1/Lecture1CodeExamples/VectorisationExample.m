A = rand(100, 100);
B = rand(100, 100);

tic 
% Non-vectorisation version
C = zeros(100, 100);
for i=1:size(A,1)
    for j=1:size(B,1)
        for k=1:size(A,2)
            C(i,j) = C(i,j) + A(i,k)*B(k,j);
        end
    end
end
nonvec = toc;

tic 
Cvec = A*B;
vec = toc;
ratio = nonvec / vec




         