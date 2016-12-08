function best_constraint= violations( population, matrix_a )

[N,X] = size(population);
[X,Y] = size(matrix_a);
best_constraint = zeros(N,1);

for i = 1:N
F = population(i,:);
    
    
temp2 =  (matrix_a*F')';
best_constraint(i) = sum(((temp2-1).^2)');
 

end

end
