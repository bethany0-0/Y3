function [fitness idx] = stochastic_ranking_sort(fitness, G_sum)
% Bubble sort
mu = length(fitness);
Pf = .45;
idx = [1:mu];

for i = 1:mu
    swapped = false;
    % Iterate through fitness
    for j = 2:mu
        U = rand(1);
        % Swap elements in wrong order
        % There is no constraint violation or U < Pf
        % Please complete the implementation
        
        if ((fitness(i) == 0)&&(fitness(i) == fitness(i - 1)))||(U <= Pf)
            if (x(i) < x(i - 1))
                fitness = swap(fitness,i,i - 1);
                idx = swap(idx, i, i-1);
                swapped = true;
            end
        end
    end

end

end




function fitness = swap(fitness,i,j)
% Swap fitness(i) and fitness(j)
% Note: In practice, fitness xhould be passed by reference

val = fitness(i);
fitness(i) = fitness(j);
fitness(j) = val;

end

