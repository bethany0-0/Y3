function [sorted_pop, sorted_fitness ] = StochasticRanking(population, fitness, penalty)

sorted_pop = population;
sorted_fitness = fitness;
Pf = 0.45;
N = size(population);

for i = 1 : N
    
    swapped = false;
    
    for j = 1 : (N-1)
        
        %sample u in U(0,1)
        u = rand(1);
        
        if (penalty(j) == penalty(j+1) == 0 || u<Pf)
            if (fitness(j) > fitness(j+1))

                swap(sorted_pop,j,(j+1)); 
                swap(sorted_fitness,j, (j+1));
                swapped = true;
            end
        else
            if penalty(j) > penalty(j+1)
                 swap(sorted_pop,j, (j+1)); 
                 swap(sorted_fitness, j, (j+1));
                swapped =true;
            end
        end
        
        if swapped == false
            break;
        end
    end
end

function fitness = swap(list,i,j)
% Swap fitness(i) and fitness(j)
% Note: In practice, fitness xhould be passed by reference

val = list(i);
list(i) = list(j);
list(j) = val;

end

end


