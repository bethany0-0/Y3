function [fitness, total_profit, total_cons_vio, best_solution] = BGA_template
%% Define your a and c here
% Try the Exercise 1 to see whether you can get better results.  
a = [1 0 1; 0 1 1; 1 0 1; 1 0 1; 0 1 0];
c=[5 10 3];
% Generate 4 bits are enough for our problem: 4 projects
num_bit = 3;

% Parameters
crossover_prob = 0.85;
mutation_prob = 1/num_bit;
num_ind = 10;
max_iter = 100;
num_parents = floor(num_ind * 0.3);

% Generate initial solution
pop = rand(num_ind,num_bit)>0.5;

% Calculate fitness for the initial population
fitness = [];
for ii=1:num_ind
    x = pop(ii, :)
    fitness = [fitness calculatefitness(x, c, a)];
end

% Sort the individuals in the population according to their fitness values
% Note: we are maximising profit (the more profit, the better the
% individual)
[~, sorted_idx] = sort(fitness);
pop = pop(sorted_idx,:);


termination_flag = false;
t=1;
while termination_flag == false
    
    
    %  Select parents from the population based on their fitness using truncation selection
    
    parents = pop(1:num_parents,:);
    offerspring = parents;
    
    
    
    %% apply crossover    
    for j=1:floor(num_parents/2)
        if rand(1) <  crossover_prob
            % Randomly select two individuals from parents
            idx  = randi([1 num_parents],2, 1);
            % Randomly select a bit as cross point
            cross_point =  randi([1 num_bit]);
            % Swap the bits beyond the cross point
            offerspring(idx(1,:), cross_point+1:end) = parents(idx(2,:), cross_point+1:end);
            offerspring(idx(2,:), cross_point+1:end) = parents(idx(1,:), cross_point+1:end);
        end
    end
    
    %% apply mutation
    for j=1:num_parents
        if rand(1) <  mutation_prob
            % Randomly select a bit
            mutation_bit = randi([1 num_bit]);
            % flip it
            offerspring(j, mutation_bit) = ~offerspring(j, mutation_bit);
        end
    end
    
    % Evaluation fitness of the new population (old population + new offspring)
    % Note: my implementation is not efficient
    temp_pop = [pop; offerspring];   
    fitness = [];
    num_ind = length(temp_pop)
    for ii=1:num_ind
        x = temp_pop(ii, :);
        fitness = [fitness calculatefitness(x, c, a)];
    end    
    % Sort the individuals in the population according to their fitness values
    [fitness, sorted_idx] = sort(fitness);
    % Replace the worst individuals, or select the top num_ind individuals from the new population  
    pop = temp_pop(sorted_idx(1:num_ind),:);

    
    % Termination condiction 
    t=t+1;
    if(t>max_iter)
        termination_flag = true;
    end
    
    
    
end
best_solution = pop(1, :);
[fitness, total_cost, total_cons_vio]  = calculatefitness(best_solution, c, a);
end







