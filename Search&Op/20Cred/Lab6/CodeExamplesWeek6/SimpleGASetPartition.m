function [total_cost,best_ind] = SimpleGASetPartition(matrix_a, column_cost, MaxIter)

%Input Arguments:
%   fname       - the name of the evaluation .m function
%   NDim        - dimension of the evalation function
%   MaxIter     - maximum iteration

% Simple GA for Set Partitioning (Air Crew Scheduling) problem
% Copyright (C) 2014-2016 Shan He, the University of Birmingham

%% Parameters
column_size = size(matrix_a,2);
num_bit = column_size;
% Let's say we want 100 individuals
PopSize = 200;
max_iter = MaxIter;
termination_flag = false;
best_fitness = [];
avg_fitness = [];
t = 0;

%% Generate initial solution
genotypes = rand(PopSize,num_bit)>0.5;
fitness = [];

%% Evaluate phenotypes


fitness = cal_fitness(genotypes, matrix_a, column_cost);

[fitness, sorted_idx] = sort(fitness);
genotypes = genotypes(sorted_idx,:);


while termination_flag == false
    
    
    %% selection
    num_parent = PopSize * 0.3;
    parents = genotypes(1:num_parent,:);
    offerspring = parents;
    
    %% apply crossover
    % Randomly select two individuals as parents
    idx  = randi([1 num_parent],2, 1);
    % Randomly select a bit as cross point
    cross_point =  randi([1 num_bit]);
    % Swap the bits beyond the cross point
    offerspring(idx(1,:), cross_point+1:end) = parents(idx(2,:), cross_point+1:end);
    offerspring(idx(2,:), cross_point+1:end) = parents(idx(1,:), cross_point+1:end);
    
    %% apply mutation
    for j=1:num_parent
        % Randomly select a bit
        mutation_bit = randi([1 num_bit]);
        % flip it
        offerspring(j, mutation_bit) = ~parents(j, mutation_bit);
    end
    
    %% Evaluation fitness
    genotypes = [genotypes; offerspring];   
    fitness = cal_fitness(genotypes, matrix_a, column_cost);
  
    %% Replace the worst individuals
    [fitness, sorted_idx] = sort(fitness);
    genotypes = genotypes(sorted_idx,:);
    genotypes = genotypes(1:PopSize,:);
    
    
    
    %% Results statistics
    best_fitness = [best_fitness fitness(1)];
    avg_fitness = [avg_fitness mean(fitness)];
    
    plot(avg_fitness(1:t),'r--');xlabel('iteration'); ylabel('Average f(x)');
    text(0.5,0.95,['Best = ', num2str(fitness(1))],'Units','normalized');
    drawnow;
    
    %% Terminate?
    t=t+1;
    if(t>max_iter)
        termination_flag = true;
    end
    
    
    
end

best_ind = genotypes(1,:)
[fitness, total_cost, total_cons_vio]  = cal_fitness(best_ind, matrix_a, column_cost);
disp(['The minimum cost found by the GA is: ', num2str(total_cost)]);
disp(['The sum of volations of the constraints is: ', num2str(total_cons_vio)]);
     
end

function [fitness, total_cost, total_degree_vio]  = cal_fitness(pop, matrix_a, column_cost)
    pop_size = size(pop,1);
    num_constraints = size(matrix_a,1);
    total_cost = zeros(1, pop_size);
    for i=1:pop_size
       x = pop(i,:);
       total_cost(1,i) = sum(x.*column_cost);
    end
    % Using matrix element wise multiplication is much faster than a loop
    % temp1 = pop .* repmat(column_cost, pop_size, 1);
    % temp1 is now a pop_size by column_size matirx 
    % 200 rows as 200 individuals, we sum up the column for each row as
    % the total cost for each individual
    % total_cost = sum(temp1,2)';
    total_degree_vio = zeros(1, pop_size);
    for i=1:pop_size
        x = pop(i,:);
        h  =  (matrix_a*x'-1).^2;
        total_degree_vio(1,i) = sum(h);
    end
    % You can also use matrix manipulation to speed up the constranit
    % violation calculation
    % temp2 =  (matrix_a*pop')';
    % total_cons_vio = sum(((temp2-1).^2)');
    fitness = total_cost + 20000*total_degree_vio; 
end
